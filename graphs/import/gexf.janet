# -*- mode: janet; -*-
# graphs/import/gexf: An incomplete GEXF XML graph importer.

(import ../util :prefix "")
(import ../multigraph :prefix "mg_")
(import ../record :prefix "pr-")
(import ./adjacency :prefix "a_")

(defn- tags
  ```Every `<name ...>` element in `text`, as a substring.```
  [text name]
  (let [out @[]
        open (string "<" name)]
    (var i 0)
    (def n (length text))
    (while (< i n)
      (def at (string/find open text i))
      (if (nil? at) (break) (do
        (def gt (string/find ">" text at))
        (if (nil? gt)
          (break)
          (array/push out (string/slice text at (inc gt))))
        (set i (inc at)))))
    out))

(defn- attr
  "Value of attribute `name` in a single element, or nil.."
  [tag name]
  (let [key (string name "=\"")
        at (string/find key tag)]
    (if (nil? at)
      nil
      (let [start (+ at (length key))
            close (string/find "\"" tag start)]
        (if (nil? close) nil (xml-unescape (string/slice tag start close)))))))

(defn- elements
  ```Full text of every `<name ...>...</name>` element, children included.

`tags` returns only the opening tag, which is enough for `id`, `source`,
`target` and `weight`attributes. It is not enough for `<attvalues>`, which is
a child element the requires the matching close tag.
Self-closing elements like `<node id="a"/>` are returned whole.
```
  [text name]
  (let [out @[]
        open (string "<" name)
        close (string "</" name ">")
        nlen (length name)]
    (var i 0)
    (def n (length text))
    (while (< i n)
      (def at (string/find open text i))
      (if (nil? at) (break))
      # name must end here. space, tab, newline, `>` or `/`
      (def after (get text (+ at 1 nlen)))
      (if (or (= after 32) (= after 9) (= after 10) (= after 62) (= after 47))
        (do
          (def gt (string/find ">" text at))
          (if (nil? gt)
            (break)
            (do
              (def stop (if (= (get text (- gt 1)) 47)
                           # self-closing: include the `>`
                           (inc gt)
                           (let [ce (string/find close text gt)]
                             (if (nil? ce) n (+ ce (length close))))))
              (array/push out (string/slice text at stop))
              (set i (inc at)))))
        (set i (inc at))))
    out))


(defn- attvalues
  "Parse the `<attvalues>` children of one element into a table."
  [element]
  (def av (tags element "attvalue"))
  (if (empty? av)
    @{}
    (do
      (def out @{})
      (each t av
        (def k (attr t "for"))
        (def v (attr t "value"))
        (when k (put out k (if v v ""))))
      out)))


(defn- element-text
  "Inner text of the first `<name ...>...</name>` element, XML-unescaped."
  [text name]
  (let [open (string "<" name)
        close (string "</" name ">")
        at (string/find open text)]
    (if (nil? at)
      nil
      (do
        (def gt (string/find ">" text at))
        (if (nil? gt)
          nil
          (do
            (def self-closing (= (get text (- gt 1)) 47))
            (if (or self-closing (= (get text (inc gt)) 47))
              nil
              (let [ce (string/find close text gt)]
                (when ce
                  (xml-unescape (string/trim (string/slice text (inc gt) ce))))))))))))


(defn- graph-meta
  "Contents of the document's `<meta>` element as a table."
  [text]
  (def meta-tag (first (elements text "meta")))
  (if (nil? meta-tag)
    @{}
    (do
      (def out @{})
      # Keys are keywords. meta element names are fixed in the GEXF schema.
      (each [k v]
        [[:name (element-text meta-tag "description")]
         [:creator (element-text meta-tag "creator")]
         # GEXF 1.3 makes lastmodifieddate a child element; 1.2 uses an
         # attribute on <meta>, so accept either.
         [:lastmodifieddate (or (element-text meta-tag "lastmodifieddate")
                                (attr meta-tag "lastmodifieddate"))]
         [:version (element-text meta-tag "version")]
         [:keywords (element-text meta-tag "keywords")]]
        (when v
          (put out k (if (= k :keywords)
                        (filter (fn [w] (not= w "")) (string/split " " v))
                        v))))
      out)))

(defn- graph-directedness
  "Directedness declared by the file's `<graph>` element, or nil."
  [text]
  (def graph-tag (first (tags text "graph")))
  (when graph-tag
    (def t (attr graph-tag "defaultedgetype"))
    (when t (string/ascii-lower t))))


(defn- multigraph-from-edges
  "Build a :multigraph or :multidigraph from [source target weight] edges."
  [directed? labels edges]
  (let [g (if directed?
            (mg_make-multidigraph @{})
            (mg_make-multigraph @{}))
        add-node (if directed? mg_multidigraph-add-node mg_multigraph-add-node)
        add-edge (if directed? mg_multidigraph-add-edge mg_multigraph-add-edge)]
    (each l labels (add-node g l))
    (each edge edges (add-edge g (labels (edge 0)) (labels (edge 1))))
    g))


(defn graph-from-gexf
  ```Import node and edge declarations from GEXF XML.

Options;
  :directed   -- override the file's `defaultedgetype`
  :weighted   -- keep edge weights (default true, since GEXF weights are
                 a core field and to-gexf should always write them)
  :labels     -- node names, overriding the labels in the file
  :multigraph -- set to true to always build a multigraph, false to never,
                 or absent (default) to decide from the file
  :meta       -- true to return a table of the document's <meta> element
```
  [text & args]
  (let [opts (table ;args)
        declared (graph-directedness text)
        given (get opts :directed)
        directed? (if (nil? given)
                    (and declared (= declared "directed"))
                    given)
        want-mg (get opts :multigraph)
        labels @[]
        index @{}
        node-data @{}
        edge-data @{}
        edges @[]]
    (each tag (elements text "node")
      (def id (attr tag "id"))
      (when id
        (put index id (length labels))
        (array/push labels id)
        (def data (attvalues tag))
        (unless (empty? data) (put node-data id data))))
    (each tag (elements text "edge")
      (def source (attr tag "source"))
      (def target (attr tag "target"))
      (def weight (attr tag "weight"))
      # note: node index 0 is valid. test against nil explicitly
      (when (and source target
                 (not (nil? (get index source)))
                 (not (nil? (get index target))))
        (array/push edges [(get index source) (get index target)
                          (if weight (scan-number weight) 1)])
        (def data (attvalues tag))
        (when (and (table? data) (not (empty? data)))
          (put edge-data (pr-edge-key directed? source target) data))))
    (def multi? (if (nil? want-mg)
                  (duplicate-pairs? edges)
                  want-mg))
    (when (and multi? (= (get opts :weighted) true))
      (error (string "graph-from-gexf: :multigraph with :weighted true is not "
                     "supported, because a weighted multigraph does not exist. "
                     "Pass :weighted false, or accept unit weights on the "
                     "parallel edges.")))
    (def attach
      (fn [g]
        (each k (keys node-data) (put (g :node-data) k (get node-data k)))
        (each k (keys edge-data) (put (g :edge-data) k (get edge-data k)))
        g))
    # :labels renames the nodes, applied after `index` and `edges`
    (def given-labels (get opts :labels))
    (when (and (not (nil? given-labels))
               (not= (length given-labels) (length labels)))
      (error (string "graph-from-gexf :labels: expected "
                     (length labels) " names, got " (length given-labels))))
    (def use-labels (if (nil? given-labels) labels given-labels))
    (def built
      (if multi?
      (attach (multigraph-from-edges directed? use-labels edges))
      (let [g (do
                (def n (length use-labels))
                (def matrix (seq [i :in (range n)]
                              (seq [j :in (range n)] 0)))
                (each edge edges
                  (put (get matrix (edge 0)) (edge 1) (edge 2)))
                (a_graph-from-adjacency-matrix matrix
                                               :directed directed?
                                               :weighted (get opts :weighted true)
                                               :labels use-labels))]
        (attach g))))
    (if (get opts :meta false)
      [built (graph-meta text)]
      built)))


(defn import-gexf
  [path & args]
  (graph-from-gexf (slurp path) ;args))
