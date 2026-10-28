# -*- mode: janet; -*-
# graphs/import/dot: An incomplete DOT graph importer

(import ./adjacency :prefix "a_")
(import ../util :prefix "u_")


(defn- scan
  ```Split into statements. Separators are ';' and bracketed block.
Keeps quoted strings and drops comments.```
  [text]
  (def stmts @[])
  (var buf @"")
  (var in-quote false)
  (var escaped false)
  (var in-block false)
  (var i 0)
  (def n (length text))
  (def only-space? (fn []
                     (var k 0) (var ok true)
                     (while (< k (length buf))
                       (unless (or (= (buf k) 32) (= (buf k) 9))
                         (set ok false) (break))
                       (++ k))
                     ok))
  (while (< i n)
    (def c (get text i))
    (def c2 (if (< (inc i) n) (get text (inc i)) -1))
    (cond
      in-block
      (if (and (= c 42) (= c2 47))
        (do (set in-block false) (set i (+ i 2)))
        (++ i))
      in-quote
      (do (buffer/push-byte buf c)
          (cond
            escaped (set escaped false)
            (= c 92) (set escaped true)
            (= c 34) (set in-quote false))
          (++ i))
      (and (= c 47) (= c2 42))
      (do (set in-block true) (set i (+ i 2)))
      (and (= c 47) (= c2 (get "/" 0)))
      (do (while (and (< i n) (not= (get text i) 10)) (++ i)))
      (and (= c 35) (only-space?))
      (do (while (and (< i n) (not= (get text i) 10)) (++ i)))
      (= c 34)
      (do (set in-quote true) (buffer/push-byte buf c) (++ i))
      (or (= c 59) (= c 123) (= c 125))
      (do (array/push stmts (string buf))
          (set buf @"")
          (when (not= c 59)
            (def one (buffer/new 1))
            (buffer/push-byte one c)
            (array/push stmts (string one)))
          (++ i))
      true
      (do (buffer/push-byte buf c) (++ i))))
  (array/push stmts (string buf))
  stmts)

#  Arrow statements and tokens.
#  Separators are whitespace, ';', ',', '=', ':' and brackets.
#  Edge operators are NOT separators and are matched as units first.
#  String "a->b" should lex the same as "a -> b"

(defn- has-arrow? [toks]
  (some |(or (= $ "->") (= $ "--")) toks))

(defn- separator? [c]
  (or (= c 32) (= c 9) (= c 13) (= c 10)  # space, tab, cr, lf
      (= c 59) (= c 44) (= c 58)))        # ; , : (= is also a token. see below)

(defn- tokenize
  [text]
  (def toks @[])
  (var i 0)
  (def n (length text))
  (def bracket? (fn [at]
                  (def a (if (< at n) (get text at) -1))
                  (or (= a 91) (= a 93))))     # [ ]
  # '=' must be emitted, not skipped. e.g. `ranksep=1.0`
  (def assign? (fn [at] (= (if (< at n) (get text at) -1) 61)))
  (def arrow? (fn [at]
                (def a (if (< at n) (get text at) -1))
                (def b (if (< (+ at 1) n) (get text (+ at 1)) -1))
                (and (= a 45) (or (= b 45) (= b 62)))))
  (while (< i n)
    (def c (get text i))
    (cond
      (separator? c) (++ i)
      (arrow? i)
      (do (array/push toks (string/slice text i (+ i 2))) (set i (+ i 2)))
      (bracket? i)
      (do (array/push toks (string/slice text i (inc i))) (set i (inc i)))
      (assign? i)
      (do (array/push toks "=") (set i (inc i)))
      (= c 34)
      (do
        (def buf @"\"")
        (++ i)
        (var escaped false)
        (while (< i n)
          (def q (get text i))
          (cond
            escaped (do (buffer/push-byte buf q) (set escaped false))
            (= q 92) (do (buffer/push-byte buf q) (set escaped true))
            (= q 34) (do (buffer/push-byte buf q) (break))
            true (buffer/push-byte buf q))
          (++ i))
        (when (< i n) (++ i))
        (array/push toks (string buf)))
      true
      (do
        (def buf @"")
        (while (and (< i n)
                    (not (separator? (get text i)))
                    (not (arrow? i))
                    (not (bracket? i))
                    (not (assign? i))
                    (not= (get text i) 34))
          (buffer/push-byte buf (get text i))
          (++ i))
        (array/push toks (string buf)))))
  toks)

(defn- strip-quotes
  "Strip surrounding quotes from a quoted identifier."
  [tok]
  (def n (length tok))
  (if (and (> n 1) (= (get tok 0) 34) (= (get tok (- n 1)) 34))
    (do
      (def out @"")
      (var i 1)
      (while (< i (- n 1))
        (def c (get tok i))
        (if (and (= c 92) (< i (- n 2)))
          (do (buffer/push-byte out (get tok (+ i 1))) (+= i 2))
          (do (buffer/push-byte out c) (++ i))))
      (string out))
    tok))

#  NOTE: to-dot always writes a weight, normalising an unweighted edge to 1 so
#  the penwidth and colour are well-defined. Therefore the 'weight' attribute
#  can't be used to decide if a graph is weighted. opt-in via :weighted option

(defn- attr-weight
  "Pull the value of `weight` out of a DOT attribute list, or nil."
  [attrs]
  (var i 0)
  (var result nil)
  (while (< i (length attrs))
    (when (and (= (get attrs i) "weight")
               (< (+ i 1) (length attrs))
               (= (get attrs (+ i 1)) "=")
               (< (+ i 2) (length attrs)))
      (when-let [n (scan-number (strip-quotes (get attrs (+ i 2))))]
        (set result n)))
    (+= i 1))
  result)


(defn graph-from-dot
  ```Import a DOT graph.

Supports node and edge declarations, including chained edges (a -> b -> c).
Comments (// # /* */) are stripped outside quoted strings (an identifier
may contain "//" or a space). Subgraphs are flattened and their contents are
treated as belonging to the enclosing graph.

Options
  :directed - read `digraph` as directed (default: taken from the header)
  :weighted - read each edge's `weight` attribute, default to 1 when absent
  :multigraph - true to build a multigraph, decide from file when absent
  :labels - node names, overriding the ones in the file

`:weighted` is opt-in rather than inferred. to-dot always writes a weight,
normalising an unweighted edge to 1 so its penwidth and colour stay
well-defined.

`:multigraph` is consistent with  graph-from-gexf. A DOT file is non-strict
by default, so parallel edges are legal, to-dot writes them for a multigraph.
```
  [text & args]
  (let [opts (table ;args)
        statements (scan text)
        nodes @{}
        edges @[]]
    (var directed? false)
    (var in-body false)
    (each stmt statements
      (def trimmed (string/trim stmt))
      (def toks (tokenize trimmed))
      (def head (if (empty? toks) "" (toks 0)))
      (def has-arrow (has-arrow? toks))
      (def assigns? (do
                       (var in-attrs false)
                       (var found false)
                       (each t toks
                         (cond
                           (= t "[") (set in-attrs true)
                           (= t "]") (set in-attrs false)
                           (and (not in-attrs) (= t "=")) (set found true)))
                       found))
      (cond
        (= trimmed "{") (set in-body true)
        (= trimmed "}") (set in-body false)
        (empty? toks) nil
        (and (= head "digraph") (not in-body)) (set directed? true)
        (and (= head "graph") (not in-body)) (set directed? false)
        # node/edge/graph statements carry attributes
        (or (= head "node") (= head "edge") (= head "graph") (= head "digraph")) nil
        # 'subgraph' and 'strict' are structure, not names.
        (or (= head "subgraph") (= head "strict")) nil
        (or (= head "=") (= head "[") (= head "]") (= head ",")) nil
        assigns? nil
        has-arrow
        (do
          # Emit an edge at each arrow
          (var n-ends 0)
          (var prev nil)
          (var pending-arrow false)
          (var in-attrs false)
          (var attrs @[])
          (var last-edge nil)
          (each tok toks
            (cond
              (= tok "[")
              (do (set attrs @[])
                  (set in-attrs true))
              (= tok "]")
              (do
                (set in-attrs false)
                # the weight of an edge is written after the edge
                (when last-edge
                  (let [w (attr-weight attrs)]
                    (when w (put (get edges last-edge) 2 w))))
                (set attrs @[]))
              in-attrs (do (array/push attrs tok) nil)
              (or (= tok "->") (= tok "--")) (set pending-arrow true)
              (and pending-arrow (not (nil? prev)))
              (let [u (strip-quotes prev)
                    v (strip-quotes tok)]
                (++ n-ends)
                (put nodes u true)
                (put nodes v true)
                (array/push edges @[u v nil])
                (set last-edge (- (length edges) 1))
                (set prev v)
                (set pending-arrow false))
              true (set prev tok)))
          # an endpoint with no edge is still a node declaration
          (when (and (not (nil? prev)) (zero? n-ends))
            (put nodes (strip-quotes prev) true)))
        true
        # a node statement names its first token
        (put nodes (strip-quotes head) true)))
    (def labels (sort (keys nodes)))
    # :labels renames the nodes, applied after label-index
    (def given-labels (get opts :labels))
    (when (and (not (nil? given-labels))
               (not= (length given-labels) (length labels)))
      (error (string "graph-from-dot :labels: expected "
                     (length labels) " names, got " (length given-labels))))
    (def use-labels (if (nil? given-labels) labels given-labels))
    (def label-index (table))
    (each node-name labels (put label-index node-name
                                (length label-index)))
    (def weighted? (get opts :weighted false))
    (def adjacency (seq [_label :in labels] @[]))
    (if weighted?
      # the adjacency converter reads a weighted neighbour as [index weight]
      (each [u v w] edges
        (array/push (get adjacency (get label-index u))
                    [(get label-index v) (if w w 1)]))
      (each [u v _] edges
        (array/push (get adjacency (get label-index u)) (get label-index v))))
    (def want-mg (get opts :multigraph))
    (a_graph-from-adjacency-list
     adjacency
     :directed (if (get opts :directed directed?)
                 (get opts :directed true) false)
     :weighted weighted?
     :multigraph (if (nil? want-mg)
                   (u_duplicate-pairs? edges)
                   want-mg)
     :labels use-labels)))


(defn import-dot
  [path & args]
  (graph-from-dot (slurp path) ;args))
