# -*- mode: janet; -*-
# graphs/test/import:  graph import (DOT, GEXF, adjacency list)

(import ./helper :prefix "")
(import ../graphs :prefix "")
(import ../graphs/util :prefix "")
(import ../graphs/import :prefix "")
(import ./data/graphs :prefix "gd-")

(print "\n* import: DOT")

(defn dot-nodes [src]
  (def r (graph-from-dot src))
  (string/join (sort (map string (r :nodes))) ","))

(defn dot-edges [src]
  (def r (graph-from-dot src))
  (def out @[])
  (each u (sort (map string (r :nodes)))
    (def kids (sort (keys (get (get r :adj) u @{}))))
    (each v kids
      (array/push out (string u "->" v))))
  (string/join (sort out) ","))


(cassert-eq "quoted id containing a space stays one node"
            (dot-nodes "digraph G { \"a b\" -> c; }") "a b,c")

(cassert-eq "quoted id containing a space links the right pair"
            (dot-edges "digraph G { \"a b\" -> c; }") "a b->c")

(cassert-eq "// inside a quoted id is text, not a comment"
            (dot-edges "digraph G { \"x//y\" -> b; }") "x//y->b")

(cassert-eq "chained edge yields every pair"
            (dot-edges "digraph G { a -> b -> c; }") "a->b,b->c")

(cassert-eq "chained undirected edge yields every pair"
            (dot-edges "graph G { a -- b -- c; }") "a->b,b->a,b->c,c->b")

(cassert-eq "no space around the operator"
            (dot-edges "digraph G { a->b; }") "a->b")

(cassert-eq "chained edge with a trailing attribute list"
            (dot-edges "digraph G { a -> b -> c [w=1]; }") "a->b,b->c")

# comments
(cassert-eq "line comment" (dot-edges "digraph G { // note\n a -> b; }") "a->b")
(cassert-eq "hash comment" (dot-edges "digraph G { # note\n a -> b; }") "a->b")
(cassert-eq "block comment" (dot-edges "digraph G { /* note */ a -> b; }") "a->b")
(cassert-eq "block comment spanning lines"
            (dot-edges "digraph G { /* one\n two */ a -> b; }") "a->b")
(cassert-eq "comment before the header is ignored"
            (dot-nodes "// leading\ndigraph G { a -> b; }") "a,b")

# attribute lists are parsed and discarded

(cassert-eq "node attribute list discarded"
            (dot-edges "digraph G { a [shape=box]; a -> b; }") "a->b")
(cassert-eq "edge attribute list discarded"
            (dot-edges "digraph G { a -> b [label=\"hi there\"]; }") "a->b")
(cassert-eq "keyword statements discarded"
            (dot-edges "digraph G { node [shape=box]; edge [color=red]; a -> b; }") "a->b")
(cassert-eq "graph-level attribute statement discarded"
            (dot-edges "digraph G { graph [rankdir=LR]; a -> b }") "a->b")
(cassert-eq "several attribute lists on one node"
            (dot-edges "digraph G { a [x=1][y=2]; a -> b; }") "a->b")

# structure

(cassert-eq "plain directed edge" (dot-edges "digraph G { a -> b; }") "a->b")
(cassert-eq "braces on their own lines" (dot-edges "digraph G {\n a -> b;\n}") "a->b")
(cassert-eq "empty graph" (dot-edges "digraph G { }") "")
(cassert-eq "trailing statement without a semicolon"
            (dot-edges "digraph G { a -> b }") "a->b")
(cassert-eq "three statements on one line"
            (dot-edges "digraph G { a -> b; b -> c; c -> a; }") "a->b,b->c,c->a")
(cassert-eq "isolated node declaration"
            (dot-nodes "digraph G { a; a -> b; }") "a,b")
(cassert-eq "numerical identifiers"
            (dot-edges "graph G { 1 -- 2; 2 -- 3 }") "1->2,2->1,2->3,3->2")

# directedness

(def dr (graph-from-dot "digraph G { a -> b; }"))
(cassert-eq "digraph edge is directed"
            (get (get (get dr :adj) "a") "b") 1)

(def gr (graph-from-dot "graph G { a -- b; }"))

(cassert-eq "graph edge is undirected"
            (get (get (get gr :adj) "a") "b") true)
(cassert-eq "node count" (dr :node-count) 2)
(cassert-eq "edge count" (dr :edge-count) 1)


(print "\n* import: GEXF")

(defn gexf-nodes [src]
  (def r (graph-from-gexf src))
  (string/join (sort (map string (r :nodes))) ","))

(cassert-eq "gexf nodes"
            (gexf-nodes (string "<gexf><graph><nodes>"
                                "<node id=\"a\"/><node id=\"b\"/>"
                                "</nodes><edges><edge source=\"a\" target=\"b\"/>"
                                "</edges></graph></gexf>"))
            "a,b")

(print "\n* import: adjacency list")

(def al (graph-from-adjacency-list @[@[1] @[0 2] @[1]]))
(cassert-eq "adjacency list node count" (al :node-count) 3)
(cassert-eq "adjacency list edge count" (al :edge-count) 2)


# GEXF export -> import round trip

(print "\n* import: GEXF round trip")

(def tricky (make-digraph))
(each nm ["plain" "a&b" "x<y" "per%cent" "under_score" "dash-x"]
  (digraph-add-node tricky nm))

(digraph-add-edge tricky "a&b" "x<y" 1)
(digraph-add-edge tricky "plain" "per%cent" 2)

(def back (graph-from-gexf (to-gexf tricky)))

(cassert-eq "gexf round trip preserves every node name"
            (string/join (sort (map string (back :nodes))) ",")
            (string/join (sort @["plain" "a&b" "x<y" "per%cent" "under_score" "dash-x"]) ","))
(cassert-eq "gexf round trip preserves the edge count"
            (back :edge-count) 2)

(cassert-eq "xml-escape handles the five predefined entities"
            (xml-escape "a&b<c>d\"e'f") "a&amp;b&lt;c&gt;d&quot;e&apos;f")
(cassert-eq "xml-unescape is the inverse of xml-escape"
            (xml-unescape (xml-escape "a&b<c>d\"e'f")) "a&b<c>d\"e'f")
(cassert-eq "xml-unescape decodes numeric references"
            (xml-unescape "plain&#45;&gt;x") "plain->x")
(cassert-eq "xml-unescape leaves a bare ampersand alone"
            (xml-unescape "a & b") "a & b")
(cassert-eq "xml-unescape leaves an unknown entity alone"
            (xml-unescape "&notanentity;") "&notanentity;")


# DOT round trip

(print "\n* import: DOT round trip")

(def dsrc (make-digraph))
(each nm ["plain" "a&b" "x<y" "q\"z" "per%cent"] (digraph-add-node dsrc nm))

(digraph-add-edge dsrc "plain" "a&b" 1)
(digraph-add-edge dsrc "b" "c" 2)
(digraph-add-edge dsrc "x<y" "q\"z" 1)
(digraph-add-node dsrc "b")
(digraph-add-node dsrc "c")

(def dback (graph-from-dot (to-dot dsrc)))
(cassert-eq "dot round trip preserves every node name"
            (string/join (sort (map string (dback :nodes))) ",")
            (string/join (sort @["plain" "a&b" "x<y" "q\"z" "per%cent" "b" "c"]) ","))
(cassert-eq "dot round trip does not turn graph attributes into nodes"
            (dot-nodes (to-dot dsrc))
            (string/join (sort @["plain" "a&b" "x<y" "q\"z" "per%cent" "b" "c"]) ","))
(cassert-eq "dot round trip preserves the edge count"
            (dback :edge-count) 3)
(cassert-eq "dot round trip keeps edges directed"
            (get (get (dback :adj) "plain") "a&b") 1)


# House of Graphs

(print "\n* import: House of Graphs")

(def hog-text
  (string
   "{\"name\":\"C4\","
   "\"adjacencyList\":[[1,2],[0,2],[0,1],[]],"
   "\"adjacencyMatrix\":[[0,1,1,0],[1,0,1,0],[1,1,0,0],[0,0,0,0]]}"))

(def hog-u (graph-from-hog-json hog-text))
(cassert-eq "hog: node count from adjacencyList"
            (hog-u :node-count) 4)
(cassert-eq "hog: undirected halves the symmetric edge count"
            (hog-u :edge-count) 3)
(cassert-eq "hog: undirected edges are stored as true"
            (get (get (hog-u :adj) "0") "1") true)

(def hog-d (graph-from-hog-json hog-text :directed true))
(cassert-eq "hog: directed keeps every listed edge"
            (hog-d :edge-count) 6)
(cassert-eq "hog: directed edges are stored as a unit weight"
            (get (get (hog-d :adj) "0") "1") 1)

(def hog-lab (graph-from-hog-json
              hog-text :labels @["a" "b" "c" "d"] :directed true))

(cassert-eq "hog: labels are attached in order"
            (string/join (sort (map string (hog-lab :nodes))) ",")
            "a,b,c,d")

(cassert-eq "hog: :matrix returns the decoded matrix"
            (length (graph-from-hog-json hog-text :matrix true))4)

(cassert-err "hog: a response with no adjacencyList is rejected"
             (graph-from-hog-json "{\"name\":\"x\"}"))

(cassert-err "hog: malformed JSON is reported, not swallowed"
             (graph-from-hog-json "{not json"))

(def hog-w (graph-from-hog-json
            (string "{\"adjacencyList\":[[[1,4]],[[0,9]],[],[]]}")
            :weighted true))

(cassert-eq "hog: a weighted neighbour pair keeps its weight"
            (edge-weight hog-w "0" "1") 9.0)

(cassert-eq "hog: a plain neighbour index reads as weight 1"
            (edge-weight (graph-from-hog-json hog-text :weighted true) "0" "1") 1.0)

(def hog-par (string "{\"adjacencyList\":[[1,1],[0,0],[],[]]}"))
(cassert-eq "hog: a repeated neighbour collapses by default"
            (edge-count (graph-from-hog-json hog-par)) 1)

(cassert-eq "hog: :multigraph keeps both parallel edges"
            (edge-count (graph-from-hog-json hog-par :multigraph true)) 2)


(def hog-m (graph-from-hog-json hog-text :matrix true))
(cassert-set-eq "hog: :matrix returns the matrix as written, not its transpose"
                (string/join (map string (hog-m 0)) ",")
                "0,1,1,0")

(cassert-eq "hog: :matrix is square"
            (length (map (fn [row] (length row)) hog-m)) 4)

# DOT weights

(print "\n* import: DOT weights")

(def wsrc "digraph G { \"a\" -> \"b\" [weight=\"2\"]; \"b\" -> \"a\" [weight=\"5\"]; \"c\" -> \"a\"; }")

(def w-un (graph-from-dot wsrc))
(cassert-eq "dot weight: unweighted import still sees every edge"
            (w-un :edge-count) 3)
(cassert-eq "dot weight: unweighted import gives a unit weight"
            (get (get (w-un :adj) "a") "b") 1)

(def w-wt (graph-from-dot wsrc :weighted true))
(cassert-eq "dot weight: edge count is unchanged by :weighted" (w-wt :edge-count) 3)
(cassert-eq "dot weight: reads weight from the attribute list"
            (get (get (w-wt :adj) "a") "b") 2)
(cassert-eq "dot weight: a second edge keeps its own weight"
            (get (get (w-wt :adj) "b") "a") 5)
(cassert-eq "dot weight: an edge with no weight defaults to 1"
            (get (get (w-wt :adj) "c") "a") 1)

(def w-ex (graph-from-dot wsrc :weighted false))
(cassert-eq "dot weight: :weighted false is the default"
            (get (get (w-ex :adj) "a") "b") 1)


# GEXF directedness

(print "\n* import: GEXF directedness")

(defn gexf-doc [edgetype]
  (string
   "<gexf><graph mode=\"static\""
   (if edgetype (string " defaultedgetype=\"" edgetype "\"") "")
   "><nodes><node id=\"a\"/><node id=\"b\"/></nodes>"
   "<edges><edge id=\"0\" source=\"a\" target=\"b\" weight=\"2\"/>"
   "</edges></graph></gexf>"))

(defn gexf-is-undirected [g]
  (not= nil (get (get (g :adj) "b") "a")))

(cassert-eq "gexf: a digraph stays directed"
            (gexf-is-undirected (graph-from-gexf (gexf-doc "directed"))) false)
(cassert-eq "gexf: an undirected file stays undirected"
            (gexf-is-undirected (graph-from-gexf (gexf-doc "undirected"))) true)
(cassert-eq "gexf: no defaultedgetype falls back to undirected"
            (gexf-is-undirected (graph-from-gexf (gexf-doc nil))) true)
(cassert-eq "gexf: :directed false overrides the file"
            (gexf-is-undirected (graph-from-gexf (gexf-doc "directed") :directed false)) true)
(cassert-eq "gexf: :directed true overrides the file"
            (gexf-is-undirected (graph-from-gexf (gexf-doc "undirected") :directed true)) false)
(cassert-eq "gexf: the weight survives either way"
            (get (get (get (graph-from-gexf (gexf-doc "directed")) :adj) "a") "b") 2)

# export -> import round trip
(def grt (make-digraph))
(add-nodes grt "a" "b" "c")
(digraph-add-edge grt "a" "b" 3)
(digraph-add-edge grt "a" "c" 1)
(digraph-add-edge grt "b" "a" 3)

(def gback (graph-from-gexf (to-gexf grt)))

(cassert-eq "gexf round trip: node count"
            (gback :node-count) 3)
(cassert-eq "gexf round trip: all three edges survive"
            (gback :edge-count) 3)
(cassert-eq "gexf round trip: a->b keeps weight 3"
            (get (get (gback :adj) "a") "b") 3)
(cassert-eq "gexf round trip: b->a survives as its own edge"
            (get (get (gback :adj) "b") "a") 3)
(cassert-eq "gexf round trip: a->c keeps weight 1"
            (get (get (gback :adj) "a") "c") 1)


# GEXF parallel edges

(print "\n* import: GEXF parallel edges")

(defn par-g-is-node [g n]
  (some |(= $ n) (g :nodes)))

(defn gexf-par [edgetype pairs]
  (var body @"")
  (var i 0)
  (each p pairs
    (buffer/push-string body
                        (string "<edge id=\"" i "\" source=\"" (get p 0) "\" target=\""
                                (get p 1) "\" weight=\"" (get p 2) "\"/>"))
    (++ i))
  (string
   "<gexf><graph mode=\"static\" defaultedgetype=\"" edgetype "\"><nodes>"
   "<node id=\"a\"/><node id=\"b\"/><node id=\"z\"/></nodes><edges>"
   (string body) "</edges></graph></gexf>"))

(def par-g (graph-from-gexf
            (gexf-par "directed" [["a" "b" "1"] ["a" "b" "1"] ["a" "b" "1"]])))

(cassert-eq "gexf parallel: three parallel edges all survive"
            (par-g :edge-count) 3)
(cassert-eq "gexf parallel: node count is unaffected"
            (par-g :node-count) 3)
(cassert-eq "gexf parallel: an isolated node is kept"
            (par-g-is-node par-g "z") true)
(cassert-eq "gexf parallel: re-exporting writes three edges"
            (length (string/find-all "<edge " (to-gexf par-g))) 3)

(def par-u (graph-from-gexf
            (gexf-par "undirected" [["a" "b" "1"] ["a" "b" "1"]])))

(cassert-eq "gexf parallel: undirected repeat is two edges"
            (par-u :edge-count) 2)

# a simple file must stay on the weighted matrix path
(def simple-g (graph-from-gexf
               (gexf-par "directed" [["a" "b" "3"] ["a" "z" "1"]])))

(cassert-eq "gexf simple: a simple file keeps its edge count"
            (simple-g :edge-count) 2)
(cassert-eq "gexf simple: a simple file keeps its weights"
            (get (get (simple-g :adj) "a") "b") 3)

# both directions of an undirected pair is one edge, not two parallel ones
(def sym-g (graph-from-gexf
            (gexf-par "undirected" [["a" "b" "2"] ["b" "a" "2"]])))

(cassert-eq "gexf parallel: both directions of an undirected pair is one edge"
            (sym-g :edge-count) 1)

(cassert-eq "gexf parallel: :multigraph true forces it on a simple file"
            ((graph-from-gexf
              (gexf-par "directed" [["a" "b" "1"]]) :multigraph true) :edge-count) 1)

(cassert-eq "gexf parallel: :multigraph false collapses a parallel file"
            ((graph-from-gexf
              (gexf-par "directed" [["a" "b" "1"] ["a" "b" "1"]])
              :multigraph false) :edge-count) 1)


# GEXF attvalues as node-data

(print "\n* import: GEXF attvalues")

(def av-doc (string
             "<gexf><graph mode=\"static\" defaultedgetype=\"directed\"><nodes>"
             "<node id=\"a\"><attvalues><attvalue for=\"color\" value=\"red\"/>"
             "<attvalue for=\"size\" value=\"7\"/></attvalues></node>"
             "<node id=\"b\"/>"
             "<node id=\"z\"><attvalues><attvalue for=\"group\" value=\"core\"/>"
             "</attvalues></node>"
             "</nodes><edges><edge id=\"0\" source=\"a\" target=\"b\" weight=\"3\"/>"
             "</edges></graph></gexf>"))

(def av-g (graph-from-gexf av-doc))
(cassert-eq "gexf attvalues: node count is unaffected"
            (av-g :node-count) 3)
(cassert-eq "gexf attvalues: edge count is unaffected"
            (av-g :edge-count) 1)
(cassert-eq "gexf attvalues: parsed into node-data"
            (get (get (av-g :node-data) "a") "color") "red")
(cassert-eq "gexf attvalues: a second attribute is kept"
            (get (get (av-g :node-data) "a") "size") "7")
(cassert-eq "gexf attvalues: readable through the node-data accessor"
            (get (node-data av-g "z") "group") "core")
(cassert-eq "gexf attvalues: a node with none keeps the identity mapping"
            (get (av-g :node-data) "b") "b")
(cassert-eq "gexf attvalues: weights still import"
            (get (get (av-g :adj) "a") "b") 3)

# '<nodes>' is a prefix match for '<node' but must not be read as a node
(cassert-eq "gexf attvalues: the <nodes> container is not a node"
            (not (some |(= "nodes" $) (av-g :nodes))) true)

# the exporter's half: attvalues written and read back
(def av-g2 (make-digraph))
(add-nodes av-g2 "a" "b")
(add-edge av-g2 "a" "b" 1)

(def av-xml (to-gexf av-g2 :node-data @{"a" @{"color" "red" "size" "7"}}))

(cassert-eq "gexf attvalues: exporter writes an <attvalue> per field"
            (length (string/find-all "<attvalue " av-xml)) 2)

(def av-back (get (graph-from-gexf av-xml) :node-data))
(cassert-eq "gexf attvalues: export -> import keeps them"
            (length (keys (get av-back "a"))) 2)
(cassert-eq "gexf attvalues: export -> import keeps the values"
            (get (get av-back "a") "color") "red")
(cassert-eq "gexf attvalues: no attvalues written without the option"
            (= nil (string/find "attvalue" (to-gexf av-g2))) true)
(cassert-eq "gexf attvalues: the identity node-data is not emitted as junk"
            (= nil (string/find "attvalue" (to-gexf av-g2 :node-data @{}))) true)
(cassert-eq "gexf attvalues: edge-data writes an <attvalues> child"
            (not= nil (string/find "<attvalue for=\"class\""
                                   (to-gexf av-g2 :edge-data
                                      @{(tuple "a" "b") @{"class" "catalysis"}})))
            true)


#  GEXF edge attvalues into edge-data

(print "\n* import: GEXF edge attvalues")

(def eg (make-digraph))
(add-nodes eg "a" "b" "c")
(add-edges eg ["a" "b" 1] ["a" "c" 2])

(set-edge-data eg "a" "b" @{:class "catalysis" :label "paid"})
(def eg-back (graph-from-gexf (to-gexf eg)))

(cassert-eq "gexf edge attvalues: survives a round trip"
            (get (edge-data eg-back "a" "b") "class") "catalysis")
(cassert-eq "gexf edge attvalues: a second field survives"
            (get (edge-data eg-back "a" "b") "label") "paid")
(cassert-eq "gexf edge attvalues: an unannotated edge stays empty"
            (edge-data eg-back "a" "c") nil)
(cassert-eq "gexf edge attvalues: the reverse direction is untouched"
            (edge-data eg-back "b" "a") nil)

(def eg-u (make-graph))
(add-nodes eg-u "a" "b")
(add-edge eg-u "a" "b")
(set-edge-data eg-u "a" "b" @{:color "red"})

(def eg-u-xml (to-gexf eg-u))
(cassert-eq "gexf edge attvalues: a simple :graph exports its edges at all"
            (length (string/find-all "<edge " eg-u-xml)) 1)

(def eg-u-back (graph-from-gexf eg-u-xml))
(cassert-eq "gexf edge attvalues: undirected edge count survives"
            (eg-u-back :edge-count) 1)
(cassert-eq "gexf edge attvalues: undirected data reads either way"
            (get (edge-data eg-u-back "b" "a") "color") "red")
(cassert-eq "gexf edge attvalues: one canonical key on the way back"
            (length (keys (edge-data-table eg-u-back))) 1)

(cassert-eq "gexf edge attvalues: an explicit empty option suppresses them"
            (= nil (string/find "attvalue"
                                (to-gexf eg :edge-data @{}))) true)


#  GEXF <meta>

(print "\n* import: GEXF meta")

(def meta-g (make-digraph))
(add-node meta-g "a")
(add-edge meta-g "a" "a" 1)
(def meta-xml (to-gexf meta-g :name "My Network"))

(def [meta-back meta-tab] (graph-from-gexf meta-xml :meta true))
(cassert-eq "gexf meta: the graph name comes back"
            (meta-tab :name) "My Network")
(cassert-eq "gexf meta: the graph is still returned first"
            (meta-back :node-count) 1)
(cassert-eq "gexf meta: a self loop survives alongside"
            (meta-back :edge-count) 1)
(cassert-eq "gexf meta: without :meta the call still yields one value"
            (type (graph-from-gexf meta-xml)) :table)

(def [_ m2] (graph-from-gexf (to-gexf meta-g :name "A & B <net>") :meta true))
(cassert-eq "gexf meta: the name is unescaped on the way back"
            (m2 :name) "A & B <net>")

(def [_ m3] (graph-from-gexf
             (string "<gexf><meta>"
                     "<creator>someone</creator><description>named</description>"
                     "<lastmodifieddate>2026-01-02</lastmodifieddate>"
                     "<keywords>a b</keywords></meta>"
                     "<graph defaultedgetype=\"directed\"><nodes><node id=\"x\"/></nodes>"
                     "<edges/></graph></gexf>") :meta true))

(cassert-eq "gexf meta: creator is read"
            (m3 :creator) "someone")
(cassert-eq "gexf meta: lastmodifieddate is read"
            (m3 :lastmodifieddate) "2026-01-02")
(cassert-arr-eq "gexf meta: keywords split on spaces"
                (m3 :keywords) @["a" "b"])
(cassert-eq "gexf meta: name is surfaced from description"
            (m3 :name) "named")

(def [_ m5] (graph-from-gexf
             (string "<gexf><meta lastmodifieddate=\"2020-05-06\"/>"
                     "<graph defaultedgetype=\"directed\"><nodes/><edges/></graph></gexf>")
             :meta true))

(cassert-eq "gexf meta: lastmodifieddate also reads as an attribute"
            (m5 :lastmodifieddate) "2020-05-06")

(cassert-eq "gexf meta: a file with no meta yields an empty table"
            (do (def [_ m4] (graph-from-gexf
                             "<gexf><graph defaultedgetype=\"directed\"><nodes/><edges/></graph></gexf>"
                             :meta true))
                (length (keys m4))) 0)

# adjacency representations

(defn adj-ec [g] (edge-count g))

# a path 0-1-2, three valid ways
(def adj-upper [[0 1 0] [0 0 1] [0 0 0]])
(def adj-lower [[0 0 0] [1 0 0] [0 1 0]])
(def adj-sym [[0 1 0] [1 0 1] [0 1 0]])

(each spec [["upper triangle" adj-upper] ["lower triangle" adj-lower]
            ["symmetric" adj-sym]]
  (cassert-eq (string "adjacency matrix, " (spec 0) ": two edges for a 3-path")
              (adj-ec (graph-from-adjacency-matrix (spec 1))) 2))
(cassert-eq "adjacency matrix, symmetric: no pair counted twice"
            (adj-ec (graph-from-adjacency-matrix adj-sym)) 2)
(cassert-eq "adjacency matrix, directed: arcs, not pairs"
            (adj-ec (graph-from-adjacency-matrix adj-upper :directed true)) 2)
(cassert-eq "adjacency matrix, a symmetric matrix directed gives four arcs"
            (adj-ec (graph-from-adjacency-matrix adj-sym :directed true)) 4)
(cassert-eq "adjacency matrix, labels replace the default indices"
            (string/join
             (map string
                  (nodes
                   (graph-from-adjacency-matrix
                    adj-upper :labels ["a" "b" "c"]))) ",") "a,b,c")
(cassert-eq "adjacency matrix, a zero entry is not an edge"
            (adj-ec (graph-from-adjacency-matrix [[0 0 0] [0 0 0] [0 0 0]])) 0)
(cassert-eq "adjacency matrix, a diagonal entry is a self loop"
            (adj-ec (graph-from-adjacency-matrix [[1 0 0] [0 0 0] [0 0 0]])) 1)
(cassert-eq "adjacency matrix, weighted keeps the value as a weight"
            (edge-weight (graph-from-adjacency-matrix
                          [[0 5 0] [0 0 3] [0 0 0]]
                          :weighted true) "0" "1") 5.0)
(cassert-eq "adjacency matrix, unweighted ignores a nonzero value as a weight"
            (adj-ec (graph-from-adjacency-matrix [[0 5 0] [0 0 3] [0 0 0]])) 2)
(cassert-err "adjacency matrix: a non-square matrix is rejected"
             (graph-from-adjacency-matrix [[0 1] [1 0 0]]))
(cassert-err "adjacency matrix: a label list of the wrong length is rejected"
             (graph-from-adjacency-matrix adj-upper :labels ["a" "b"]))

# the list form, same idea
(cassert-eq "adjacency list: a 3-path has two edges"
            (adj-ec (graph-from-adjacency-list @[@[1] @[0 2] @[1]])) 2)
(cassert-eq "adjacency list: a neighbour of itself is a self loop"
            (adj-ec (graph-from-adjacency-list @[@[0]])) 1)
(cassert-eq "adjacency list: directed keeps the asymmetry"
            (adj-ec (graph-from-adjacency-list @[@[1] @[]] :directed true)) 1)
(cassert-eq "adjacency list: labels replace the default indices"
            (string/join
             (map string (nodes
                          (graph-from-adjacency-list
                           @[@[1] @[0]] :labels ["x" "y"]))) ",") "x,y")
(cassert-err "adjacency list: an index past the end is rejected"
             (graph-from-adjacency-list @[@[5]]))
(cassert-err "adjacency list: a negative index is rejected"
             (graph-from-adjacency-list @[@[-1]]))
(cassert-eq "adjacency list: a weighted neighbour pair keeps its weight"
            (edge-weight (graph-from-adjacency-list
                          @[@[[1 3]] @[]] :weighted true) "0" "1") 3.0)

# :multigraph
(cassert-eq "adjacency list, :multigraph: a repeated neighbour is a parallel edge"
            (edge-count (graph-from-adjacency-list @[@[1 1] @[0]] :multigraph true)) 2)

(cassert-eq "adjacency list, without :multigraph the pair collapses to one edge"
            (edge-count (graph-from-adjacency-list @[@[1 1] @[0]])) 1)

(cassert-eq "adjacency list, :multigraph with :directed: one entry per arc"
            (edge-count (graph-from-adjacency-list
                         @[@[1 1] @[0]] :multigraph true :directed true)) 3)

(cassert-err "adjacency list: :multigraph with :weighted is refused, not silently downgraded"
             (graph-from-adjacency-list @[@[1]] :multigraph true :weighted true))

(cassert-eq "adjacency matrix, :multigraph does not double-count the symmetric pair"
            (edge-count (graph-from-adjacency-matrix adj-sym :multigraph true)) 2)

(cassert-eq "adjacency matrix, without :multigraph the same matrix is unchanged"
            (edge-count (graph-from-adjacency-matrix adj-sym)) 2)

# importer options

(def opt-digraph (gd-digraph-from-pairs @["a" "b" "c"]
                                        [["a" "b"] ["b" "a"] ["a" "c"]]))

(def opt-weighted (gd-weighted-from-pairs @["a" "b" "c"]
                                          [["a" "b" 2.5] ["b" "c" 0.5]]))

(def opt-parallel (gd-multidigraph-from-pairs @["a" "b" "c"]
                                              [["a" "b"] ["a" "b"] ["b" "c"]]))

(def opt-complete (gd-complete 3))

(defn- ec [g] (edge-count g))

# :directed

(cassert-eq "gexf: directedness comes from the file when not overridden"
            (ec (graph-from-gexf (to-gexf opt-digraph))) 3)
(cassert-eq "gexf: :directed false collapses a->b with b->a"
            (ec (graph-from-gexf (to-gexf opt-digraph) :directed false)) 2)
(cassert-eq "gexf: :directed true reads an undirected file as directed"
            (ec (graph-from-gexf (to-gexf opt-complete) :directed true)) 3)
(cassert-eq "dot: directedness comes from the header"
            (ec (graph-from-dot (to-dot opt-digraph))) 3)
(cassert-eq "dot: :directed false collapses a->b with b->a"
            (ec (graph-from-dot (to-dot opt-digraph) :directed false)) 2)
(cassert-eq "dot: :directed true reads an undirected file as directed"
            (ec (graph-from-dot (to-dot opt-complete) :directed true)) 3)

# :weighted

(cassert-eq "gexf: weights survive by default"
            (edge-weight (graph-from-gexf (to-gexf opt-weighted)) "a" "b") 2.5)

(cassert-eq "gexf: :weighted false drops the weight"
            (edge-weight (graph-from-gexf (to-gexf opt-weighted)
                                          :weighted false) "a" "b") nil)

(cassert-eq "dot: without :weighted the graph defaults to unweighted"
            (edge-weight (graph-from-dot (to-dot opt-weighted)) "a" "b") nil)

(cassert-eq "dot: weight needs :weighted, since the file cannot signal it"
            (edge-weight (graph-from-dot (to-dot opt-weighted)
                                         :weighted true) "a" "b") 2.5)


# :multigraph

(each [name importer exporter] [["gexf" graph-from-gexf to-gexf]
                                ["dot" graph-from-dot to-dot]]

  (cassert-eq (string name ": a repeated pair is detected as parallel")
              (ec (importer (exporter opt-parallel))) 3)

  (cassert-eq (string name ": :multigraph false collapses on request")
              (ec (importer (exporter opt-parallel) :multigraph false)) 2)

  (cassert-eq (string name ": :multigraph true on a simple file is accepted")
              (ec (importer (exporter opt-complete) :multigraph true)) 3))

(cassert-err "gexf: :multigraph with :weighted true is refused, not downgraded"
             (graph-from-gexf (to-gexf opt-parallel) :multigraph true :weighted true))

# The default still works
(cassert-eq "gexf: the default on a parallel file keeps multiplicity, at unit weight"
            (ec (graph-from-gexf (to-gexf opt-parallel))) 3)

# :labels

(cassert-set-eq "gexf: :labels renames the nodes"
                (sort (map string
                           (nodes (graph-from-gexf (to-gexf opt-complete)
                                                   :labels ["p" "q" "r"]))))
                @["p" "q" "r"])

(cassert-set-eq "dot: :labels renames the nodes"
                (sort (map string
                           (nodes (graph-from-dot (to-dot opt-complete)
                                                  :labels ["p" "q" "r"]))))
                @["p" "q" "r"])

# Renaming must move the edges, not just the nodes

(cassert-eq "dot: :labels moves the edges with the names"
            (has-edge? (graph-from-dot (to-dot opt-complete)
                                       :labels ["p" "q" "r"]) "p" "q") true)

(cassert-eq "gexf: :labels moves the edges with the names"
            (has-edge? (graph-from-gexf (to-gexf opt-complete)
                                        :labels ["p" "q" "r"]) "p" "q") true)

(cassert-err "dot: a :labels list of the wrong length is rejected"
             (graph-from-dot (to-dot opt-complete) :labels ["only" "two"]))

(cassert-err "gexf: a :labels list of the wrong length is rejected"
             (graph-from-gexf (to-gexf opt-complete) :labels ["only" "two"]))

#  :meta

(def opt-meta (graph-from-gexf (to-gexf opt-complete) :meta true))
(cassert-set-eq "gexf: :meta returns the document's meta as a second value"
                (sort (map string (keys (last opt-meta))))
                @["name"])

(cassert-eq "gexf: :meta carries the graph name to-gexf wrote"
            (get (last opt-meta) :name) "G")


#  DOT subgraphs

(print "\n* import: DOT subgraphs")

(defn dot-nodes [text]
  (sort (map string (nodes (graph-from-dot text)))))

(defn dot-edges [text]
  (def g (graph-from-dot text))
  (sort (map (fn [e] (string (e 0) ">" (e 1))) (structural-edges g))))

(cassert-set-eq "dot subgraph: the keyword is not imported as a node"
                (dot-nodes "graph G { subgraph s1 { a -> b } b -> c }")
                @["a" "b" "c"])

(cassert-set-eq "dot subgraph: the edges inside are flattened into the outer graph"
                (dot-edges "graph G { subgraph s1 { a -> b } b -> c }")
                @["a>b" "b>c"])

(cassert-set-eq "dot subgraph: the subgraph's own name is not a node either"
                (dot-nodes "graph G { subgraph cluster1 { x -> y } }")
                @["x" "y"])

(cassert-set-eq "dot subgraph: nesting is flattened all the way down"
                (dot-nodes "graph G { subgraph a { subgraph b { p -> q } } }")
                @["p" "q"])

(cassert-set-eq "dot subgraph: an anonymous subgraph declares nothing"
                (dot-nodes "graph G { subgraph { a -> b } }")
                @["a" "b"])

(cassert-set-eq "dot subgraph: a node declared only inside a subgraph survives"
                (dot-nodes "graph G { subgraph s1 { lonely } }")
                @["lonely"])

# strict changes parsing rules, not the node set
(cassert-set-eq "dot strict: strict graph adds no node"
                (dot-nodes "strict graph G { a -> b }")
                @["a" "b"])

(cassert-set-eq "dot strict: strict digraph adds no node"
                (dot-nodes "strict digraph G { a -> b }")
                @["a" "b"])

# a node named "subgraph" has to be quoted
(cassert-set-eq "dot: a quoted node really named subgraph is still a node"
                (dot-nodes "digraph G { \"subgraph\" -> b }")
                @["b" "subgraph"])


#  GEXF weighted multigraph

(print "\n* import: GEXF weighted multigraph")

(def gexf-parallel
  (string "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
          "<gexf version=\"1.2\"><graph defaultedgetype=\"directed\">"
          "<nodes><node id=\"a\"/><node id=\"b\"/></nodes>"
          "<edges><edge id=\"0\" source=\"a\" target=\"b\" weight=\"2\"/>"
          "<edge id=\"1\" source=\"a\" target=\"b\" weight=\"3\"/>"
          "</edges></graph></gexf>"))

(def gexf-simple
  (string "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
          "<gexf version=\"1.2\"><graph defaultedgetype=\"directed\">"
          "<nodes><node id=\"a\"/><node id=\"b\"/></nodes>"
          "<edges><edge id=\"0\" source=\"a\" target=\"b\" weight=\"2\"/>"
          "</edges></graph></gexf>"))

(defn raised-by [f]
  (try (do (f) "") ([e] (string e))))

(cassert-eq "gexf mg: a parallel file with no :weighted is fine"
            (raised-by (fn [] (graph-from-gexf gexf-parallel))) "")

(cassert-eq "gexf mg: asking for both weights and a multigraph is refused"
            (not= nil (string/find ":multigraph with :weighted true is not supported"
                                   (raised-by (fn [] (graph-from-gexf
                                                      gexf-parallel :weighted true)))))
            true)

(cassert-eq "gexf mg: the refusal says how to get an answer"
            (not= nil (string/find "Pass :weighted false"
                                   (raised-by (fn [] (graph-from-gexf
                                                      gexf-parallel :weighted true)))))
            true)

(cassert-eq "gexf mg: :multigraph true with :weighted false is accepted"
            (raised-by (fn [] (graph-from-gexf
                               gexf-parallel :multigraph true :weighted false))) "")

(cassert-eq "gexf mg: :multigraph false collapses, so weights are fine again"
            (raised-by (fn [] (graph-from-gexf
                               gexf-parallel :multigraph false :weighted true))) "")

(cassert-eq "gexf mg: a simple file with :multigraph true and :weighted true is refused too"
            (not= nil (string/find
                       ":multigraph with :weighted true is not supported"
                       (raised-by (fn [] (graph-from-gexf
                                          gexf-simple :multigraph true :weighted true)))))
            true)

# the adjacency importer refuses the same combination
(cassert-eq "gexf mg: the adjacency importer refuses it the same way"
            (not= nil (string/find
                       ":multigraph with :weighted is not supported"
                       (raised-by (fn [] (graph-from-adjacency-list
                                          @[[1] [1]] :multigraph true :weighted true)))))
            true)


#  House of Graphs, live

(def HOG-ID 2760)
(def HOG-URL (string "https://houseofgraphs.org/api/graphs/" HOG-ID))

(def hog-devnull (file/open "/dev/null" :w))

(defn network-available? []
  (try
    (= 0 (os/execute ["curl" "-fsS" "-m" "20" "-o" "/dev/null" HOG-URL]
                     :p {:out hog-devnull}))
    ([_] false)))

(print "\n* import: House of Graphs (live)")

(if (network-available?)
  (do
    (def live (import-hog HOG-ID))
    (cassert-eq "hog live: the node count comes from the response"
                (length (nodes live)) 18)
    (cassert-eq "hog live: undirected halves the symmetric adjacency list"
                (edge-count live) 27)
    (cassert-eq "hog live: the default import is undirected"
                (directed? live) false)
    (cassert-eq "hog live: :directed true overrides it"
                (directed? (import-hog HOG-ID :directed true)) true)
    (cassert-eq "hog live: labels are the ones the file gave"
                (string/join (sort (map string (nodes live))) ",")
                (string/join (sort (map string (range 18))) ","))
    (cassert-eq "hog live: :labels renames every node"
                (string/join
                 (sort
                  (map string
                       (nodes (import-hog
                               HOG-ID
                               :labels (map string (range 100 118)))))) ",")
                (string/join (map string (range 100 118)) ","))
    (cassert-eq "hog live: :matrix returns the matrix, not a graph"
                (length (import-hog HOG-ID :matrix true)) 18)
    (cassert-err "hog live: an id that does not exist raises an error"
                 (import-hog 999999)))
  (print "  - hog live: skipped, houseofgraphs.org is not reachable"))


(print-results)
