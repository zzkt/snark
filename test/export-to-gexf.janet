# -*- mode: janet; -*-
# graphs/test/export: to-gexf

(import ./helper :prefix "")
(import ../graphs :prefix "")

(defn- has?
  [hay needle]
  (not= nil (string/find needle hay)))

(defn- misses?
  [hay needle]
  (nil? (string/find needle hay)))

(print "\n* export: to-gexf")

(def dg (make-digraph))
(add-edge dg "a" "b" 1)
(add-edge dg "b" "c" 2)

(def wg (make-weighted-graph))
(weighted-graph-add-edge wg "a" "b" 1.5)

(def gxml (to-gexf dg))
(cassert-eq "xml declaration"
            (string/find "<?xml" gxml) 0)
(cassert-eq "gexf root"
            (has? gxml "<gexf") true)
(cassert-eq "directed default"
            (has? gxml "defaultedgetype=\"directed\"") true)
(cassert-eq "edge source"
            (has? gxml "source=\"a\"") true)
(cassert-eq "edge target"
            (has? gxml "target=\"c\"") true)
(cassert-eq "node emitted"
            (has? gxml "id=\"a\"") true)
(cassert-eq "two edges"
            (length (string/find-all "<edge " gxml)) 2)

(cassert-eq "undirected default"
            (has? (to-gexf wg) "defaultedgetype=\"undirected\"") true)

(cassert-eq ":name"
            (has? (to-gexf dg :name "M")
                  "<description>M</description>") true)

(cassert-eq ":precision default writes four decimals"
            (has? (to-gexf wg) "weight=\"1.5000\"") true)

(cassert-eq ":precision 1 writes one decimal"
            (has? (to-gexf wg :precision 1) "weight=\"1.5\"") true)

(print "\n* export: escaping")

(def tricky (make-digraph))
(add-nodes tricky "a&b" "x<y")
(add-edge tricky "a&b" "x<y" 1)

(def gx (to-gexf tricky))
(cassert-eq "gexf escapes ampersand in a node id"
            (has? gx "id=\"a&amp;b\"") true)
(cassert-eq "gexf escapes less-than in a node id"
            (has? gx "id=\"x&lt;y\"") true)
(cassert-eq "gexf emits no raw less-than inside an attribute"
            (misses? gx "id=\"x<y\"") true)
(cassert-eq "gexf escapes ampersand in an edge endpoint"
            (has? gx "source=\"a&amp;b\"") true)

# The graph's own edge-data is read by default
(def anno (make-digraph))
(add-nodes anno "a" "b")
(add-edge anno "a" "b" 1)
(set-node-data anno "a" @{:class "Source"})
(set-edge-data anno "a" "b" @{:class "catalysis"})
(def anno-gx (to-gexf anno))

(cassert-eq "gexf: node attvalues come from the graph by default"
            (has? anno-gx "<attvalue for=\"class\" value=\"Source\"") true)

(cassert-eq "gexf: edge attvalues come from the graph by default"
            (has? anno-gx
                  "<attvalue for=\"class\" value=\"catalysis\"") true)

(cassert-eq "gexf: an unannotated node emits no attvalues"
            (misses? anno-gx "for=\"b\"") true)

(def sl-d (make-digraph))
(add-edge sl-d "a" "a" 5)

(cassert-eq "self loop reaches to-gexf"
            (has? (to-gexf sl-d) "<edge ") true)
(cassert-eq "self loop keeps its weight"
            (has? (to-gexf sl-d)
                  "source=\"a\" target=\"a\" weight=\"5") true)

(def sl-w (make-weighted-graph))
(add-edge sl-w "a" "a" 3)
(cassert-eq "weighted self loop reaches to-gexf"
            (has? (to-gexf sl-w) "weight=\"3") true)

(def mg (make-multigraph @{}))
(add-edges mg ["a" "b"] ["a" "b"] ["a" "b"])

(cassert-eq "multigraph of three parallel edges stays three"
            (length (string/find-all "<edge " (to-gexf mg))) 3)

(cassert-eq "multigraph parallel edges each carry unit weight"
            (length (string/find-all "weight=\"1.0000\"" (to-gexf mg))) 3)

(cassert-eq "multigraph does not report the count as a weight"
            (length (string/find-all "weight=\"3" (to-gexf mg))) 0)

(def plain (make-digraph))
(add-edges plain ["a" "b" 1] ["b" "a" 2] ["a" "c" 3])

(cassert-eq "plain digraph still writes three edges"
            (length (string/find-all "<edge " (to-gexf plain))) 3)

(cassert-eq "plain digraph keeps each distinct weight"
            (length (string/find-all "weight=\"2" (to-gexf plain))) 1)

(cassert-eq "multidigraph: to-gexf emits the edge"
            (do (def md2 (make-multidigraph @{} @{}))
                (add-nodes md2 "a" "b")
                (add-edge md2 "a" "b")
                (length (string/find-all "<edge " (to-gexf md2)))) 1)

(print-results)
