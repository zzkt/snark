# -*- mode: janet; -*-
# graphs/test/weighted-graph: undirected weighted graph

(import ./helper :prefix "")
(import ../graphs :prefix "")

(print "\n* weighted-graph construction")

(def g (make-weighted-graph))
(cassert-eq "node count initially 0"
            (weighted-graph-node-count g) 0)

(add-edges g ["A" "B" 1] ["B" "C" 2] ["A" "C" 5])
(cassert-eq "add-edge auto-creates 3 nodes"
            (weighted-graph-node-count g) 3)
(cassert-eq "edge count 3"
            (weighted-graph-edge-count g) 3)

(print "\n* symmetry and lookups")

(cassert-eq "edge-weight A->B"
            (weighted-graph-edge-weight g "A" "B") 1)
(cassert-eq "edge-weight symmetric B->A"
            (weighted-graph-edge-weight g "B" "A") 1)
(cassert-eq "degree A"
            (weighted-graph-degree g "A") 2)
(cassert-eq "degree unknown node is 0"
            (weighted-graph-degree g "NOPE") 0)
(cassert-eq "degree B"
            (weighted-graph-degree g "B") 2)
(cassert-arr-eq "neighbour-names of A (sorted)"
                (sort (weighted-graph-neighbour-names g "A")) @["B" "C"])
(cassert-arr-eq "neighbour-names of B (sorted)"
                (sort (weighted-graph-neighbour-names g "B")) @["A" "C"])

(print "\n* remove-edge")

(weighted-graph-remove-edge g "A" "B")

(cassert-eq "removed edge A-B weight nil"
            (weighted-graph-edge-weight g "A" "B") nil)
(cassert-eq "removed edge B-A weight nil"
            (weighted-graph-edge-weight g "B" "A") nil)
(cassert-eq "edge count after removal 2"
            (weighted-graph-edge-count g) 2)
(cassert-eq "degree A after removal 1"
            (weighted-graph-degree g "A") 1)

(weighted-graph-add-edge g "A" "B" 1)

(print "\n* shortest path (Dijkstra)")

(def sp (weighted-graph-shortest-path g "A" "C"))
(cassert-eq "shortest-path A->C distance" (sp 0) 3)
(cassert-arr-eq "shortest-path A->C route" (sp 1) @["A" "B" "C"])

(def sp2 (weighted-graph-shortest-path g "A" "B"))
(cassert-eq "shortest-path A->B distance" (sp2 0) 1)

(def allsp (weighted-graph-all-shortest-paths g "A"))
(cassert-eq "all-shortest-paths A (self)" (get allsp "A") 0)
(cassert-eq "all-shortest-paths B dist" (get allsp "B") 1)
(cassert-eq "all-shortest-paths C dist" (get allsp "C") 3)

(print "\n* MST")

(def mst (weighted-graph-mst g))
(cassert-eq "MST edge count = n-1"
            (weighted-graph-edge-count mst) 2)
(cassert-eq "MST connects all nodes"
            (weighted-graph-node-count mst) 3)

# Sum MST edge weights
(def total-mst (sum (seq [n :in (weighted-graph-nodes mst)
                          [m] :in (weighted-graph-neighbours mst n)]
                         (weighted-graph-edge-weight mst n m))))

(cassert-eq "MST picks the two lightest edges (1+2=3)" (/ total-mst 2) 3)


(print "\n* remove node + cycle detection")

(def g2 (make-weighted-graph))
(add-nodes g2 "a" "b" "c")
(weighted-graph-add-edge g2 "a" "b" 1)
(weighted-graph-add-edge g2 "b" "c" 2)

(cassert-eq "weighted-graph: path is not a cycle"
            (has-cycle? g2) false)

(weighted-graph-add-edge g2 "c" "a" 3)
(cassert-eq "weighted-graph: triangle is a cycle"
            (has-cycle? g2) true)

(weighted-graph-remove-edge g2 "c" "a")
(cassert-eq "weighted-graph remove-edge: edge gone"
            (weighted-graph-edge-weight g2 "c" "a") nil)
(cassert-eq "weighted-graph remove-edge: residual a-c gone"
            (weighted-graph-edge-weight g2 "a" "c") nil)
(cassert-eq "weighted-graph: acyclic after remove-edge"
            (has-cycle? g2) false)

(weighted-graph-remove-node g2 "b")
(cassert-eq "weighted-graph remove-node: node count decremented"
            (weighted-graph-node-count g2) 2)
(cassert-eq "weighted-graph remove-node: edge count decremented"
            (weighted-graph-edge-count g2) 0)
(cassert-eq "weighted-graph remove-node: neighbours cleaned"
            (weighted-graph-degree g2 "a") 0)
(cassert-arr-eq "weighted-graph remove-node: removed node gone"
                (weighted-graph-neighbours g2 "b") @[])

(def g3 (make-weighted-graph))
(weighted-graph-add-node g3 "solo")
(weighted-graph-remove-node g3 "solo")

(cassert-eq "weighted-graph remove-node isolated: node gone"
            (weighted-graph-node-count g3) 0)

(weighted-graph-remove-edge g3 "solo" "x")
(cassert-eq "weighted-graph remove missing edge is no-op"
            (weighted-graph-edge-count g3) 0)

(print "\n* weighted-graph node-data")

(def wnd (make-weighted-graph))
(add-nodes wnd "P" "Q")

(cassert-eq "weighted-graph default node data is node itself"
            (weighted-graph-node-data wnd "P") "P")
(cassert-eq "weighted-graph default node data Q"
            (weighted-graph-node-data wnd "Q") "Q")

(weighted-graph-set-node-data wnd "P" {:kind "start"})
(cassert-eq "weighted-graph set node data"
            (weighted-graph-node-data wnd "P") {:kind "start"})
(cassert-eq "weighted-graph unset node data unchanged"
            (weighted-graph-node-data wnd "Q") "Q")

(weighted-graph-set-node-data wnd "R" "extra")
(cassert-eq "weighted-graph add node via set-node-data"
            (weighted-graph-node-data wnd "R") "extra")
(cassert-eq "weighted-graph set-node-data adds node"
            (weighted-graph-node-count wnd) 3)
(cassert-eq "weighted-graph nodes-table has node"
            (in (weighted-graph-nodes-table wnd) "P") {:kind "start"})

(weighted-graph-remove-node wnd "P")
(cassert-eq "weighted-graph remove clears data"
            (weighted-graph-node-data wnd "P") nil)
(cassert-eq "weighted-graph node-data for absent node is nil"
            (weighted-graph-node-data wnd "zz") nil)

# A self-loop occupies one adjacency entry while a normal edge occupies two
(def wloop (make-weighted-graph))
(weighted-graph-add-node wloop "a")
(weighted-graph-add-edge wloop "a" "a" 5)

(cassert-eq "weighted lone self-loop counts as 1 edge"
            (weighted-graph-edge-count wloop) 1)

(weighted-graph-add-node wloop "b")
(weighted-graph-add-edge wloop "a" "b" 7)
(cassert-eq "weighted self-loop plus normal edge counts as 2"
            (weighted-graph-edge-count wloop) 2)

# The :edge-count field is maintained
(def wrem (make-weighted-graph))
(weighted-graph-add-node wrem "a")
(weighted-graph-add-node wrem "b")
(weighted-graph-add-edge wrem "a" "b" 7)
(weighted-graph-add-edge wrem "a" "a" 5)

(cassert-eq "weighted field tracks self-loop plus normal edge"
            (wrem :edge-count) 2)
(cassert-eq "weighted derived count agrees"
            (weighted-graph-edge-count wrem) 2)

(weighted-graph-remove-node wrem "a")
(cassert-eq "weighted field back to zero after remove"
            (wrem :edge-count) 0)
(cassert-eq "weighted derived count zero after remove"
            (weighted-graph-edge-count wrem) 0)

# re-weighting an existing edge must not double count
(def wrew (make-weighted-graph))
(add-nodes wrew "a" "b")
(weighted-graph-add-edge wrew "a" "b" 1)
(weighted-graph-add-edge wrew "a" "b" 9)

(cassert-eq "re-weighting does not add an edge"
            (wrew :edge-count) 1)

(weighted-graph-remove-edge wrew "a" "b")
(cassert-eq "removing the edge drops the count"
            (wrew :edge-count) 0)

(print-results)
