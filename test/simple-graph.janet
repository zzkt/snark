# -*- mode: janet; -*-
# graph/test/simple-graph: graph construction, mutation, and queries

(import ./helper :prefix "")
(import ../graphs :prefix "")
(import ./data/graphs :prefix "fx-")


(print "* graph: construction")

(def g1 (make-graph @{}))
(cassert-eq "empty graph node-count" (node-count g1) 0)
(cassert-eq "empty graph edge-count" (edge-count g1) 0)

(add-node g1 "a")
(cassert-eq "add-node a"
            (has-node? g1 "a") true)
(cassert-eq "add-node idempotent"
            (do (add-node g1 "a") (node-count g1)) 1)

(add-node g1 "b")
(cassert-eq "add-node b"
            (has-node? g1 "b") true)
(cassert-eq "node-count after 2 adds"
            (node-count g1) 2)

(add-edge g1 "a" "b")
(cassert-eq "add-edge a-b"
            (has-edge? g1 "a" "b") true)
(cassert-eq "add-edge symmetric"
            (has-edge? g1 "b" "a") true)
(cassert-eq "edge-count"
            (edge-count g1) 1)

(add-edge g1 "a" "a")
(cassert-eq "self-loop"
            (has-edge? g1 "a" "a") true)
(cassert-eq "self-loop edge-count"
            (edge-count g1) 2)


# graph queries

(print "\n* graph: queries")

(def g2 (make-graph @{}))
(add-edges g2 @[["a" "b"] ["b" "c"] ["c" "a"] ["c" "c"]])

(cassert-eq "degree a" (graph-degree g2 "a") 2)
(cassert-eq "degree c (self-loop)" (graph-degree g2 "c") 4)
(cassert-eq "neighbours a" (length (neighbours g2 "a")) 2)
(cassert-eq "self-loops has c" (arr-contains (self-loops g2) "c") true)
(cassert-eq "edge-count 4" (edge-count g2) 4)


# graph removal

(print "\n* graph: removal")

(def g3 (make-graph @{}))
(add-edges g3 @[["a" "b"] ["b" "c"] ["c" "a"]])
(remove-edge g3 "a" "b")
(cassert-eq "remove-edge" (has-edge? g3 "a" "b") false)
(cassert-eq "remove-edge count" (edge-count g3) 2)

(remove-node g3 "c")
(cassert-eq "remove-node" (has-node? g3 "c") false)
(cassert-eq "remove-node edge-count" (edge-count g3) 0)

(def g4 (make-graph @{}))
(add-edges g4 @[["a" "b"] ["a" "a"]])
(cassert-eq "self-loop edge-count" (edge-count g4) 2)
(remove-node g4 "a")
(cassert-eq "remove-node with self-loop edge-count" (edge-count g4) 0)
(cassert-eq "remove-node with self-loop nodes" (has-node? g4 "a") false)


# cycles

(print "\n* cycles")

(def gc (make-graph @{}))
(add-edges gc @[["a" "b"] ["b" "c"] ["c" "a"]])
(cassert-eq "triangle has cycle"
            (has-cycle? gc) true)

(def gd (make-graph @{}))
(add-edges gd @[["a" "b"] ["b" "c"]])
(cassert-eq "path no cycle"
            (has-cycle? gd) false)

(def basis (graph-cycle-basis gc))
(cassert-eq "cycle basis non-empty"
            (> (length basis) 0) true)


# shortest-paths

(print "\n* shortest-paths")

(def gs (make-graph @{}))
(add-edges gs @[["a" "b"] ["b" "c"] ["c" "d"]])

(def spl (graph-shortest-path-lengths gs))
(cassert-eq "dist a-a" (get (get spl "a") "a") 0)
(cassert-eq "dist a-c" (get (get spl "a") "c") 2)
(cassert-eq "dist a-d" (get (get spl "a") "d") 3)


# node data

(print "\n* node data")

(def gd2 (make-graph @{}))
(add-nodes gd2 "a" "b")
(cassert-eq "default node data is the node itself"
            (graph-node-data gd2 "a") "a")
(cassert-eq "default node data node b"
            (graph-node-data gd2 "b") "b")

(graph-set-node-data gd2 "a" {:color "red" :weight 3})
(cassert-eq "set node data"
            (graph-node-data gd2 "a") {:color "red" :weight 3})

(cassert-eq "unset node data unchanged"
            (graph-node-data gd2 "b") "b")

(cassert-eq "nodes-table keys equal adj keys"
            (arr-eq (sorted (keys (graph-nodes-table gd2)))
                    (sorted (graph-nodes gd2))) true)

(cassert-eq "node-data for absent node is nil"
            (graph-node-data gd2 "nope") nil)

(set-node-data gd2 "c" "label") # adds node "c"
(cassert-eq "set-node-data adds a node"
            (graph-node-data gd2 "c") "label")
(cassert-eq "added node visible to graph-nodes"
            (arr-contains (graph-nodes gd2) "c") true)

(remove-node gd2 "a")
(cassert-eq "remove-node clears data"
            (graph-node-data gd2 "a") nil)
(cassert-eq "remove-node clears nodes-table"
            (get (graph-nodes-table gd2) "a") nil)


(def hedge (make-graph @{"a" @{"b" 1} "b" @{"a" 1}} @["a" "b"] @{}))
(cassert-eq "has-edge? absent first node -> false"
            (has-edge? hedge "zz" "a") false)
(cassert-eq "has-edge? absent first node, absent second -> false"
            (has-edge? hedge "zz" "yy") false)
(cassert-eq "has-edge? present first, absent second -> false"
            (has-edge? hedge "a" "zz") false)
(cassert-eq "has-edge? present edge -> true"
            (has-edge? hedge "a" "b") true)
(cassert-eq "has-edge? reversed -> true"
            (has-edge? hedge "b" "a") true)

# The node list is filled from the adjacency
(def adjonly (make-graph @{"a" @{"b" 1} "b" @{"a" 1}} @[] @{}))
(cassert-eq "adj-only graph node-count"
            (adjonly :node-count) 2)
(cassert-eq "adj-only graph nodes"
            (node-count adjonly) 2)
(cassert-eq "adj-only graph edge-count"
            (adjonly :edge-count) 1)
(cassert-eq "adj-only has-node? a"
            (has-node? adjonly "a") true)

# an explicit node list is still authoritative
(def withiso (make-graph @{"a" @{"b" 1} "b" @{"a" 1}} @["a" "b" "lonely"] @{}))
(cassert-eq "explicit node list keeps isolated node"
            (node-count withiso) 3)
(cassert-eq "empty adj, explicit nodes"
            (node-count (make-graph @{} @["x" "y"] @{})) 2)

(def allempty (make-graph @{} @[] @{}))
(cassert-eq "empty graph node-count"
            (node-count allempty) 0)
(cassert-eq "empty graph edge-count"
            (edge-count allempty) 0)

(print-results)
