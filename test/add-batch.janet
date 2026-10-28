# -*- mode: janet; -*-
# graphs/test/add-batch:  add-nodes & add-edges across graph types

(import ./helper :prefix "")
(import ../graphs :prefix "")

(print "\n* add-nodes / add-edges on digraph")

(def dg (-> (make-digraph)
            (add-nodes @["a" "b" "c"])
            (add-nodes @["a" "b"]) # add 'duplicate' nodes
            (add-edges @[["a" "b" 5] ["b" "c"] ["a" "c" 2]])))

(cassert-eq "digraph nodes added once"
            (= (digraph-node-count dg) 3) true)
(cassert-eq "digraph edges added"
            (= (digraph-edge-count dg) 3) true)
(cassert-eq "digraph weight 5"
            (= (digraph-edge-weight dg "a" "b") 5) true)
(cassert-eq "digraph weight default 1"
            (= (digraph-edge-weight dg "b" "c") 1) true)
(cassert-eq "add-nodes returns g"
            (= (add-nodes (make-digraph)) false) false)

(print "\n* add-nodes/add-edges on weighted-graph")

(def wg (-> (make-weighted-graph)
            (add-nodes @["x" "y" "z"])
            (add-edges @[["x" "y" 3.5] ["y" "z"]])))

(cassert-eq "weighted nodes"
            (= (weighted-graph-node-count wg) 3) true)
(cassert-eq "weighted weight 3.5"
            (= (get-in (wg :adj) ["x" "y"]) 3.5) true)
(cassert-eq "weighted default 1"
            (= (get-in (wg :adj) ["y" "z"]) 1) true)

(print "\n* add-nodes/add-edges on simple graph")

(def sg (-> (make-graph @{})
            (add-nodes @["a" "b" "c"])
            (add-edges @[["a" "b"] ["b" "c" 99]])))

(cassert-eq "graph nodes"
            (= (length (graph-nodes sg)) 3) true)
(cassert-eq "graph edges"
            (= (/ (reduce + 0 (map (fn [n] (graph-degree sg n))
                                   (graph-nodes sg))) 2) 2) true)
(cassert-eq "graph undirected symmetric"
            (and (get-in (sg :adj) ["a" "b"])
                 (get-in (sg :adj) ["b" "a"])) true)

(print "\n* add-nodes/add-edges on multigraph/multidigraph")

# Three entries [[1 2] [1 2] [2 1]] increment multiplicity to 3.
(def mg (-> (make-multigraph @{})
            (add-nodes @[1 2])
            (add-edges @[[1 2] [1 2] [2 1]])))

(cassert-eq "multigraph edge count 3 (three parallel undirected edges)"
            (= (edge-count mg) 3) true)

(def md (-> (make-multidigraph @{} @{})
            (add-nodes @["p" "q"])
            (add-edges @[["p" "q"] ["q" "p"]])))

(cassert-eq "multidigraph edge count 2"
            (= (edge-count md) 2) true)

(print "\n* empty calls are no-ops")

(cassert-eq "empty add-nodes ok"
            (= (digraph-node-count
                (add-nodes (make-digraph))) 0) true)

(cassert-eq "empty add-edges ok"
            (= (digraph-edge-count
                (add-edges (make-digraph))) 0) true)

(print-results)
