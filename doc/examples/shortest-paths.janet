# Example


(import graphs :prefix "")



# Dijkstra choses a path with lower total weight over a path with fewer edges.
# e.g. =A → B → C= path weight is 3, while the direct =A → C= path weight is 5.


(def wg (make-weighted-graph))
(add-nodes wg ["A" "B" "C"])
(add-edges wg [["A" "B" 1] ["B" "C" 2] ["A" "C" 5]])

(def sp (weighted-graph-shortest-path wg "A" "C"))

(printf "shortest path from A to C: %s (distance %p)"
        (string/join (get sp 1) "->") (get sp 0))

(printf "distance table from A: %s"
        (string/join
         (map |(string/format "%.0f" $0)
              (values (weighted-graph-all-shortest-paths wg "A"))) " "))



# #+RESULTS:
# : shortest path from A to C: A->B->C (distance 3)
# : distance table from A: 1 0 3


# MST and distance-matrix


(def mg (make-weighted-graph))
(add-nodes mg ["A" "B" "C" "D"])
(add-edges mg [["A" "B" 1] ["B" "C" 2] ["C" "D" 3]
               ["A" "C" 6] ["B" "D" 5]])

(def mst (weighted-graph-mst mg))
(printf "MST edge count (n-1): %p" (weighted-graph-edge-count mst))

(def dm (make-distance-matrix @["a" "b" "c"] @[[0 4 8] [4 0 2] [8 2 0]]))
(printf "nearest pair distance: %p" (get (distance-matrix-nearest-pair dm) 2))



# #+RESULTS:
# : MST edge count (n-1): 3
# : nearest pair distance: 2

# All-pairs shortest paths via Floyd–Warshall on a weighted graph


(def wg2 (make-weighted-graph))
(add-nodes wg2 ["A" "B" "C" "D"])
(add-edges wg2 [["A" "B" 1] ["B" "C" 2] ["C" "D" 3]
                ["A" "C" 6] ["B" "D" 5]])

(def dm2 (weighted-graph->distance-matrix wg2))
(printf "distance matrix labels: %s" (string/join (get dm2 :names) ", "))
(printf "A->D distance: %.0f" (get-in dm2 [:matrix 0 3]))
(printf "B->C distance: %.0f" (get-in dm2 [:matrix 1 2]))
