(import graphs :prefix "")

(def c4 (make-graph))
(add-nodes c4 ["a" "b" "c" "d"])
(add-edges c4 [["a" "b"] ["b" "c"] ["c" "d"] ["d" "a"]])

(def col (greedy-colour c4))
(printf "χ(C4) = %p  proper? %p" (chromatic-number c4) (proper-colouring? c4 col))

# ex(7, K4): a K_4-free graph on 7 vertices has at most 16 edges.
(printf "ex(7, K4) = %p" (turan-number 7 3))
(printf "2-partite Turán is bipartite: %p" (= 2 (chromatic-number (make-turan-graph 7 2))))
