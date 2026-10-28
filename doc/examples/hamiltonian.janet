# Examples


(import graphs :prefix "")

(def g (make-graph))
(add-nodes g ["a" "b" "c" "d"])
(add-edges g [["a" "b"] ["b" "c"] ["c" "d"] ["d" "a"]])

(printf "possible cycle? %p" (hamiltonian-possible? g))
(printf "cycle: %p" (hamiltonian-cycle g))
(printf "path: %p" (hamiltonian-path g))
