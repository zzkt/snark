(import graphs :prefix "")

(def k4 (make-graph))
(add-nodes k4 ["a" "b" "c" "d"])
(add-edges k4 [["a" "b"] ["a" "c"] ["a" "d"] ["b" "c"] ["b" "d"] ["c" "d"]])

(printf "spanning trees of K4 (Cayley: 4^2): %p" (count-spanning-trees k4))

(def k3 (make-digraph))
(add-nodes k3 ["0" "1" "2"])
(add-edges k3 [["0" "1" 1] ["0" "2" 1]
               ["1" "0" 1] ["1" "2" 1]
               ["2" "0" 1] ["2" "1" 1]])

(printf "Eulerian circuits of bidirected K3: %p" (count-eulerian-circuits k3))
