# Examples


(import graphs :prefix "")



# check the output of =count-spanning-trees= against Cayley's formula $|V|^(|V|-2) = 4² = 16$ for =K₄=


(def k4 (make-graph))
(add-nodes k4 ["a" "b" "c" "d"])
(add-edges k4 [["a" "b"] ["a" "c"] ["a" "d"] ["b" "c"] ["b" "d"] ["c" "d"]])

(printf "spanning trees of K4 (Cayley: 4^2): %p" (count-spanning-trees k4))



# #+RESULTS:
# : spanning trees of K4 (Cayley: 4^2): 16

# Eulerian circuits of the bidirected triangle (BEST)


(def k3 (make-digraph))
(add-nodes k3 ["0" "1" "2"])
(add-edges k3 [["0" "1" 1] ["0" "2" 1]
               ["1" "0" 1] ["1" "2" 1]
               ["2" "0" 1] ["2" "1" 1]])

(printf "Eulerian circuits of bidirected K3: %p" (count-eulerian-circuits k3))
