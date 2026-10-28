# Example


(import graphs :prefix "")

(def g (random-graph 8 0.5 42))
(printf "G(8, 1/2) edges: %p (density %.2f)" (edge-count g) (density g))

(def gm (random-graph-edges 8 10 7))
(printf "G(8, 10 edges) actual: %p" (edge-count gm))

(def ba (barabasi-albert 20 3 2 7))
(printf "BA(20) max degree: %p"
        (reduce (fn [mx u] (max mx (degree ba u))) 0 (nodes ba)))
