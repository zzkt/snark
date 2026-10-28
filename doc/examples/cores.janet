(import graphs :prefix "")

(def g (make-graph))
(add-nodes g "a" "b" "c" "d" "e")
(add-edges g ["a" "b"] ["a" "c"] ["b" "c"] ["c" "d"] ["d" "e"] ["c" "e"])

(def cn (core-number g))
(printf "core numbers: %s"
        (string/join (map |(string (get cn $0)) (sort (nodes g) >)) " "))

(def k2 (k-core g 2))

(printf "2-core: %d of %d nodes" (node-count k2) (node-count g))
(printf "2-shell: %d" (node-count (k-shell g 2)))
(printf "2-crust: %d" (node-count (k-crust g 2)))
(printf "2-corona: %s" (string/join (sort (map string (nodes (k-corona g 2)))) " "))
