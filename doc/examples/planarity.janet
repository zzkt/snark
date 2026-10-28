# Example


(import graphs :prefix "")



# =K₄= is the largest complete planar graph, =K₅= is the smallest complete graph that is not.


(defn k-n [n]
  (def g (make-graph))
  (add-nodes g (seq [i :range [0 n]] i))
  (add-edges g (seq [i :range [0 n] j :range [0 n] :when (< i j)] @[i j]))
  g)

(printf "is K4 planar? %p" (is-planar? (k-n 4)))
(printf "is K5 planar? %p" (is-planar? (k-n 5)))

(def faces (planar-faces (k-n 4)))
(printf "K4 embedding: %p faces, boundary lengths %s"
        (length faces)
        (string/join (map |(string (length ($ :vs))) faces) " "))
