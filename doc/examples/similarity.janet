# Example


(import graphs :prefix "")



# A triangle, where every pair of nodes has Jaccard similarity 1/3


(def g (make-graph))
(add-nodes g ["a" "b" "c" "d"])
(add-edges g [["a" "b"] ["a" "c"] ["b" "c"]])

(printf "J(a,b) = %.3f" (jaccard-similarity g "a" "b"))

(printf "knn(a) = %p" (knn g "a" 1))

(printf "knn(a) cosine = %p" (knn g "a" 1 :cosine))
