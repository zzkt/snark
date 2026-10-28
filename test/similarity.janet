# -*- mode: janet; -*-
# graphs/test/similarity: Jaccard, cosine, and kNN

(import ./helper :prefix "")
(import ../graphs :prefix "")

(defn ≈ [a b] (< (math/abs (- a b)) 1e-6))


# Two nodes sharing one neighbour. Jaccard is exactly 1/2.
#   a -- x -- b     (a: {x},   b: {x})
#   a -- y          (c: {x,y})
#   c -- y

(def two-over-one
  (fn []
    (def g (make-graph))
    (add-nodes g @["a" "b" "c" "x" "y"])
    (add-edges g @[["a" "x"] ["b" "x"] ["a" "y"] ["c" "y"]])
    g))

# Identical neighbourhoods: Jaccard 1.0
(def twins
  (fn []
    (def g (make-graph))
    (add-nodes g @["a" "b" "x" "y"])
    (add-edges g @[["a" "x"] ["a" "y"] ["b" "x"] ["b" "y"]])
    g))

(print "\n* jaccard-similarity")

(def g1 (two-over-one))
(cassert-eq "jaccard: shared one of two union is 1/2"
            (≈ (jaccard-similarity g1 "a" "b") 0.5) true)

(cassert-eq "jaccard: symmetric"
            (≈ (jaccard-similarity g1 "a" "b")
               (jaccard-similarity g1 "b" "a")) true)

(cassert-eq "jaccard: self is 1 when the node has neighbours"
            (≈ (jaccard-similarity g1 "a" "a") 1.0) true)

(cassert-eq "jaccard: a pair that reaches nothing in common is 0"
            (≈ (jaccard-similarity g1 "a" "x") 0.0) true)

(cassert-eq "jaccard: and the partial overlap next to it is 1/3"
            (≈ (jaccard-similarity g1 "x" "y") (/ 1.0 3.0)) true)

(def g2 (twins))
(cassert-eq "jaccard: identical neighbourhoods are 1"
            (≈ (jaccard-similarity g2 "a" "b") 1.0) true)

# Two isolated nodes have an empty union (divide-by-zero case)
(def iso (make-graph))
(add-nodes iso @["p" "q"])
(cassert-eq "jaccard: two isolated nodes are 0, not a division by zero"
            (≈ (jaccard-similarity iso "p" "q") 0.0) true)


(print "\n* cosine-similarity")

# A path a-b-c
(def path (make-graph))
(add-nodes path @["a" "b" "c"])
(add-edges path @[["a" "b"] ["b" "c"]])

(cassert-eq "cosine: a node with itself is 1"
            (≈ (cosine-similarity path "b" "b") 1.0) true)

(cassert-eq "cosine: two nodes whose rows share a neighbour are 1"
            (≈ (cosine-similarity path "a" "c") 1.0) true)

(cassert-eq "cosine: rows with no overlap are 0"
            (≈ (cosine-similarity path "a" "b") 0.0) true)

(cassert-eq "cosine: symmetric"
            (≈ (cosine-similarity path "a" "b")
               (cosine-similarity path "b" "a")) true)

(cassert-eq "cosine: an unknown node is 0 rather than a division by zero"
            (≈ (cosine-similarity path "a" "nope") 0.0) true)

(cassert-eq "cosine: two isolated nodes are 0"
            (≈ (cosine-similarity iso "p" "q") 0.0) true)


(print "\n* knn")

# u's neighbours are {x,y,z,w} which determine Jaccard
#   c reaches {y,z} -> 2/4 = 0.5     a reaches {x}   -> 1/4 = 0.25
#   b reaches {y}   -> 1/4 = 0.25    d reaches {z}   -> 1/4 = 0.25
#   x,y,z,w reach u and one candidate, neither of which is in u's row -> 0
(defn hub-graph []
  (def g (make-graph))
  (add-nodes g @["u" "a" "b" "c" "d" "x" "y" "z" "w"])
  (add-edges g @[["u" "x"] ["u" "y"] ["u" "z"] ["u" "w"]
                 ["a" "x"] ["b" "y"] ["c" "y"] ["c" "z"] ["d" "z"]])
  g)

(def gh (hub-graph))

# scores of one knn call
(defn scores-of [res] (map (fn [p] (get p 1)) res))

(defn descending? [s]
  (all (fn [i] (>= (s i) (s (+ i 1)))) (range (- (length s) 1))))

(def all10 (knn gh "u" 10))
(cassert-eq "knn: every node but u is a candidate"
            (length all10) 8)

(cassert-eq "knn: u is never its own neighbour"
            (truthy? (some (fn [p] (= "u" (get p 0))) all10)) false)

(cassert-eq "knn: scores in descending order"
            (descending? (scores-of all10)) true)

(cassert-eq "knn: the best candidate scores 0.5"
            (≈ (get (get all10 0) 1) 0.5) true)

(cassert-eq "knn: scores are same as Jaccard"
            (≈ (get (get all10 0) 1) (jaccard-similarity gh "u" "c")) true)

(cassert-eq "knn: every reported score agrees with jaccard-similarity"
            (all (fn [p] (≈ (get p 1) (jaccard-similarity gh "u" (get p 0)))) all10) true)


(def k3 (knn gh "u" 3))
(cassert-eq "knn: k=3 returns three"
            (length k3) 3)
(cassert-eq "knn: k=3 is descending too"
            (descending? (scores-of k3)) true)
(cassert-eq "knn: k=3 keeps the best score"
            (≈ (get (get k3 0) 1) 0.5) true)
(cassert-eq "knn: k=3 keeps the two runners-up at 0.25"
            (all (fn [i] (≈ (get (k3 i) 1) 0.25)) [1 2]) true)
(cassert-eq "knn: k=3 drops everything below the cut"
            (all (fn [i] (>= (get (k3 i) 1) 0.25)) (range 3)) true)

# k larger than the graph should return every candidate
(def kbig (knn gh "u" 99))
(cassert-eq "knn: k beyond the node count returns them all"
            (length kbig) 8)
(cassert-eq "knn: k beyond the node count is still descending"
            (descending? (scores-of kbig)) true)

# k=1 floor check
(cassert-eq "knn: k=1 returns one" (length (knn gh "u" 1)) 1)

# Both modes must both run and sort in descending order
(def j-mode (knn gh "u" 3))
(def c-mode (knn gh "u" 3 :cosine))

(cassert-eq "knn: :cosine mode returns the same count"
            (length c-mode) 3)

(cassert-eq "knn: :cosine mode is descending too"
            (descending? (scores-of c-mode)) true)

(cassert-eq "knn: :cosine mode scores by cosine, not jaccard"
            (all (fn [p] (≈ (get p 1)
                            (cosine-similarity gh "u" (get p 0)))) c-mode) true)

# An isolated node has no neighbours. all scores should be zero
(cassert-eq "knn: an isolated node still returns its candidates"
            (length (knn iso "p" 5)) 1)
(cassert-eq "knn: an isolated node scores everything zero"
            (≈ (get (get (knn iso "p" 5) 0) 1) 0.0) true)

(print-results)
