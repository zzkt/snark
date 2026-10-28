# -*- mode: janet; -*-
# graphs/similarity: Node similarity (Jaccard, cosine, and kNN)

(import deft :prefix "")
(import ./types :prefix "")
(import ./util :prefix "")
(import ./heap :prefix "")


(define jaccard-similarity
  ```Jaccard similarity between nodes `u` and `v`.
Defined as |N(u) ∩ N(v)| / |N(u) ∪ N(v)|.```
  [g :any-graph u :dynamic v :dynamic] :number
  (let [nu (neighbours g u)
        nv (neighbours g v)
        nv-set (to-set nv)
        inter-size (length (filter |(get nv-set $) nu))
        union-size (+ (length nu) (length nv) (- inter-size))]
    (if (= union-size 0) 0.0
      (/ inter-size union-size))))


(define cosine-similarity
  ```Cosine similarity between nodes `u` and `v`.
Uses adjacency vector overlap weighted by edge values.```
  [g :any-graph u :dynamic v :dynamic] :number
  (let [adj-u (edge-weights g u)
        adj-v (edge-weights g v)]
    (if (or (nil? adj-u) (nil? adj-v)
            (= 0 (length adj-u)) (= 0 (length adj-v)))
      0.0
      (lett [(dot 0.0)
             (norm-u 0.0)
             (norm-v 0.0)]
        (loop [[x wu] :pairs adj-u]
          (+= norm-u (* wu wu))
          (def wv (get adj-v x))
          (when wv (+= dot (* wu wv))))
        (loop [[_ wv] :pairs adj-v]
          (+= norm-v (* wv wv)))
        (def denom (* (math/sqrt norm-u)
                      (math/sqrt norm-v)))
        (if (= denom 0.0) 0.0 (/ dot denom))))))


(define knn
 ```Find top `k` most similar nodes to `u` sorted by descending similarity.

Uses jaccard by default, pass :cosine as `mode` for cosine.

Returns an array of [node similarity].
```
  [g :any-graph u :dynamic k :integer &opt mode] :array
  (def mode (or mode :jaccard))
  (def sim-fn (if (= mode :cosine) cosine-similarity jaccard-similarity))
  # Keep only the top-k candidates in a bounded min-heap keyed by similarity.
  (def top (make-binary-min-heap @[]
                                 :compare (fn [a b] (< (last a) (last b)))))
  (each v (nodes g)
    (when (not= v u)
      (def candidate [v (sim-fn g u v)])
      (if (< (heap-size top) k)
        (heap-push top candidate)
        (when (and (> (heap-size top) 0)
                   (> (last candidate) (last (heap-peek top))))
          (heap-pop top)
          (heap-push top candidate)))))
  # drain ascending by similarity, reverse for descending order
  (def result @[])
  (while (and (> (heap-size top) 0) (< (length result) k))
    (array/push result (heap-pop top)))
  (reverse result))
