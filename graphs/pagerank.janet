# -*- mode: janet; -*-
# graphs/pagerank: PageRank and personalized PageRank

(import deft :prefix "")
(import ./types :prefix "")
(import ./util :prefix "")
(import ./record :prefix "r-")

(defn- out-degree [g u] (length (or (get (r-adjacency g) u) @{})))

(define pagerank
  ```PageRank via power iteration.

   &keys
     :damping  — damping factor (default 0.85)
     :max-iter — max iterations (default 100)
     :tol      — convergence tolerance (default 1e-6)
     :personalization — {node: weight} table (optional, default uniform)

Returns {node: rank} table where ranks sum to 1.
```
  [g :weighted & kws] :table
  (def opts (table ;kws))
  (def d (get opts :damping 0.85))
  (def max-iter (get opts :max-iter 100))
  (def tol (get opts :tol 1e-6))
  (def nodes (nodes g))
  (def n (length nodes))
  (if (= n 0) @{}
    (do
      (var rank @{})
      (var next-rank @{})
      (def out-deg @{})
      (def dangling-nodes @[])
      (def init (/ 1.0 n))
      (each u nodes
        (put rank u init)
        (put next-rank u 0.0)
        (def od (out-degree g u))
        (put out-deg u od)
        (when (= od 0) (array/push dangling-nodes u)))
      (def pers @{})
      (def personalization (get opts :personalization))
      (if personalization
        (do
          (def pers-sum (reduce + 0.0
                                (seq [u :in nodes]
                                     (get personalization u 0.0))))
          (if (> pers-sum 0)
            (each u nodes
              (put pers u (/ (get personalization u 0.0) pers-sum)))
            (each u nodes
              (put pers u (/ 1.0 n)))))
        (each u nodes
          (put pers u (/ 1.0 n))))
      (def teleport-scale (- 1.0 d))
      (for _ 0 max-iter
        (var dangling-sum 0.0)
        (each u dangling-nodes
          (+= dangling-sum (get rank u)))
        (each u nodes
          (put next-rank u (* teleport-scale (get pers u))))
        (var err 0.0)
        (each v nodes
          (def od (get out-deg v))
          (when (> od 0)
            (def contrib (/ (* d (get rank v)) od))
            (loop [[u _] :pairs (or (get (r-adjacency g) v) @{})]
              (put next-rank u (+ (get next-rank u) contrib)))))
        (each u nodes
          (put next-rank u (+ (get next-rank u) (* d dangling-sum (get pers u))))
          (+= err (math/abs (- (get next-rank u) (get rank u)))))
        (def stale rank)
        (set rank next-rank)
        (set next-rank stale)
        (when (< err tol) (break)))
      rank)))
