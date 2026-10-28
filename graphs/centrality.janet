# -*- mode: janet; -*-
# graphs/centrality: degree, closeness, and betweenness centrality

(import deft :prefix "")
(import ./types :prefix "")
(import ./util :prefix "")


(defn- degree-normalizer
  "Scale a raw degree into a normalized centrality. 1/(n-1)."
  [n]
  (if (<= n 1) 1.0 (/ 1.0 (- n 1))))

(defn- normalized-degree-centrality
  "Map each node to its degree, normalized by n-1."
  [g deg-fn]
  (let [nodes (nodes g)
        n (length nodes)]
    (def result @{})
    (each u nodes
      (put result u (if (<= n 1) 1.0 (/ (deg-fn g u) (- n 1)))))
    result))


(define degree-centrality
  ```Degree centrality for a graph `g`.

For undirected graphs this is the the simple degree, for directed graphs, the
total in-degree + out-degree.

Returns {node: degree-centrality} normalized by n-1.```
  [g :any-graph] :table
  (def ins (make-in-neighbours-fn g))
  (normalized-degree-centrality g
    (fn [g u]
      (if (directed? g)
        (+ (length (ins g u)) (length (neighbours g u)))
        (length (neighbours g u))))))


(define degree-out-centrality
  ```Out-degree centrality.

Uses only the out-degree, equal to degree-centrality for undirected graphs.
Returns {node: out-degree-centrality} normalized by n-1.```
  [g :any-graph] :table
  (normalized-degree-centrality g (fn [g u] (length (neighbours g u)))))


(define degree-in-centrality
  ```In-degree centrality.

Uses only the in-degree, equal to degree-centrality for undirected graphs.
Returns {node: in-degree-centrality} normalized by n-1.```
  [g :any-graph] :table
  (def ins (make-in-neighbours-fn g))
  (normalized-degree-centrality g (fn [g u] (length (ins g u)))))


(define multidigraph-degree-centrality
 ```Directed degree centrality for a :multidigraph.

Returns {node: {:out o :in i :total t}}, each normalized by n-1.```
  [g :multidigraph] :table
  (let [nodes (nodes g)
        norm (degree-normalizer (length nodes))]
    (def result @{})
    (each u nodes
      (def out-deg (length (neighbours g u)))
      (def in-deg (length (in-neighbours g u)))
      (put result u
        {:out (* out-deg norm)
         :in (* in-deg norm)
         :total (* (+ out-deg in-deg) norm)}))
    result))


(define multidigraph-in-degree-centrality
  ```In-degree centrality for a :multidigraph, normalized by n-1.

Returns {node: value}.```
  [g :multidigraph] :table
  (let [nodes (nodes g)
        norm (degree-normalizer (length nodes))]
    (def result @{})
    (each u nodes (put result u (* (length (in-neighbours g u)) norm)))
    result))


(define in-bfs-distances
```Hop distances to `u` in graph `g` using incoming edges only.

`in-fn` is an optional in-neighbour accessor from `make-in-neighbours-fn`,
use when sweeping many sources to avoid a walk rescaning the adjacency table.
```
  [g :any-graph u :dynamic &opt in-fn] :table
  (default in-fn (make-in-neighbours-fn g))
  (def dist @{u 0})
  (def data @[u])
  (var head 0)
  (var end 1)
  (while (< head end)
    (def v (data head))
    (set head (+ head 1))
    (def d (get dist v))
    (each w (in-fn g v)
      (unless (get dist w)
        (put dist w (+ d 1))
        (array/push data w)
        (set end (length data)))))
  dist)


(define closeness-centrality
  ```Measure closeness-centrality of a single node in graph `g`.

For connected graphs (n-1)/sum(distances). Disconnected graphs use the
Wasserman-Faust improved normalisation, i.e. for a node reaching s other
nodes at total distance d, closeness = (s/(n-1)) * (s/d).
Directed graphs use inward distances.
```
  [g :any-graph node :dynamic &opt in-fn] :number
  (def nodes (nodes g))
  (def n (length nodes))
  (def dists (in-bfs-distances g node in-fn))
  (var total-dist 0.0)
  (var s 0)
  (each v nodes
    (when (and (not= v node) (get dists v))
      (+= total-dist (get dists v))
      (++ s)))
  (if (= s 0)
    0.0
    (* (/ s (- n 1)) (/ s total-dist))))


(define closeness-centrality-all
  ```Measure centrality of all nodes in a graph.

Returns {node: closeness-centrality}.```
  [g :any-graph] :table
  (def nodes (nodes g))
  (def ins (make-in-neighbours-fn g))
  (def res @{})
  (each u nodes
    (put res u (closeness-centrality g u ins)))
  res)


# Performance oriented Brandes BFS helpers

(defn- brandes-shared
  "Per-graph shared data for a Brandes sweep; node order, a name->index map,
and working arrays, allocated once per sweep rather than once per source."
  [g]
  (def nodes (canonical-nodes (nodes g)))
  (def n (length nodes))
  (def idx @{})
  (loop [i :range [0 n]] (put idx (nodes i) i))
  (def nbrs (make-raw-adj-fn g))
  {:nodes nodes
   :n n
   :idx idx
   :nbrs nbrs
   :dist (array/new-filled n -1)
   :sigma (array/new-filled n 0)
   :pred (array/new-filled n nil)
   :delta (array/new-filled n 0.0)
   :stack @[]
   :qdata @[]
   :touched @[]})


(defn- brandes-edge
  "Fold the edge v->w into the Brandes shortest-path DAG."
  [idx dist sigma pred touched qdata w v dv sv]
  (let [wi (idx w)
        dw (dist wi)]
    (cond
      (= dw -1)
      (do (put dist wi (+ dv 1))
          (array/push qdata wi)
          (array/push touched wi)
          (if (pred wi) (array/clear (pred wi)) (put pred wi @[]))
          (array/push (pred wi) v)
          (+= (sigma wi) sv))
      (= dw (+ dv 1))
      (do (array/push (pred wi) v)
          (+= (sigma wi) sv)))))


(defn- brandes-bfs
  "One Brandes BFS pass on `g` from source `s` over the shared scratch `sc`.

Writes into scratch and returns nil. Caller can read computed `dist`, `pred`,
`sigma`, `stack` and `touched` from scratch."
  [g sc s]
  (def nodes (get sc :nodes))
  (def idx (get sc :idx))
  (def dist (get sc :dist))
  (def sigma (get sc :sigma))
  (def pred (get sc :pred))
  (def stack (get sc :stack))
  (def qdata (get sc :qdata))
  (def touched (get sc :touched))
  (def nbrs (get sc :nbrs))
  (array/clear stack)
  (array/clear qdata)
  (array/clear touched)
  (def s-idx (idx s))
  (put dist s-idx 0)
  (put sigma s-idx 1)
  (array/push qdata s-idx)
  (var head 0)
  (while (< head (length qdata))
    (def v (get qdata head))
    (set head (+ head 1))
    (array/push stack v)
    (array/push touched v)
    (def dv (dist v))
    (def sv (sigma v))
    (def tbl (when nbrs (nbrs g (nodes v))))
    (if tbl
      (loop [[w _] :pairs tbl]
        (brandes-edge idx dist sigma pred touched qdata w v dv sv))
      (each w (neighbours g (nodes v))
        (brandes-edge idx dist sigma pred touched qdata w v dv sv))))
  nil)


(defn- brandes-accumulate
  "Single Brandes dependency-accumulation pass from source `s` using scratch."
  [g sc s f]
  (brandes-bfs g sc s)
  (def nodes (get sc :nodes))
  (def idx (get sc :idx))
  (def pred (get sc :pred))
  (def sigma (get sc :sigma))
  (def delta (get sc :delta))
  (def stack (get sc :stack))
  (def touched (get sc :touched))
  (each i touched (put delta i 0.0))
  (var k (- (length stack) 1))
  (while (>= k 0)
    (def w (get stack k))
    (set k (- k 1))
    (def pw (pred w))
    (when pw
      (def dw (delta w))
      (def sw (sigma w))
      (each v pw
        (+= (delta v) (* (/ (sigma v) sw) (+ 1 dw)))))
    (unless (= w (idx s))
      (f (nodes w) (delta w))))
  (each i touched
    (put (get sc :dist) i -1)
    (put (get sc :sigma) i 0)
    (when (pred i) (array/clear (pred i)))))


(define betweenness-centrality
  ```Betweenness centrality of a graph `g` using Brandes BFS.

Returns {node: betweenness-centrality}
```
  [g :any-graph] :table
  (def nodes (nodes g))
  (def n (length nodes))
  (def sc (brandes-shared g))
  (def cb @{})
  (each v nodes (put cb v 0.0))
  (each s nodes
    (brandes-accumulate g sc s
      (fn [w dw] (put cb w (+ (get cb w) dw)))))
  (when (>= n 3)
    (def norm (/ 1.0 (* (- n 1) (- n 2))))
    (each v nodes (put cb v (* (get cb v) norm))))
  cb)


(define adjacency-matvec
  "Adjacency-matrix product. y[u] = Σ_{v ∈ in-neighbours(u)} x[v]."
  [g :any-graph x :table nodes :array] :table
  (def y @{})
  (each u nodes
    (put y u 0.0))
  (each [u tbl] (pairs (g :adj))
    (each [v _] (pairs tbl)
      (when (not= nil (get x u))
        (put y v (+ (get y v 0.0) (get x u))))))
  y)


(define eigenvector-centrality
 ```Eigenvector centrality via power iteration.

Repeatedly multiplies the score vector by the adjacency matrix and normalizes.
For undirected graphs this converges to the principal eigenvector.
A single isolated node has eigenvalue 1 and centrality 1.

Returns {node: score} with max normalized to 1.
```
  [g :any-graph &opt iter] :table
  (def nodes (nodes g))
  (def n (length nodes))
  (if (= n 0)
    @{}
    (if (= n 1)
      @{(first nodes) 1.0}
      (do
      (def iters (or iter 100))
      (def x @{})
      (def prev @{})
      (each u nodes (put x u 1.0) (put prev u 0.0))
      (for _ 0 iters
        (def y (adjacency-matvec g x nodes))
        (var norm 0.0)
        (each u nodes (+= norm (* (get y u) (get y u))))
        (def scale (if (> norm 0) (math/sqrt norm) 1))
        (each u nodes
          (put prev u (get x u))
          (put x u (/ (get y u) scale))))
      (each u nodes (put x u (/ (+ (get x u) (get prev u)) 2)))
      (def mx (reduce max 0.0 (values x)))
      (if (> mx 0)
        (each u nodes (put x u (/ (get x u) mx))))
      x))))


(define katz-centrality
 ```Katz centrality. x = (α Aᵀ + I) x.

`alpha` defaults to 1/(Δ_max+1) (safe for convergence)
`beta` (exogenous base) defaults to 1.0
`normalized` scales max to 1
`iterations` controls the number of itteration steps (default 100).

 Returns {node: score}
```
  [g :any-graph &opt alpha beta normalized iterations] :table
   (default iterations 100)
  (def nodes (nodes g))
  (def n (length nodes))
  (if (= n 0)
    @{}
    (do
      (var degmax 0)
      (each u nodes
        (def d (length (neighbours g u)))
        (when (> d degmax) (set degmax d)))
      (def a (or alpha (/ 1.0 (+ degmax 1))))
      (def b (or beta 1.0))
      (def x @{})
      (each u nodes (put x u 1.0))
      (for _ 0 iterations
        (def y (adjacency-matvec g x nodes))
        (each u nodes (put x u (+ b (* a (get y u))))))
      (when normalized
        (def mx (reduce max 0.0 (values x)))
        (if (> mx 0)
          (each u nodes (put x u (/ (get x u) mx)))))
      x)))


(define harmonic-centrality
 ```Harmonic centrality: Σ 1/d(u,v) over reachable v.

Directed graphs uses inward distances, so d(u,v) is distance from v to u.
For undirected graphs in == out (symmetric sum).
optional arg `normalized` rescales by H(n-1) = 1/1+...+1/(n-1).

Returns {node: value}
```
  [g :any-graph &opt normalized] :table
  (def nodes (nodes g))
  (def n (length nodes))
  (def res @{})
  (def scale
    (if normalized
      (sum (seq [k :in (range 1 n)] (/ 1.0 k)))
      1))
  # Use single in-edge index for the sweep
  (def ins (make-in-neighbours-fn g))
  (each u nodes
    (def dists (in-bfs-distances g u ins))
    (var s 0.0)
    (each v nodes
      (when (and (not= v u) (get dists v))
        (+= s (/ 1.0 (get dists v)))))
    (put res u (/ s (max scale 1))))
  res)


(define percolation-centrality
  ```Percolation centrality (Piraveenan, Prokopenko, and Hossain)

Brandes-style accumulation over unweighted BFS. For each source node, dependency
contribution is delta[w] * x[s] / (sum(x) - x[w]). Results are scaled by 1/(n-2).
When `normalized`, max is normailsed to 1.

Returns {node: value}
```
  [g :any-graph x :table &opt normalized] :table
  (def nodes (nodes g))
  (def n (length nodes))
  (def cb @{})
  (each v nodes (put cb v 0.0))
  (def total (reduce + 0.0 (values x)))
  (def sc (brandes-shared g))
  (each s nodes
    (brandes-accumulate g sc s
      (fn [w dw]
        (def denom (- total (get x w)))
        (when (not= denom 0)
          (put cb w (+ (get cb w) (* dw (/ (get x s) denom))))))))
  (when (not= (- n 2) 0)
    (def scale (/ 1.0 (- n 2)))
    (each v nodes (put cb v (* (get cb v) scale))))
  (when normalized
    (def mx (reduce max 0.0 (values cb)))
    (when (> mx 0)
      (each v nodes (put cb v (/ (get cb v) mx)))))
  cb)
