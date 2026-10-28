# -*- mode: janet; -*-
# graphs/spanning-tree: spanning-tree, Laplacian, matrix determinant, and
# Eulerian-circuit counts (Matrix-Tree & BEST theorems).

(import deft :prefix "")
(import ./types :prefix "")
(import ./util :prefix "")

# ==============================================================================
#  Laplace expansion helpers
# ==============================================================================

(defn- factorial
  "n! with factorial(0) = 1 and factorial(negative) = 1."
  [n]
  (var acc 1)
  (for i 2 (+ n 1) (set acc (* acc i)))
  acc)

(defn- copy-matrix
  "Deep copy of a matrix (array of rows)."
  [m]
  (map (fn [row] (array/slice row)) m))


# =============================================================================
#  Matrix determinant (fraction-free Bareiss)
# =============================================================================

(define matrix-determinant
  ```Determinant of a square matrix `m` represented as an array of rows.

Computed using the fraction-free Bareiss algorithm. The result of a 0×0 matrix
is 1 and a singular matrix returns 0. Use integers for exact counts.
```
  [m :array] :number
  (def a (copy-matrix m))
  (def n (length a))
  (cond
    (= n 0) 1
    (= n 1) (get-in a [0 0])
    (do
      (var sign 1)
      (var prev 1)
      (for k 0 (- n 1)
        (var piv k)
        (while (and (< piv n) (zero? (get-in a [piv k])))
          (++ piv))
        (if (= piv n)
          (set sign 0)
          (do
            (when (not= piv k)
              (def tmp (get a piv))
              (put a piv (get a k))
              (put a k tmp)
              (set sign (- sign)))
            (def pk (get-in a [k k]))
            (for i (+ k 1) n
              (for j (+ k 1) n
                (def newval
                  (/ (- (* (get-in a [i j]) pk)
                        (* (get-in a [i k]) (get-in a [k j])))
                     prev))
                (put-in a [i j] newval)))
            (set prev pk))))
      (* sign (if (= sign 0) 0 (get-in a [(- n 1) (- n 1)]))))))


# =============================================================================
#  Laplacian
# =============================================================================

(define graph-laplacian
  ```Laplacian matrix of a graph `g` as an array of rows.

Row/column i corresponds to the i^th entry of (nodes g).
The entry (i, j) is -w(i,j) for i ≠ j where w is the edge count for undirected
graphs, the arc weight for digraphs, and arc multiplicity for multidigraphs.
Self-loops are excluded.

Returns array or rows.
```
  [g :any-graph] :array
  (def ns (nodes g))
  (def n (length ns))
  (def idx @{})
  (for i 0 n (put idx (ns i) i))
  (def rows @[])
  (each u ns
    (def row (array/new-filled n 0))
    (var diag 0)
    (each [v w] (out-neighbours g u)
      (when (not= u v)
        (def wt (if (number? w) w 1))
        (+= diag wt)
        (put row (get idx v) (- (get row (get idx v)) wt))))
    (put row (get idx u) (+ (get row (get idx u)) diag))
    (array/push rows row))
  rows)


# =============================================================================
#  Other helpers
# =============================================================================

(defn- cofactor-removed
  "Reduced Laplacian of graph `g` with row and column of the r-th node removed."
  [g r]
  (def L (graph-laplacian g))
  (def n (length L))
  (seq [i :range [0 n] :when (not= i r)]
    (seq [j :range [0 n] :when (not= j r)]
      (get-in L [i j]))))

(defn- out-multiplicity
  "Total number of arcs leaving `u` (counting multiplicity)."
  [g u]
  (typecase-strict g
    :multidigraph (sum (values (get (g :out) u)))
    (fn [untagged] (length (get (untagged :adj) u)))))

(defn- in-multiplicity
  "Total number of arcs entering `u` (counting multiplicity)."
  [g u]
  (typecase-strict g
    :multidigraph (sum (values (get (g :in) u)))
    (fn [untagged]
      (sum (seq [[_ tbl] :pairs (untagged :adj) :when (get tbl u)] 1)))))


# =============================================================================
#  Counting spanning trees
# =============================================================================

(define count-spanning-trees
  ```Number of spanning trees of an undirected graph `g`.

By the Matrix-Tree Theorem, any cofactor of the Laplacian equals the number of
spanning trees. An empty graph has no spanning tree, and a single vertex has 1.

Directed graphs are not supported (use `graph-laplacian` for directed graphs).
```
  [g :any-graph] :number
  (var n (length (nodes g)))
  (when (>= n 2)
    (set n (matrix-determinant (cofactor-removed g 0))))
  (typecase-strict g
    :graph n
    :multigraph n
    :weighted-graph n
    :tree n
    (fn [_] (error "count-spanning-trees requires an undirected graph type"))))


# =============================================================================
#  Counting Eulerian circuits (BEST theorem)
# =============================================================================

(defn- count-eulerian-multiplicity
  "BEST theorem count for directed graphs."
  [g]
  (let [ns (nodes g)
        n (length ns)]
    (cond
      (= n 0) 0
      (do
        (var balanced true)
        (var total-arcs 0)
        (each u ns
          (when balanced
            (def out (out-multiplicity g u))
            (def in (in-multiplicity g u))
            (+= total-arcs out)
            (when (not= in out) (set balanced false))))
        (cond
          (not balanced) nil
          (= total-arcs 0) 0
          (do
            (def root 0)
            (def tau (matrix-determinant (cofactor-removed g root)))
            (var factor 1)
            (each u ns
              (def d (out-multiplicity g u))
              (set factor (* factor (factorial (- d 1)))))
            (* tau factor)))))))


(define count-eulerian-circuits
  ```Number of Eulerian circuits of a balanced digraph `g` via the BEST theorem.

Returns nil if the digraph is not balanced, 0 if it has no arcs.```
  [g :any-graph] (or :number :nil)
  (typecase-strict g
    :multidigraph (count-eulerian-multiplicity g)
    :digraph      (count-eulerian-multiplicity g)
    (fn [_] (error "count-eulerian-circuits: requires a directed graph type"))))
