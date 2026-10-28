# -*- mode: janet; -*-
# graphs/hamiltonian: Hamiltonian paths & cycles in undirected graphs

#  A Hamiltonian path visits every vertex of a graph exactly once. A Hamiltonian
#  cycle visits every vertex exactly once and returns to the starting point.
#  Both are NP-complete. Uses an exact backtracking search where the difficulty
#  arises in deciding what to prune.
#
#  Reference: Diestel 2025, Graph Theory. Ch. 10 (Hamilton cycles)

(import deft :prefix "")
(import ./types :prefix "")
(import ./util :prefix "")
(import ./connectivity :prefix "")


(defn- degrees-of
  "Degree of every vertex of `g`, in the order of `(nodes g)`."
  [g]
  (map (fn [v] (length (neighbours g v))) (nodes g)))


(define hamiltonian-possible?
  ```True when graph `g` meets the necessary conditions for a cycle or path.

`mode` can be :cycle (default) or :path. The predicate is a necessary condition,
but does not guarantee existence, only that a search is worth pursuing.

For a cycle; the graph is connected, has at least 3 vertices, and every vertex
has degree >= 2.

For a path; the graph is connected, every vertex has degree >= 1, and at most two
vertices have degree exactly 1.
```
  [g :any-graph &opt mode] :boolean
  (let [n (length (nodes g))
        deg (degrees-of g)]
    (if (= mode :path)
      (or (<= n 1)
          (and (all (fn [d] (>= d 1)) deg)
               (<= (length (filter (fn [d] (= d 1)) deg)) 2)
               (graph-connected? g)))
      (and (>= n 3)
           (all (fn [d] (>= d 2)) deg)
           (graph-connected? g)))))


(define hamiltonian-dirac-sufficient?
  ```True if Dirac's sufficient condition holds for graph `g`.

The predicate is a sufficient condition, but not a necessary condition. A graph
with min degree >= n/2 will be Hamiltonian, but a Hamiltonian cycle could still
exist if the predicate returns false.
```
  [g :any-graph] :boolean
  (let [n (length (nodes g))]
    (and (>= n 3)
         (all (fn [d] (>= (* 2 d) n)) (degrees-of g)))))


# ==============================================================================
#  Search
# ==============================================================================

(defn- by-degree
  "Warnsdorff's rule as a three-way order over the indexed adjacency."
  [deg]
  (fn [a b]
    (if (= (deg a) (deg b))
      (cmp a b)
      (cmp (deg a) (deg b)))))


(defn- hamiltonian-index
  ```Hamiltonian index of graph `g` with each adjacency row in Warnsdorff order.

Returns array of [labels adj degree]
```
  [g]
  (let [labels (nodes g)
        index @{}
        adj @[]]
    (each v labels (put index v (length index)))
    (each v labels
      (def row (map index (distinct (filter (fn [w] (not= w v))
                                           (neighbours g v)))))
      (array/push adj row))
    (def deg (map length adj))
    (def order (by-degree deg))
    (each row adj (sort-with row order))
    [labels adj deg]))


(defn- hamiltonian-budget
  "A table holding :node-limit ceiling and running expansion count."
  [opts]
  (def limit (get opts :node-limit))
  @{:limit (if (number? limit) limit math/inf)
    :expanded 0
    :exceeded false})


(defn- hamiltonian-exceeded
  "Error if :node-limit is exceeded."
  [size budget]
  (error (string "hamiltonian: gave up after expanding "
                 (budget :expanded)
                 " partial paths, which is the :node-limit ceiling of "
                 (budget :limit) ". Nothing is concluded either way: raise "
                 ":node-limit to search more of the " size " vertices.")))


(defn- hamiltonian-run
  ```One DFS from the index `start`.

Return array of vertices of a cycle or path, or nil.
```
  [adj labels start closes budget]
  (def size (length adj))
  (def limit (budget :limit))
  (var expanded (budget :expanded))
  (var exceeded false)
  (def on-path @[])
  (def stamp @[])
  (def queue @[])
  (def path @[start])
  (var stamp-id 0)
  (for i 0 size
    (array/push on-path 0)
    (array/push stamp 0)
    (array/push queue 0))
  (put on-path start 1)
  # Vertices not yet visited are visited sequentially
  (defn still-connected? [left]
    (if (<= left 1)
      true
      (do
        (++ stamp-id)
        (def seed (first (seq [i :in (range size) :when (= (on-path i) 0)] i)))
        (put queue 0 seed)
        (put stamp seed stamp-id)
        (var reached 1)
        (var head 0)
        (var tail 1)
        (while (< head tail)
          (def v (queue head))
          (set head (+ head 1))
          (each w (adj v)
            (when (and (= (on-path w) 0) (not= (stamp w) stamp-id))
              (put stamp w stamp-id)
              (put queue tail w)
              (set tail (+ tail 1))
              (set reached (+ reached 1)))))
        (= reached left))))
  # Walk in the order of `hamiltonian-index`
  (defn extend [last left]
    (++ expanded)
    (if (> expanded limit)
      (do (set exceeded true) nil)
      (if (zero? left)
        (if (or (nil? closes) (closes last)) (array ;path) nil)
        (do
            (var found nil)
            (def rest (- left 1))
            (each nxt (adj last)
              (when (and (nil? found) (= (on-path nxt) 0))
                (put on-path nxt 1)
                (array/push path nxt)
                (when (still-connected? rest)
                  (set found (extend nxt rest)))
                (array/pop path)
                (put on-path nxt 0)))
            found))))
  (def done (extend start (- size 1)))
  (put budget :expanded expanded)
  (put budget :exceeded exceeded)
  (when done (map labels done)))


(defn- hamiltonian-closes
  "Boolean array over the indexed adjacency marking the vertices adjacent to
`start` so the search can determine a closed cycle from an open path."
  [adj start]
  (def closes (map (fn [_] false) (range (length adj))))
  (each w (adj start) (put closes w true))
  closes)

(defn- has-cut-vertex?
  "True when graph `g` has a vertex whose removal disconnects it."
  [g]
  (not (empty? (articulation-points g))))

(defn- hamiltonian-order
  "The vertices to try as a start, tightest first."
  [adj deg]
  (let [leaf (first (seq [i :in (range (length adj)) :when (= (deg i) 1)] i))
        others (array ;(seq [i :in (range (length adj)) :when (not= (deg i) 1)] i))]
    (if leaf
      [leaf]
      (sort-with others (by-degree deg)))))


(define hamiltonian-cycle
  ```Find a Hamiltonian cycle in the graph `g`, or nil if none exists.

Returns [v_0 v_1 … v_{n-1}] such that each consecutive pair, and (v_{n-1}, v_0), is an edge.
```
  [g :any-graph & kvs] :any
  (cond
    (not (hamiltonian-possible? g)) nil
    (has-cut-vertex? g) nil
    (do
      (let [budget (hamiltonian-budget (table ;kvs))
            [labels adj deg] (hamiltonian-index g)
             start (first (hamiltonian-order adj deg))
            closes (hamiltonian-closes adj start)
            r (hamiltonian-run adj labels start closes budget)]
        (when (get budget :exceeded) (hamiltonian-exceeded (length adj) budget))
        r))))


(define hamiltonian-path
  ```Find a Hamiltonian path of `g`, or nil if none exists.

Returns [v_0 v_1 … v_{n-1}] of distinct vertices with (v_i, v_{i+1}) an edge.
```
  [g :any-graph & kvs] :any
  (cond
    (not (hamiltonian-possible? g :path)) nil
    (empty? (nodes g)) @[]
    (do
      (let [budget (hamiltonian-budget (table ;kvs))
            [labels adj deg] (hamiltonian-index g)
            starts (hamiltonian-order adj deg)]
        (var found nil)
        (each s starts
          (when (and (nil? found) (not (get budget :exceeded)))
            (set found (hamiltonian-run adj labels s nil budget))))
        (when (get budget :exceeded) (hamiltonian-exceeded (length adj) budget))
        found))))
