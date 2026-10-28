# -*- mode: janet; -*-
# graphs/metrics: Graph density, diameter, eccentricity, and clustering coefficients

(import deft :prefix "")
(import ./types :prefix "")
(import ./digraph :prefix "")
(import ./util :prefix "")

(define bfs-distances
  ```BFS from node `start` to all nodes in an unweighted graph `g`.
Accepts an optional `adj-fn` function for neighbour finding.

Returns array of {node: distance}.
```
  [g :any-graph start :dynamic &opt adj-fn]
  (default adj-fn (make-raw-adj-fn g))
  (def dist @{start 0})
  (def data @[start])
  (var head 0)
  (var end 1)
  (while (< head end)
    (def u (data head))
    (set head (+ head 1))
    (def d (get dist u))
    (def tbl (adj-fn g u))
    (if tbl
      (loop [[v _] :pairs tbl]
        (unless (get dist v)
          (put dist v (+ d 1))
          (array/push data v)
          (set end (length data))))
      (each v (neighbours g u)
        (unless (get dist v)
          (put dist v (+ d 1))
          (array/push data v)
          (set end (length data))))))
  dist)

(define all-pairs-distances
  ```Find istances between every pair of node in graph `g`.

BFS from every node once. Shared by other metrics function via the `*-of-dists`
helpers to avoid unecessary traversals.

 Returns {u: {v: dist}} reachability matrix.
```
  [g :any-graph] :table
  (def nodes (nodes g))
  (def nbrs (make-raw-adj-fn g))
  (def out @{})
  (each u nodes
    (put out u (bfs-distances g u nbrs)))
  out)

# ==============================================================================
#  Scans using a precomputed {u: {v: dist}} matrix.
#
#  If several metrics are required, the function `(all-pairs-distances g)` can
#  be called to construct the matrix once and then reuse it.
# ==============================================================================

(define eccentricity-of-dists
  ```Eccentricity of `u`, the max distance to any other reachable vertex.

Accepts `dists` table of {v: dist}, i.e. output from bfs-distances or a single
row of all-pairs-distances matrix.

Returns distance, or nil if `u` is isolated.
```
  [dists :table u :dynamic] :integer-or-nil
  (var ecc 0)
  (each v (keys dists)
    (unless (= u v)
      (def d (get dists v))
      (when (and d (> d ecc)) (set ecc d))))
  (if (> ecc 0) ecc nil))

(define eccentricities-of-dists
  ```All eccentricities from a `dists` matrix.
Returns table of {u: ecc} with 0 for isolated vertices.```
  [dists :table] :table
  (def ecc @{})
  (each u (keys dists)
    (put ecc u (or (eccentricity-of-dists (get dists u) u) 0)))
  ecc)

# ==============================================================================
#  Streaming folds for all-pairs
#
#  `all-pairs-distances` constructs a {u: {v: d}} matrix, every `*-of-dists`
#   walks it again, and rows can be folded one at a time.
#
#   The fold keeps at most one row active. Sources are visited as distinct
#   nodes in first-occurrence order, consistent with the matrix.
# ==============================================================================

(defn- distinct-nodes
  "Distinct nodes of graph `g` in first-occurrence order."
  [g]
  (keys (to-set (nodes g))))

(defn eccentricities-scan
  ```Eccentricity of every node in graph `g`.```
  [g]
  (let [srcs (distinct-nodes g)
        nbrs (make-raw-adj-fn g)
        ecc @{}]
    (each u srcs
      (def row (bfs-distances g u nbrs))
      (var m 0)
      (each d (values row)
        (when (> d m) (set m d)))
      (put ecc u m))
    ecc))

(defn- radius-of-ecc
  ```Minimum eccentricity across all vertices.
   Returns integer, or nil if no vertex reaches any other.```
  [ecc]
  (var min-ecc math/inf)
  (each [u e] (pairs ecc)
    (when (and (> e 0) (< e min-ecc)) (set min-ecc e)))
  (if (= min-ecc math/inf) nil min-ecc))

(defn- center-of-ecc
  ```Find vertices with minimum eccentricity.

Minimises over all vertices (including eccentricity 0), which means it can
disagree with `radius-of-ecc` if a graph contains isolated nodes.

Returns array of nodes, in `ecc` key order.
```
  [ecc]
  (when (empty? ecc) (break @[]))
  (var min-ecc (get ecc (first (keys ecc))))
  (each u (keys ecc)
    (when (< (get ecc u) min-ecc) (set min-ecc (get ecc u))))
  (filter (fn [n] (= (get ecc n) min-ecc)) (keys ecc)))

(defn- average-path-length-scan
  "Average shortest path length over all reachable ordered node pairs."
  [g] :number
  (let [srcs (distinct-nodes g)
        nbrs (make-raw-adj-fn g)]
    (var total-dist 0.0)
    (var total-pairs 0)
    (each u srcs
      (def row (bfs-distances g u nbrs))
      (each [v d] (pairs row)
        (unless (= u v)
          (+= total-dist d)
          (++ total-pairs))))
    (if (= total-pairs 0) 0.0
      (/ total-dist total-pairs))))


(define diameter-of-dists
  ```Longest shortest path between any two vertices from a dists matrix.

Returns 0 for empty or single-node matrices, or nil if disconnected.
```
  [dists :table] :integer-or-nil
  (when (empty? dists) (break 0))
  (def all-nodes (keys dists))
  (var max-dist 0)
  (var complete true)
  (each [u du] (pairs dists)
    (each v all-nodes
      (unless (= u v)
        (def d (get du v))
        (if d
          (when (> d max-dist) (set max-dist d))
          (set complete false)))))
  (if complete max-dist nil))


(define radius-of-dists
  ```Minimum eccentricity across all node using a `dists` matrix.

Returns nil for empty/single-node matrices or a disconnected graph.```
  [dists :table] :integer-or-nil
  (radius-of-ecc (eccentricities-of-dists dists)))

(define center-of-dists
  ```Find node(s) with minimum eccentricity from a `dists` matrix.

Returns an array (empty for an empty matrix) in matrix's key order.
```
  [dists :table] :array
  (center-of-ecc (eccentricities-of-dists dists)))

(define average-path-length-of-dists
  ```Average shortest path length of reachable node pairs from a `dists` matrix.

Returns a number, or 0.0 for matrices with no reachable pair.
```
  [dists :table] :number
  (var total-dist 0.0)
  (var total-pairs 0)
  (each [u du] (pairs dists)
    (each [v d] (pairs du)
      (unless (= u v)
        (when d
          (+= total-dist d)
          (++ total-pairs)))))
  (if (= total-pairs 0) 0.0
    (/ total-dist total-pairs)))


(define graph-density
 ```Find the density of graph `g`, the ratio of actual edges to possible edges.

Density is computed as m / (n(n-1)) for directed graphs, 2m / (n(n-1)) for
undirected graphs. Multigraphs are read with the structural view (i.e. parallel
edges collapsed. Use multigraph-density for the multiplicity aware density

Returns a number in [0, 1]
```
  [g :any-graph] :number
  (def nodes (nodes g))
  (def n (length nodes))
  (if (<= n 1) 0.0
    (do
      (var deg-sum 0)
      (each u nodes (+= deg-sum (length (neighbours g u))))
      (/ deg-sum (* n (- n 1))))))


(define graph-diameter
 ```Find the diamter of graph `g`, longest shortest path between any two nodes.

Returns nil for empty/single-node matrices or a disconnected graph.```
  [g :any-graph] :integer-or-nil
   (let [srcs (distinct-nodes g)
        n (length srcs)]
    (when (= n 0) (break 0))
    (def nbrs (make-raw-adj-fn g))
    (var max-dist 0)
    (var complete true)
    (each u srcs
      (def row (bfs-distances g u nbrs))
      (when (< (length row) n) (set complete false))
      (each d (values row)
        (when (> d max-dist) (set max-dist d))))
    (if complete max-dist nil)))


(define graph-radius
 ```Find the radius of graph `g`, the minimum eccentricity across all nodes.

Returns an integer or nil for an empty graph.
```
  [g :any-graph]  :integer-or-nil
  (radius-of-ecc (eccentricities-scan g)))

(define graph-center
  ```Find the center of graph `g`, the nodes with minimum eccentricity.

Returns  an array of node labels.
```
  [g :any-graph]  :array
  (center-of-ecc (eccentricities-scan g)))

(define average-path-length
  "Average shortest path length between all reachable node pairs.
   Uses BFS from each node (unweighted). Streams the sweep; see
   `average-path-length-scan`."
  [g :any-graph]  :number
  (average-path-length-scan g))

(define eccentricity
  "Eccentricity of a vertex: max distance to any other vertex.
   Returns integer distance or nil if graph has 0-1 nodes."
  [g :any-graph u :dynamic] :integer-or-nil
  (eccentricity-of-dists (bfs-distances g u) u))

(define clustering-coefficient
  ```Local clustering coefficient for a node `u` in graph `g`.

Measures how connected a node's neighbours are to each other. For undirected
graphs, the fraction of pairs of neighbours that are connected.
Self-loops are ignored on both ends (a node is not its own neighbour).

Returns a number in [0, 1].
```
  [g :any-graph u :node-label] :number
  # a self-loop must not count as a neighbour
  (def ns (filter (fn [v] (not= v u)) (neighbours g u)))
  (def k (length ns))
  (if (< k 2) 0.0
    (do
      (def neighbour-set (frequencies ns))
      (var connections 0)
      (each a ns
        # needs both ends of the exclusion applied
        (each b (neighbours g a)
          (when (and (not= b a) (get neighbour-set b))
            (++ connections))))
      (/ connections (* k (- k 1))))))


(define average-clustering-coefficient
  ```Find the average local clustering coefficient across all nodes of graph `g`.

Also known as a network's clustering coefficient (Watts-Strogatz).
```
  [g :any-graph]  :number
  (def nodes (nodes g))
  (def n (length nodes))
  (if (= n 0) 0.0
    (/ (sum (seq [u :in nodes] (clustering-coefficient g u))) n)))


# ==============================================================================
#  Type-flexible clustering helpers (undirected semantics)
# ==============================================================================

(defn- _neighbour-sets-via
  ```Build {node: {neighbour: true}} from `neighbours-of` free of self-loops.```
  [g neighbours-of]
  (def sets @{})
  (each u (nodes g)
    (def s @{})
    (each v (neighbours-of u) (put s v true))
    (put sets u s))
  sets)


(define undirected-neighbour-sets
  "Returns table of {node: {neighbour: true}} treating every edge as undirected."
  [g :any-graph] :table
  (def nb (make-undirected-neighbours-fn g))
  (_neighbour-sets-via g (fn [u] (nb g u))))


(define directed-neighbour-sets
  ```Returns table of {node: {neighbour: true}} over the graph's directed neighbours.
Self-loops are dropped.```
  [g :any-graph] :table
  (_neighbour-sets-via g (fn [u]
                           (filter (fn [v] (not= v u))
                                   (neighbours g u)))))


(defn- _count-links
  ```Ordered pairs of `neighbours` that are adjacent in `sets`.
Each triangle among the neighbours is counted twice, once per orientation.
```
  [sets neighbours]
  (var n 0)
  (each a neighbours
    (def na (get sets a))
    (each b neighbours
      (when (and (not= a b) (get na b)) (++ n))))
  n)


(define transitivity-from-sets
 ```Find transitivty from {node: {neighbour: true}} sets.

Returns a number in [0, 1].```
  [sets :table] :number
  (var triangles 0)
  (var triples 0)
  (each [u nu] (pairs sets)
    (def neighbours (keys nu))
    (def k (length neighbours))
    (when (>= k 2)
      (+= triples (* k (- k 1)))
      (+= triangles (_count-links sets neighbours))))
  (if (= triples 0) 0.0
    (/ triangles triples)))


(define global-clustering-coefficient
  ```Find global clustering coefficient (transitivity ratio) of graph `g`.
Computed over the graph's directed neighbours. Self-loops are excluded.

Returns a number in [0, 1].
```
  [g :any-graph]  :number
  (transitivity-from-sets (directed-neighbour-sets g)))


(define local-clustering
  "Local clustering coefficient of node `u` using precomputed neighbour-sets."
  [u :dynamic sets :table] :number
  (let [neighbours (keys (get sets u))
        k (length neighbours)]
    (if (< k 2) 0.0
      (/ (_count-links sets neighbours) (* k (- k 1))))))


(define transitivity-undirected
  "Global clustering coefficient (transitivity ratio) treats graph as undirected"
  [g :any-graph] :number
  (transitivity-from-sets (undirected-neighbour-sets g)))

(define avg-clustering-undirected
  "Average local clustering coefficient across all nodes."
  [g :any-graph] :number
  (def sets (undirected-neighbour-sets g))
  (def nodes (nodes g))
  (def n (length nodes))
  (if (= n 0) 0.0
    (/ (sum (seq [u :in nodes] (local-clustering u sets))) n)))

(define graph-transitivity
  "Transitivity (global clustering coefficient) Returns number in [0, 1]."
  [g :graph] :number
  (transitivity-undirected g))

(define graph-average-clustering
  "Average local clustering coefficient. Returns a number in [0, 1]."
  [g :graph] :number
  (avg-clustering-undirected g))

(define multidigraph-average-clustering
  "Average local clustering coefficient across a multidigraph's nodes,
   treating edges as undirected. Returns a number in [0, 1]."
  [g :multidigraph] :number
  (avg-clustering-undirected g))


# ==============================================================================
#  Cycle and independence invariants
#
#  Computed from the undirected simple structure of `g`.
#  Self-loops are dropped and parallel edges collapsed.
# ==============================================================================

(defn- simple-node-sets
  "{node: {neighbour: true}} sorted for deterministic iteration."
  [g]
  (def nodes (sort (nodes g)))
  (def sets (undirected-neighbour-sets g))
  (def out @{})
  (each u nodes
    (def s @{})
    (each v (keys (get sets u)) (unless (= v u) (put s v true)))
    (put out u s))
  out)

(defn- _lca
  "Lowest common ancestor of u and v in a BFS tree, from the parent and distance arrays."
  [par dist u v]
  (var a u)
  (var b v)
  (while (not= (get dist a) (get dist b))
    (if (> (get dist a) (get dist b))
      (set a (get par a))
      (set b (get par b))))
  (while (not= a b)
    (set a (get par a))
    (set b (get par b)))
  a)

(defn- _arm-to-ancestor
  "The vertices from `u` up to (and including) the ancestor `a`."
  [par u a]
  (def arm @[u])
  (var x u)
  (while (not= x a)
    (set x (get par x))
    (array/push arm x))
  arm)

(defn- _cycle-from-lca
  "The cycle u..lca..v, closed by the edge (u,v), as an array of node names."
  [par u v lca]
  (let [au (_arm-to-ancestor par u lca)
        av (_arm-to-ancestor par v lca)]
    (def down (reverse (slice av 0 (dec (length av)))))
    (array ;(array/concat au down))))


(defn- girth-cycle
  ```A shortest cycle as an array of node names, or nil if the graph is a forest.

One BFS per start vertex, then for every edge (u,v), length is given by

       dist(u) + dist(v) + 1 - 2*dist(lca(u,v))

  - (u,v) with v the BFS parent of u is a tree edge. It doesn't close a cycle,
    and the formula degenerates (lca is one of the endpoints). skipped.
  - an edge with either endpoint that is unreachable from s is skipped.
```
  [sets]
  (let [vs (keys sets)
        n (length vs)
        idx (invert vs)
        adj (map (fn [u] (map (fn [v] (get idx v)) (keys (get sets u)))) vs)]
    # every undirected edge once, as a pair of indices, lower first
    (def es @[])
    (loop [u :range [0 n]]
      (each v (adj u)
        (when (> u v) (array/push es @[v u]))))
    (var best nil)
    (var best-cycle nil)
    (each s (range n)
      (def dist (array/new-filled n -1))
      (def par (array/new-filled n -1))
      (put dist s 0)
      (def q @[s])
      (var qi 0)
      (while (< qi (length q))
        (def u (q qi))
        (++ qi)
        (each v (adj u)
          (when (= (get dist v) -1)
            (put dist v (+ 1 (get dist u)))
            (put par v u)
            (array/push q v))))
      (each e es
        (def u (e 0))
        (def v (e 1))
        # skip tree edges or edges reaching an unreached vertex.
        (def usable (and (not= (get par v) u)
                         (not= (get par u) v)
                         (>= (get dist u) 0)
                         (>= (get dist v) 0)))
        (when usable
          (def lca (_lca par dist u v))
          (def len (+ (get dist u) (get dist v) (- (* 2 (get dist lca))) 1))
          (when (or (nil? best) (< len best))
            (set best len)
            (set best-cycle (_cycle-from-lca par u v lca))))))
    (if best-cycle (map (fn [i] (vs i)) best-cycle) nil)))


(define girth
  "Length of a shortest cycle, or nil if the graph has none (e.g. a forest)."
  [g :any-graph] :integer-or-nil
  (let [sets (simple-node-sets g)
        cyc (girth-cycle sets)]
    (if (nil? cyc) nil (length cyc))))


(define shortest-cycle
  ```A shortest cycle of `g`.

Returns array of node names, or nil if no cycle exists.```
  [g :any-graph] :array-or-nil
  (girth-cycle (simple-node-sets g)))

(define triangle-count
  ```Number of triangles (3-cycles) in the undirected simple structure of `g`.```
  [g :any-graph] :integer
  (def sets (simple-node-sets g))
  (var total 0)
  (each [u su] (pairs sets)
    (each v (keys su)
      (when (> u v)
        (def sv (get sets v))
        (each w (keys su)
          (when (and (not= w u) (not= w v) (get sv w))
            (++ total))))))
  (/ total 3))


(define cubic?
  "True when every vertex of `g` has degree 3."
  [g :any-graph] :boolean
  (def nodes (nodes g))
  (all (fn [u] (= 3 (length (neighbours g u)))) nodes))

(define bipartite?
  ```True when graph `g` is bipartite.

A graph is bipartite if nodes can be split into two sets where every edge
crosses the split. A bipartite graph has no odd cycle, so a two-colouring by
BFS can be used to check that no edge joins two nodes of the same colour.

This is a predicate form. Use `bipartite-sets` to return the two sides.
```
  [g :any-graph] :boolean
  (def nodes (nodes g))
  (if (< (length nodes) 2) true
    (do
      (def sets (simple-node-sets g))
      (def side @{})
      (var ok true)
      (each s nodes
        (unless (has-key? side s)
          (put side s 0)
          (var qi 0)
          (def q @[s])
          (while (< qi (length q))
            (def u (q qi))
            (++ qi)
            (each v (keys (get sets u))
              (cond
                (not (has-key? side v)) (do (put side v (- 1 (get side u)))
                                            (array/push q v))
                (= (get side v) (get side u)) (set ok false)))))
        (when (not ok) (break)))
      ok)))

(defn- _max-independent-set
  ```Find the non-isolated part of a maximum independent set as an array.

Uses branch and bound over vertices in increasing degree order. Isolated
vertices are dropped and should be added back by the caller, they are in
every maximum set and never conflict.

Exact, but NP-hard in general. Use for small graphs.
```
  [sets]
  (let [all (keys sets)
        iso (filter (fn [u] (= (length (get sets u)) 0)) all)]
    # lowest degree first (use name as tiebreak for deterministic result)
    (def cand (sort (filter
                     (fn [u] (> (length (get sets u)) 0)) all)
                    (fn [a b] (< (length (get sets a)) (length (get sets b))))))
    (def m (length cand))
    (def cidx (invert cand))
    # the candidates adjacent to each candidate
    (def nbrs (map (fn [u]
                     (map (fn [v] (get cidx v)) (keys (get sets u)))) cand))
    # a candidate is only taken when `barred` count is 0.
    (def barred (array/new-filled m 0))
    (var best 0)
    (var best-set @[])
    (def chosen @[])
    (defn grow [i]
      # bound
      (when (> (+ (length chosen) (- m i)) best)
        (if (>= i m)
          (do
            (when (> (length chosen) best)
              (set best (length chosen))
              (set best-set (array ;chosen))))
          (do
            (if (> (get barred i) 0)
              (grow (+ i 1))
              (do
                # branch 1
                (each j (get nbrs i) (put barred j (+ 1 (get barred j))))
                (array/push chosen (cand i))
                (grow (+ i 1))
                (array/pop chosen)
                # branch 2
                (each j (get nbrs i) (put barred j (- (get barred j) 1)))
                (grow (+ i 1))))))))
    (grow 0)
    (array ;(array/concat iso best-set))))


(define maximum-independent-set
  "A maximum independent set as an array of node names."
  [g :any-graph] :array
  (sort (_max-independent-set (simple-node-sets g))))

(define independence-number
  "Size of a maximum independent set, i.e. alpha(G)."
  [g :any-graph] :integer
  (length (maximum-independent-set g)))

(define minimum-vertex-cover
  "Return a minimum vertex cover as array of node labels."
  [g :any-graph] :array
  (let [sets (simple-node-sets g)
        ind (_max-independent-set sets)
        in-set @{}]
    (each u ind (put in-set u true))
    (filter (fn [u] (not (get in-set u))) (keys sets))))

(define vertex-cover-number
  "Size of a minimum vertex cover, i.e. tau(G) = |V| - alpha(G)."
  [g :any-graph] :integer
  (- (length (nodes g)) (independence-number g)))


(defn- _det
  ```Find the determinant of a square numeric array.
Uses Gaussian elimination with partial pivoting.

Returns 0 for a singular matrix, destroys `a`.
```
  [a]
  (def n (length a))
  (var det 1.0)
  (var singular false)
  (loop [c :range [0 n]]
    (unless singular
      # pivot using the largest value in the column at or below the diagonal
      (var piv c)
      (var best (math/abs (get (a c) c)))
      (loop [r :range [(+ c 1) n]]
        (def v (math/abs (get (a r) c)))
        (when (> v best)
          (set best v)
          (set piv r)))
      # a zero pivot implies remaining block is singular
      (if (= best 0.0)
        (set singular true)
        (do
          (when (not= piv c)
            (def tmp (get a c))
            (put a c (get a piv))
            (put a piv tmp)
            (set det (- det)))
          (def d (get (a c) c))
          (*= det d)
          (loop [r :range [(+ c 1) n]]
            (def f (/ (get (a r) c) d))
            (when (not= f 0.0)
              (loop [k :range [c n]]
                (put (a r) k (- (get (a r) k) (* f (get (a c) k)))))))))))
  (if singular 0.0 det))


(define spanning-tree-count
  ```Number of spanning trees of an undirected graph, via Matrix-Tree theorem.

The count is integral by definition but returned as a number since :integer cast
rejects anything above 2^31-1. `math/round` is used for an exact count.

e.g. K_{2,28} has 3758096384 spanning trees and J_7 has 3626421435.
```
  [g :any-graph] :number
  (let [sets (simple-node-sets g)
        vs (keys sets)
        n (length vs)]
    (if (< n 1) 1
      (do
        (def idx (invert vs))
        (def m (dec n))
        # Laplacian cofactor. Each edge is walked once (with i > j)
        (def lap (map (fn [_] (array/new-filled m 0.0)) (range m)))
        (loop [i :range [0 n]]
          (each v (keys (get sets (vs i)))
            (def j (get idx v))
            (when (> i j)
              (cond
                (and (< i m) (< j m))
                (do
                  (put (lap i) i (+ 1.0 (get (lap i) i)))
                  (put (lap j) j (+ 1.0 (get (lap j) j)))
                  (put (lap i) j -1.0)
                  (put (lap j) i -1.0))
                (< i m) (put (lap i) i (+ 1.0 (get (lap i) i)))
                (< j m) (put (lap j) j (+ 1.0 (get (lap j) j)))))))
        (math/round (math/abs (_det lap)))))))


(define toughness
  ```Find toughness of graph `g`.

Toughness is defined as t(G) = min |S| / c(G - S) over vertex cuts S that
disconnect G, where c(H) is the number of components of H.
Complete graphs have no disconnecting cut and return +inf.

Exponential. Not suitable as a large-graph metric.
```
  [g :any-graph] :number
  (let [sets (simple-node-sets g)
        vs (keys sets)
        n (length vs)
        idx (invert vs)
        adj (map (fn [u] (map (fn [v] (get idx v)) (keys (get sets u)))) vs)]
    (def connected?
      (fn []
        (if (< n 1) true
          (do
            (def seen (array/new-filled n false))
            (put seen 0 true)
            (var count 1)
            (def q @[0])
            (var qi 0)
            (while (< qi (length q))
              (def u (q qi))
              (++ qi)
              (each v (adj u)
                (unless (get seen v)
                  (put seen v true)
                  (++ count)
                  (array/push q v))))
            (= count n)))))
    # complete and connected -> no cut disconnects
    (var complete true)
    (each u vs
      (unless (= (length (get sets u)) (dec n)) (set complete false)))
    (if (connected?)
      (if complete math/inf
        (do
          (var best math/inf)
          (var mask 1)
          (var limit (blshift 1 n))
          (while (< mask limit)
            (var popcount 0)
            (loop [i :range [0 n]]
              (when (not= (band mask (blshift 1 i)) 0) (++ popcount)))
            (when (and (>= popcount 1) (<= popcount (- n 2)))
              (def removed (array/new-filled n false))
              (loop [i :range [0 n]]
                (when (not= (band mask (blshift 1 i)) 0) (put removed i true)))
              (def seen (array/new-filled n false))
              (var comps 0)
              (loop [v :range [0 n]]
                (unless (get removed v)
                  (unless (get seen v)
                    (++ comps)
                    (put seen v true)
                    (def q @[v])
                    (var qi 0)
                    (while (< qi (length q))
                      (def u (q qi))
                      (++ qi)
                      (each w (adj u)
                        (unless (or (get removed w) (get seen w))
                          (put seen w true)
                          (array/push q w)))))))
              (when (> comps 1)
                (set best (min best (/ popcount comps)))))
            (++ mask))
          best))
      0.0)))
