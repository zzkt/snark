# -*- mode: janet; -*-
# graphs/colouring: vertex colouring and Turán numbers

#  Greedy (Welsh–Powell) colouring, the exact chromatic number by branch and
#  bound. Turán graphs and Turán numbers for K_{r+1}-free extremal graphs.
#
#  Turán's theorem: A graph on n vertices with more than (1-1/r)·n²/2 edges
#  contains a K_{r+1} as a subgraph.
#
#  Reference: Diestel, 2025. Graph Theory, Ch.5

(import deft :prefix "")
(import ./types :prefix "")
(import ./util :prefix "")
(import ./simple-graph :prefix "")


# ==============================================================================
#  Greedy colouring
# ==============================================================================

(defn- degree-order
  ```The nodes of `g`, highest degree first.

Welsh-Powell colouring and branch & bound both visit the most constrained vertex
first to prune the search. The order must remain consistent.```
  [g]
  (def deg @{})
  (each u (nodes g) (put deg u (length (neighbours g u))))
  (sort (array/slice (nodes g)) (fn [a b] (> (get deg a) (get deg b)))))


(defn- used-colours
  ```The set of colours used by the neighbours of `u` for the partial
colouring `col`. Vertices not yet coloured are nil.```
  [g col u]
  (def used @{})
  (each v (neighbours g u)
    (def cv (get col v))
    (when cv (put used cv true)))
  used)


(define greedy-colour
  ```Proper vertex colouring of `g` using the greedy Welsh-Powell algorithm.

Vertices are processed in decreasing degree order with each given the first
available colour (indexed from 1) not used by any of its neighbours.

Returns a table {node colour} using at most Δ(g)+1 colours.
```
  [g :any-graph] :table
  (def col @{})
  (each u (degree-order g)
    (def used (used-colours g col u))
    (var c 1)
    (while (get used c) (++ c))
    (put col u c))
  col)

(define proper-edge-colour?
  "True if `col` is a proper edge colour of graph `g`."
  [g col] :boolean
  (var ok true)
  (each u (graph-nodes g)
    (each v (graph-neighbours g u)
      (when (not= u v)
        (def k (if (< (string u) (string v)) (tuple u v) (tuple v u)))
        (when (nil? (get col k)) (set ok false)))))
  (when ok
    (each u (graph-nodes g)
      (def seen @{})
      (each v (graph-neighbours g u)
        (when (not= u v)
          (def k (if (< (string u) (string v)) (tuple u v) (tuple v u)))
          (def c (get col k))
          (when (get seen c) (set ok false))
          (put seen c true)))))
  ok)


(define proper-colouring?
  ```True if `vc` is a proper vertex colouring of `g`. i.e. Every vertex is
assigned a colour and all adjacent vertices have different colours.```
  [g :any-graph vc :table] :boolean
  (var ok true)
  (each u (nodes g)
    (when (nil? (get vc u)) (set ok false)))
  (when ok
    (each u (nodes g)
      (each v (neighbours g u)
        (when (= (get vc u) (get vc v))
          (set ok false)))))
  ok)


# ==============================================================================
#  Exact chromatic number
# ==============================================================================

(define chromatic-number
  ```Chromatic number of the graph `g`.

A greedy colouring provides the initial upper bound, vertices are then coloured
one at a time in decreasing degree order, pruning any partial colouring that
needs `best` colours. Exact but exponential in the worst case.

Returns 0 for an empty graph.
```
  [g :any-graph] :number
  (def n (length (nodes g)))
  (cond
    (= n 0) 0
    (= n 1) 1
    (do
      (var best (apply max (values (greedy-colour g))))
      (def col @{})
      (def order (degree-order g))
      (defn search [i k]
        (if (= i n)
          (set best k)
          (when (< k best)
            (def u (order i))
            (def used (used-colours g col u))
            (for c 1 (+ k 2)
              (when (and (< c best) (not (get used c)))
                (put col u c)
                (search (+ i 1) (max k c))
                (put col u nil))))))
      (search 0 0)
      best)))


(define vertex-colouring
  ```Find a proper vertex colouring of `g` with at most `k` colours.

Uses the same search as `chromatic-number` but stops at the specified `k` rather
than searching for a minimum. Vertices are processed in decreasing degree order
for best pruning unless a custom `order` is provided.

Returns {node colour} if a k-colouring exists, or nil.
```
  [g :any-graph k :number &opt order] :table-or-nil
  (def n (length (nodes g)))
  (cond
    (= n 0) @{}
    (= n 1) @{(first (nodes g)) 1}
    (do
      (def col @{})
      (def order (or order (degree-order g)))
      (defn search [i]
        (if (= i n)
          true
          (do
            (def u (order i))
            (def used (used-colours g col u))
            (var ok false)
            (for c 1 (+ k 1)
              (when (and (not ok) (not (get used c)))
                (put col u c)
                (when (search (+ i 1)) (set ok true))
                (when (not ok) (put col u nil))))
            ok)))
      (when (search 0) col))))


# ==============================================================================
#  Turán graphs and the Turán number
# ==============================================================================

(define make-turan-graph
  ```Complete equitable r-partite graph on `n` vertices (the Turán graph).

The vertices 0 … n-1 are split into `r` parts as evenly as possible and every
pair of vertices in different parts is joined.

T(n, r) is K_{r+1}-free and attains the Turán number ex(n, K_{r+1}).
```
  [n :number r :number] :graph
  (when (or (< r 1) (> r n))
    (error "make-turan-graph: requires 1 ≤ r ≤ n"))
  (def g (make-graph))
  (def parts @[])
  (for i 0 r (array/push parts @[]))
  (for i 0 n (array/push (parts (mod i r)) i))
  (each part parts (each u part (graph-add-node g u)))
  (for i 0 r
    (for j (+ i 1) r
      (each u (parts i)
        (each v (parts j)
          (graph-add-edge g u v)))))
  g)


(define turan-number
  ```Maximum number of edges in a K_{r+1}-free graph on `n` vertices.

i.e. ex(n, K_{r+1}) = (1-1/r)·n²/2 rounded down to the complete equitable
r-partite graph count. By Turán's theorem a graph on n vertices with more
edges than this always contains a K_{r+1}.
```
  [n :number r :number] :number
  (let [a (div n r)
        b (% n r)]
    (- (div (* n (- n 1)) 2)
       (+ (div (* (- r b) a (- a 1)) 2)
          (div (* b (+ a 1) a) 2)))))


# ==============================================================================
#  Edge colouring / chromatic index (Vizing)
# ==============================================================================

# The canonical unordered edge key is `canonical-edge-key` from ./util

(defn- edge-list
  "Return @[[u v] ...] of unique non-loop edges of g, canonical orientation."
  [g]
  (let [out @[]
        seen @{}]
    (each u (nodes g)
      (each v (neighbours g u)
        (def key (canonical-edge-key u v))
        (when (and (not= u v) (nil? (get seen key)))
          (put seen key true)
          (array/push out @[u v]))))
    out))

(defn- edge-incident?
  "True when edge a and edge b share an endpoint."
  [a b]
  (or (= (b 0) (a 0)) (= (b 1) (a 0))
      (= (b 0) (a 1)) (= (b 1) (a 1))))

(defn- incident-colours
  ```The colours already used in the partial edge colouring `col` by the edges
that share an endpoint with `e`.```
  [col e]
  (def used @{})
  (each [ek ck] (pairs col)
    (when (edge-incident? ek e)
      (put used ck true)))
  used)

(defn- max-degree
  "Maximum vertex degree Δ of g."
  [g]
  (apply max 0 (map (fn [u] (length (neighbours g u))) (nodes g))))


(define edge-colour-greedy
  ```Proper edge colouring.

Edges are processed in order of decreasing endpoint degree. Each edge gets the
smallest colour (counting from 1) unused by any edge sharing an endpoint.
Uses at most 2Δ-1 colours, where Δ is the maximum degree. Self-loops are ignored.

Returns a table {(u v) colour} with canonical unordered keys
```
  [g :any-graph] :table
  (let [edges (edge-list g)
        degree-map @{}]
    (each u (nodes g) (put degree-map u (length (neighbours g u))))
    (def ordered
      (sort edges (fn [a b]
        (> (max (get degree-map (a 0)) (get degree-map (a 1)))
           (max (get degree-map (b 0)) (get degree-map (b 1)))))))
    (def col @{})
    (each e ordered
      (def key (canonical-edge-key (e 0) (e 1)))
      (def used (incident-colours col e))
      (var c 1)
      (while (get used c) (++ c))
      (put col key c))
    col))


(define vizing-edge-colour
  ```Vizing edge colouring. Chromatic index classifier.

By Vizing's theorem, every simple graph has χ′(G) ∈ {Δ, Δ+1}, where Δ is the
maximum degree. The search finds a proper edge colouring with exactly χ′ colours
by exhaustive search over the line graph, then classifies into 2 classes.

- class 1: a Δ-colouring exists (χ′ = Δ)
- class 2: only a Δ+1-colouring exists (χ′ = Δ + 1)

Exact but exponential in the worst case (χ′ is NP-hard to decide in general).

Returns a table @{:colouring (edge colour) :uses k :delta Δ :class 1|2}.
```
  [g :any-graph] :table
  (let [edges (edge-list g)
        delta (max-degree g)
        n-edges (length edges)
        result (table)]
    (put result :delta delta)
    (put result :class 1)
    (put result :uses 0)
    (put result :colouring @{})
    (cond
      (= n-edges 0)
      (do (put result :uses 0) (put result :class 1) result)
      (do
        # backtracking search for a proper k-colouring of the line graph
        (defn k-colourable? [k]
          (def col @{})
          (defn dfs [i]
            (if (= i n-edges)
              true
              (do
                (def e (edges i))
                (def key (canonical-edge-key (e 0) (e 1)))
                (def used (incident-colours col e))
                (var ok false)
                (for c 1 (+ k 1)
                  (when (and (not ok) (not (get used c)))
                    (put col key c)
                    (when (dfs (+ i 1)) (set ok true))
                    (when (not ok) (put col key nil))))
                ok)))
          (when (dfs 0) col))
        (def lower (k-colourable? delta))
        (if lower
          (do (put result :uses delta)
              (put result :class 1)
              (put result :colouring lower)
              result)
          (do (put result :uses (+ delta 1))
              (put result :class 2)
              (put result :colouring (k-colourable? (+ delta 1)))
              result))))))


(define chromatic-index
  ```Chromatic index χ′(G), the minimum number of edge colours.

By Vizing's theorem χ′ ∈ {Δ, Δ+1} for simple graphs.
Exact but exponential in the worst case.```
  [g :any-graph] :number
  (get (vizing-edge-colour g) :uses))

# vocabulary alias
(def edge-chromatic-number chromatic-index)
