# -*- mode: janet; -*-
# graphs/matching: matching algorithms & clique enumeration

# Maximal/perfect matching predicates, greedy maximal matching, and clique
# enumeration (Bron-Kerbosch). Works on undirected :graph and :multigraphs

(import deft :prefix "")
(import ./types :prefix "")
(import ./util :prefix "")


# ==============================================================================
#  Matching
# ==============================================================================

(defn- matched-nodes
  "Set of all endpoints across the given edge list."
  [edges]
  (def used @{})
  (each e edges
    (put used (e 0) true)
    (put used (e 1) true))
  used)

(define is-matching?
  "True when an array of [u v] edge pairs has no two edges that share an endpoint."
  [g :any-graph edges :array] :boolean
  (def used @{})
  (var ok true)
  (each e edges
    (when ok
      (def u (e 0))
      (def v (e 1))
      (when (or (get used u) (get used v) (not (has-edge? g u v)))
        (set ok false))
      (when ok
        (put used u true)
        (put used v true))))
  ok)


(define is-maximal-matching?
  "True when the matching cannot be extended by any unused edge."
  [g :any-graph edges :array] :boolean
  (if (not (is-matching? g edges))
    false
    (do
      (def used (matched-nodes edges))
      (var extendable false)
      (each u (nodes g)
        (when (not extendable)
          (each v (neighbours g u)
            (when (and (not (get used u))
                       (not (get used v))
                       (not= u v))
              (set extendable true)))))
      (not extendable))))


(define is-perfect-matching?
  ```True when every vertex of the graph is an endpoint of the matching.
Requires an even number of vertices.```
  [g :any-graph edges :array] :boolean
  (if (not (is-matching? g edges))
    false
    (do
      (def used (matched-nodes edges))
      (var ok true)
      (each u (nodes g)
        (when (not (get used u))
          (set ok false)))
      ok)))

(define maximal-matching
 ```Find a maximal matching as an array of [u v] pairs.
Scans vertices, matching each free vertex with the first free neighbour.
```
  [g :any-graph] :array
  (let [used @{}
        result @[]]
    (each u (nodes g)
      (when (not (get used u))
        (var matched nil)
        (each v (neighbours g u)
          (when (and (nil? matched) (not (get used v)) (not= u v))
            (set matched v)))
        (when (not (nil? matched))
          (array/push result @[u matched])
          (put used u true)
          (put used matched true))))
    result))


# ==============================================================================
#  Clique enumeration (Bron-Kerbosch)
# ==============================================================================

(defn- bron-kerbosch
  ```Bron-Kerbosch search.

 `r` is the current clique
 `p` is a candidate table
 `x` is the excluded table.

Accumulates maximal cliques into `cliques`.
```
  [g r p x cliques]
  (if (and (empty? p) (empty? x))
    (array/push cliques (array/slice r))
    (do
      # pivot on a vertex in p or x
      # only recurse on non-neighbours of the pivot
      (def pk (keys p))
      (def all-px (array/concat (array/slice pk) (keys x)))
      (def pivot (if (empty? all-px) nil (all-px 0)))
      (def cands
        (if (nil? pivot)
          pk
          (seq [v :in pk :when (not (has-edge? g v pivot))] v)))
      (each v cands
        (def newp @{})
        (each w pk
          (when (has-edge? g v w) (put newp w true)))
        (def newx @{})
        (each w (keys x)
          (when (has-edge? g v w) (put newx w true)))
        (array/push r v)
        (bron-kerbosch g r newp newx cliques)
        (array/pop r)
        (put p v nil)
        (put x v true)))))

(defn- _clique-set-key
  "Safe key for a clique's node set. A nested struct path from sorted node labels."
  [nodes]
  (def s (sort (array/slice nodes)))
  (var acc true)
  (each n (reverse s)
    (set acc (struct n acc)))
  acc)

(define find-cliques
  ```Find all maximal cliques of a graph.

Uses Bron-Kerbosch with pivoting and works on any undirected graph type.
Search can find the same max clique more than once, results are deduplicated.

Returns an array of arrays of node labels.
```
  [g :any-graph] :array
  (let [cliques @[]
        p @{}]
    (each n (nodes g) (put p n true))
    (bron-kerbosch g @[] p @{} cliques)
    (def seen @{})
    (def uniq @[])
    (each c cliques
      (def key (_clique-set-key c))
      (unless (get seen key)
        (put seen key true)
        (array/push uniq c)))
    uniq))


(define find-all-cliques
   ```Find all cliques of a graph.

Finds all cliques (not just maximal), including empty set and all singletons.

Returns an array of arrays of node labels.
```
  [g :any-graph] :array
  (let [maximal (find-cliques g)
        allc @[@[]]
        seen @{}]
    (each c maximal
      (def n (length c))
      # enumerate all non-empty subsets of c
      (defn enum-subs [start cur]
        (for i start n
          (array/push cur (c i))
          (def key (_clique-set-key cur))
          (when (nil? (get seen key))
            (put seen key true)
            (array/push allc (array/slice cur)))
          (enum-subs (+ i 1) cur)
          (array/pop cur)))
      (enum-subs 0 @[]))
    allc))


(define node-clique-number
  "Size of the largest clique containing vertex `u`."
  [g :any-graph u :dynamic] :number
  (var best 0)
  (each c (find-cliques g)
    (when (arr-contains? c u)
      (set best (max best (length c)))))
  best)

(define number-of-cliques
  "Count of all cliques (including empty set and singletons) in graph `g`."
  [g :any-graph] :number
  (length (find-all-cliques g)))


# ==============================================================================
#  Bipartite graphs
# ==============================================================================

(define bipartite-sets
  ```Split a bipartite graph `g` into two sides.

Create two sets covering every vertex of the bipartite graph `g`.
Disconnected graphs are handled component by component.

Returns table {:x [nodes...] :y [nodes...]} or nil if graph is not bipartite.
```
  [g :any-graph] [:or :table :nil]
  (def color @{})
  (var ok true)
  (defn bfs-color [start]
    (put color start 0)
    (var stack @[start])
    (while (and ok (> (length stack) 0))
      (def u (last stack))
      (array/pop stack)
      (def c (get color u))
      (each v (neighbours g u)
        (def cv (get color v))
        (if (nil? cv)
          (do (put color v (- 1 c)) (array/push stack v))
          (when (= cv c) (set ok false))))))
  (each u (nodes g)
    (when (and ok (nil? (get color u))) (bfs-color u)))
  (if ok
    @{:x (seq [u :in (nodes g) :when (= 0 (get color u))] u)
      :y (seq [u :in (nodes g) :when (= 1 (get color u))] u)}
    nil))


(define maximum-bipartite-matching
  ```Maximum cardinality matching of a bipartite graph.

Uses Kuhn's augmenting-path algorithm where `x` and `y` are the two sides,
the number of pairs equals min(|x|, |y|) when the input is bipartite with a
saturating matching.

Existence of an x-saturating matching is found by Hall's condition.

Returns an array of [x y] matched pairs.
```
  [g :any-graph x :array y :array] :array
  (def match-to-x @{})
  (defn try-augment [u visited]
    (var found false)
    (each v (neighbours g u)
      (when (and (not found) (arr-contains? y v) (not (get visited v)))
        (put visited v true)
        (def prev (get match-to-x v))
        (when (or (nil? prev) (try-augment prev visited))
          (put match-to-x v u)
          (set found true))))
    found)
  (each u x
    (try-augment u @{}))
  (seq [[v u] :pairs match-to-x] @[u v]))


(define bipartite-perfect-matching?
  "True when the bipartite graph (x, y) has a perfect matching.
i.e. |x| = |y| and has a matching covering all vertices of both sides."
  [g :any-graph x :array y :array] :boolean
  (and (= (length x) (length y))
       (= (length (maximum-bipartite-matching g x y)) (length x))))


(define satisfies-hall-condition?
  ```True when the Hall condition holds.

For the bipartite graph with sides x and y, for every subset A of x,
|N(A)| ≥ |A|, where N(A) is the set of neighbours of A lying in y.

By Hall's marriage theorem, this is equivalent to the existence of an
x-saturating matching, which can be decided by using maximum-bipartite-matching
to check if there is a matching that covers all of x.
```
  [g :any-graph x :array y :array] :boolean
  (= (length (maximum-bipartite-matching g x y)) (length x)))


# ==============================================================================
#  Vertex cover & independent set (König duality)
#
#  By König's theorem, in a bipartite graph the minimum vertex cover has the
#  same size as the maximum matching. The cover is obtained from a maximum
#  matching by the alternating-reach construction, the maximum independent set
#  is its complement.
# ==============================================================================

(defn- koenig-cover
 ``` Minimum vertex cover of bipartite graph.

Using alternating BFS from unmatched x vertices, the cover is
 (X - Zx) ∪ (Y ∩ Zy) where Zx, Zy are the reached sides.

Returns (x y) from the maximum matching array `pairs`.
```
  [g x y pairs]
  # y -> matched x partner
  (def match-x @{})
  (each p pairs (put match-x (p 1) (p 0)))
  (def zx @{})
  (def zy @{})
  (var frontier @[])
  # seed with unmatched x-vertices
  (each u x
    (var saturated false)
    (each p pairs (when (= (p 0) u) (set saturated true)))
    (unless saturated
      (put zx u true)
      (array/push frontier u)))
  (while (> (length frontier) 0)
    (def next-f @[])
    (each u frontier
      # alternate x -> y along unmatched edges
      (each v (neighbours g u)
        (when (and (arr-contains? y v) (nil? (get zy v)))
          (put zy v true)
          (def mp (get match-x v))
          (when (and mp (nil? (get zx mp)))
            (put zx mp true)
            (array/push next-f mp)))))
    (set frontier next-f))
  (def cover @{})
  (each u x (unless (get zx u) (put cover u true)))
  (each v y (when (get zy v) (put cover v true)))
  (keys cover))

(define minimum-vertex-cover
  ```Minimum vertex cover of a bipartite graph `g`.
`x` and `y` are the two sides (as bipartite-sets).```
  [g :any-graph x :array y :array] :array
  (koenig-cover g x y (maximum-bipartite-matching g x y)))

(define maximum-independent-set
  ```Maximum independent set of a bipartite graph `g`.```
  [g :any-graph x :array y :array] :array
  (def cover (to-set (minimum-vertex-cover g x y)))
  (seq [u :in (nodes g) :unless (get cover u)] u))


# ==============================================================================
#  General (non-bipartite) maximum matching (Edmonds' blossom)
#
#  Edmonds' blossom algorithm is used to find a maximum matching in any graph.
#  Odd cycles that obstruct a naive alternating-path search are contracted
#  into "blossoms" (treated as single vertices) and implicitly re-expanded.
#
#  Generalizes Kuhn's algorithm on bipartite graphs. O(V³) time.
# ==============================================================================

(defn- blossom-build
  ```Index the nodes of graph `g`.

Returns array of [node-list adj] where adj is an array of arrays of neighbours.
```
  [g]
  (let [ns (array/slice (nodes g))
        idx @{}]
    (each n ns (put idx n (length idx)))
    (def adj (seq [_ :range [0 (length ns)]] @[]))
    (each u ns
      (def seen @{})
      (each v (neighbours g u)
        (when (and (not= u v) (nil? (get seen (idx v))))
          (put seen (idx v) true)
          (array/push (adj (idx u)) (idx v)))))
    @[ns adj]))

(defn- blossom-match
  ```Maximum matching on an integer-indexed adjacency list `adj` with `n` nodes.

Returns an array `match-partner` of length n
with match-partner[v] the partner of v, or -1 if v is unmatched.
```
  [adj n]
  (let [match-partner (array/new-filled n -1)
        p (array/new-filled n -1)
        base (array/new-filled n 0)
        used (array/new-filled n false)
        bloom (array/new-filled n false)]
    (defn lca [a b]
      # LCA of the alternating trees of a and b
      (var a a)
      (var b b)
      (array/fill used false)
      (while true
        (set a (base a))
        (put used a true)
        (when (= (match-partner a) -1) (break))
        (set a (p (match-partner a))))
      (while true
        (set b (base b))
        (when (get used b) (break))
        (set b (p (match-partner b))))
      b)
    (defn mark-path [v b children]
      # mark the blossom cycle and set parent pointers
      (var v v)
      (var children children)
      (while (not= (base v) b)
        (put bloom (base v) true)
        (put bloom (base (match-partner v)) true)
        (put p v children)
        (set children (match-partner v))
        (set v (p (match-partner v)))))
    (defn find-path [root]
      # BFS of the alternating forest from root
      (array/fill used false)
      (array/fill p -1)
      (for i 0 n (put base i i))
      (def q @[root])
      (var qi 0)
      (var found -1)
      (while (and (< qi (length q)) (= found -1))
        (def v (q qi))
        (++ qi)
        (each u (adj v)
          (when (= found -1)
            (if (or (= (base v) (base u))
                    (= (match-partner v) u))
              nil
              (if (or (= u root)
                      (and (not= (match-partner u) -1)
                           (not= (p (match-partner u)) -1)))
                (do
                  # u is an outer vertex: a blossom was found, contract it
                  (def curbase (lca v u))
                  (array/fill bloom false)
                  (mark-path v curbase u)
                  (mark-path u curbase v)
                  (for i 0 n
                    (when (get bloom (base i))
                      (put base i curbase)
                      (unless (get used i)
                        (put used i true)
                        (array/push q i)))))
                (if (= (p u) -1)
                  (do
                    # grow the forest: move u into an inner vertex
                    (put p u v)
                    (if (= (match-partner u) -1)
                      (set found u)
                      (do
                        (def uu (match-partner u))
                        (put used uu true)
                        (array/push q uu))))
                  nil))))))
      found)
    # grow from every free vertex
    (for i 0 n
      (when (= (match-partner i) -1)
        (var v (find-path i))
        (while (not= v -1)
          (def pv (p v))
          (def ppv (match-partner pv))
          (put match-partner v pv)
          (put match-partner pv v)
          (set v ppv))))
    match-partner))


(define maximum-matching
  ```Maximum cardinality matching of a graph `g` (bipartite or not).

Uses Edmonds' blossom algorithm. If side-sensitivity is required for a bipartite
graph, use `maximum-bipartite-matching`.

Returns an array of [u v] matched pairs. If the graph has a perfect matching
every vertex appears exactly once.
```
  [g :any-graph] :array
  (let [built (blossom-build g)
        ns (built 0)
        mp (blossom-match (built 1) (length ns))
        result @[]
        seen @{}]
    (each i (range (length ns))
      (def j (mp i))
      (when (and (not= j -1) (nil? (get seen i)))
        (put seen i true)
        (put seen j true)
        (array/push result @[(ns i) (ns j)])))
    result))


(define is-perfect-matching-graph?
  ```True when the graph `g` admits a perfect matching.
```
  [g :any-graph] :boolean
  (def ns (nodes g))
  (and (= 0 (mod (length ns) 2)) # odd vertex counts are never perfect
       (= (length (maximum-matching g))
          (/ (length ns) 2))))
