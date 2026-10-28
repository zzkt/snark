# -*- mode: janet; -*-
# graphs/tournament: tournament digraphs

#  Tournaments are complete asymmetric digraphs. Every pair of distinct
#  vertices is joined by exactly one arc.
#
#   - every tournament has a Hamiltonian path (Easy Rédei theorem), and the
#     number of Hamiltonian paths is odd (Hard Rédei theorem)
#   - a non-acyclic tournament has a directed 3-cycle
#   - Landau's theorem characterises tournament score sequences
#   - every vertex of maximum out-degree is a king and a tournament has at
#     least one king
#
#  References
#  - Landau (1953), On dominance relations and the structure of animal societies.
#  - Maurer (1980), The king chicken theorems.

(import deft :prefix "")
(import ./types :prefix "")
(import ./util :prefix "")
(import ./traversal :prefix "")


# ==============================================================================
#  Recognition & basic invariants
# ==============================================================================

(define is-tournament?
  ```True when the graph `g` is a tournament.

Every unordered pair of distinct vertices is joined by exactly one arc, a
loopless tournament on n vertices has n(n-1)/2 arcs.
```
  [g :digraph] :boolean
  (def ns (nodes g))
  (var ok true)
  (for i 0 (length ns)
    (when ok
      (for j (+ i 1) (length ns)
        (def u (ns i))
        (def v (ns j))
        (when (= (has-edge? g u v) (has-edge? g v u))
          (set ok false)))))
  ok)

(define tournament-matrix
  "An n×n adjacency matrix with a 1 in position (i, j) when there is an arc from
the i-th to the j-th node of graph `g` and 0 on the diagonal."
  [g :digraph] :array
  (def ns (nodes g))
  (seq [u :in ns]
    (seq [v :in ns]
      (if (and (not= u v) (has-edge? g u v)) 1 0))))

(define tournament-score-sequence
  "Out-degrees of a tournament sorted into non-decreasing order."
  [g :digraph] :array
  (sort (seq [u :in (nodes g)] (length (neighbours g u)))))

(define valid-tournament-score-sequence?
  ```True when non-decreasing array `b` is the score sequence of a tournament.

By Landau's theorem, the sum of the k smallest entries is at least C(k, 2) for
every k, with equality at k = n.
```
  [b :array] :boolean
  (def n (length b))
  (var ok true)
  (var prefix-sum 0)
  (for k 1 (+ n 1)
    (when ok
      (+= prefix-sum (b (- k 1)))
      (when (< prefix-sum (div (* k (- k 1)) 2))
        (set ok false))))
  (when ok
    (when (not= prefix-sum (div (* n (- n 1)) 2))
      (set ok false)))
  ok)


# ==============================================================================
#  Hamiltonian paths (Easy Rédei theorem)
# ==============================================================================

(define tournament-hamiltonian-path
  ```A Hamiltonian path of the tournament by sequential insertion.

Starting from any vertex, each further vertex is inserted at the position where
the path flips from inward-arc to outward-arc (which always exists).

Returns the node names in path order.
```
  [g :digraph] :array
  (def ns (nodes g))
  (def path @[(ns 0)])
  (each u (seq [x :in ns :when (not= x (path 0))] x)
    (var pos nil)
    (if (has-edge? g u (path 0))
      (set pos 0)
      (do
        (var i 0)
        (while (and (nil? pos) (< i (+ (length path) 1)))
          (def p0 (if (= i 0) nil (path (- i 1))))
          (def p1 (if (= i (length path)) nil (path i)))
          (when (and (or (nil? p0) (has-edge? g p0 u))
                     (or (nil? p1) (has-edge? g u p1)))
            (set pos i))
          (++ i))))
    (array/insert path (or pos (length path)) u))
  path)


# ==============================================================================
#  Kings
# ==============================================================================

(define is-tournament-king?
  ```Returns true if `v` is a king of the tournament.

Every other vertex is reachable from `v` by a directed path of length at most 2
i.e. `v` beats it directly, or `v` beats an intermediate vertex that beats it.
A king is not necesssarily the winner (there may be more than one king).
```
  [g :digraph v :dynamic] :boolean
  (def beaten (to-set (neighbours g v)))
  (var ok true)
  (each u (nodes g)
    (when (and ok (not= u v))
      (unless (get beaten u)
        (when (empty? (seq [w :in (neighbours g v)
                            :when (has-edge? g w u)] w))
          (set ok false)))))
  ok)

(define tournament-kings
  ```Return all kings of the tournament.

Every tournament has at least one king, and every vertex of maximum out-degree
is a king, winners of the tournament are kings.
```
  [g :digraph] :array
  (seq [u :in (nodes g) :when (is-tournament-king? g u)] u))


# ==============================================================================
#  Structural classes
# ==============================================================================

(define strong-tournament?
  "True when the tournament is strongly connected."
  [g :digraph] :boolean
  (def comps (kosaraju-scc g))
  (and (> (length (nodes g)) 0) (= 1 (length comps))))

(define regular-tournament?
  "True when every vertex has the same out-degree (n-1)/2.
A regular tournament exists only for odd n (e.g. the 3-cycle, n = 3)."
  [g :digraph] :boolean
  (let [ns (nodes g)
        n (length ns)]
    (and (> n 0) (odd? n)
         (all (fn [u] (= (length (neighbours g u)) (div (- n 1) 2))) ns))))

(define transitive-tournament?
  ```True when the tournament is transitive.

Its score sequence is 0,1,…,n-1, equivalently it contains no directed 3-cycle.
By the Easy Rédei theorem, a transitive tournament has a unique Hamiltonian path.
```
  [g :digraph] :boolean
  (def scores (tournament-score-sequence g))
  (var ok true)
  (for i 0 (length scores)
    (when (not= (scores i) i) (set ok false)))
  ok)
