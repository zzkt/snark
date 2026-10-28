# -*- mode: janet; -*-
# graphs/random-graph: random graph generators

# The Erdős-Rényi G(n,p) and G(n,m) models and the Barabási-Albert
# preferential-attachment model.
#
# References
#  - Diestel 2025, Graph Theory. Ch.11
#  - [Erdős59], [Barabasi99]

(import deft :prefix "")
(import ./simple-graph :prefix "")

# ==============================================================================
#  Helpers
# ==============================================================================

(defn- seed-rng
  "Seed the PRNG when a seed is supplied."
  [&opt seed]
  (when seed (math/seedrandom seed)))

(defn- rand-below
  "Random integer in [0, n)."
  [n] (math/floor (* (math/random) n)))

(defn- rand-float
  "Random float in [0, 1)."
  [] (math/random))

(defn- sample-distinct
  "Sample `m` distinct values from {0 … N-1} uniformly.
Floyd's subset sampling with one PRNG draw per chosen value."
  [m N]
  (def s @{})
  (for j (- N m) N
    (def t (rand-below (+ j 1)))
    (if (get s t) (put s j true) (put s t true)))
  s)

(defn- ranks->pairs
  ```Map ascending ranks in [0, n(n-1)/2) to unordered pairs {i, j}, i < j.

Returns an array of [i j] pairs, positionally matching `ranks`.
 ```
  [n ranks]
  (def pairs @[])
  (var i 0)
  (var c 0)
  (var r 0)
  (each k ranks
    (set r (- k c))
    (var per (dec (- n i)))
    (while (>= r per)
      (set c (+ c per))
      (set r (- r per))
      (++ i)
      (set per (dec (- n i))))
    (array/push pairs @[i (+ i 1 r)]))
  pairs)


# ==============================================================================
#  Erdős–Rényi G(n,p)
# ==============================================================================

(define random-graph
  ```Sample an Erdős–Rényi G(n,p) graph.

Sample `n` vertices 0 … n-1, each of the n(n-1)/2 possible edges included
independently with probability `p`. Optional `seed` for reproducibility.

Returns a graph.
```
  [n :number p :number &opt seed :number] :graph
  (seed-rng seed)
  (def g (make-graph))
  (for i 0 n (graph-add-node g i))
  (for i 0 n
    (for j (+ i 1) n
      (when (< (rand-float) p)
        (graph-add-edge g i j))))
  g)


# ==============================================================================
#  Erdős-Rényi G(n,m)
# ==============================================================================

(define random-graph-edges
  ```Sample an Erdős–Rényi G(n,m) graph.

Sample n vertices 0 … n-1 with exactly m distinct edges chosen uniformly from
the n(n-1)/2 possibilites. Optional `seed` for reproducibility.

Returns a graph. Raises an error if `m` exceeds the number of possible edges.
```
  [n :number m :number &opt seed :number] :graph
  (seed-rng seed)
  (def g (make-graph))
  (for i 0 n (graph-add-node g i))
  (def total (div (* n (- n 1)) 2))
  (when (> m total)
    (error (string "random-graph-edges: m=" m " exceeds the " total
                   " possible edges of K_" n)))
  # Floyd's subset sampling
  (def ranks (sort (keys (sample-distinct m total))))
  (each pair (ranks->pairs n ranks)
    (graph-add-edge g (pair 0) (pair 1)))
  g)


# ==============================================================================
#  Barabási-Albert preferential attachment
# ==============================================================================

(define barabasi-albert
  ```Barabási–Albert graph on n vertices.

Begin with a clique of `m0` seeds, then add each new vertex connected to `m`
existing vertices chosen with probability proportional to degree.

Returns a graph. Raises an error when m0 ≥ n or m > m0.
```
  [n :number m0 :number m :number &opt seed :number] :graph
  (when (or (>= m0 n) (> m m0))
    (error "barabasi-albert: require m ≤ m0 < n"))
  (seed-rng seed)
  (def g (make-graph))
  # seed clique
  (for i 0 m0 (graph-add-node g i))
  (for i 0 m0
    (for j (+ i 1) m0
      (graph-add-edge g i j)))
  # degree-weighted target list
  (def targets @[])
  (each u (keys (g :adj))
    (def d (graph-degree g u))
    (for _ 0 d (array/push targets u)))
  (var v m0)
  (while (< v n)
    (graph-add-node g v)
    (def chosen @{})
    (var cnt 0)
    (while (< cnt m)
      (def t (targets (rand-below (length targets))))
      (when (and (nil? (get chosen t)) (not= t v))
        (put chosen t true)
        (graph-add-edge g v t)
        # each new edge adds one degree to v and t
        (array/push targets v)
        (array/push targets t)
        (++ cnt)))
    (++ v))
  g)
