# -*- mode: janet; -*-
# test/data/standard-graphs: named graph families, generated rather than stored.

(import ../../graphs :prefix "")
(import ../../graphs/export/common :prefix "")

# =============================================================================
#  Builders
# =============================================================================

(defn undirected
  "An undirected :graph over `labels`, with [u v] edges added."
  [labels edges]
  (def g (make-graph @{}))
  (each l labels (add-node g l))
  (each e edges (add-edge g (e 0) (e 1)))
  g)

(defn- names
  "The labels [from, to) as a real array. (map ...) yields a lazy sequence here,
  which array/concat will not take, so the arrays are built explicitly."
  [from to]
  (def out @[])
  (for i from to (array/push out (string i)))
  out)

(defn by-degree
  "Build from a node count and an adjacency-count function, 0-indexed."
  [n f]
  # A double for with an explicit when, not seq with a :when clause.
  (def g (make-graph @{}))
  (for i 0 n (add-node g (string i)))
  (for i 0 n
    (for j (inc i) n
      (when (f i j) (add-edge g (string i) (string j)))))
  g)

# =============================================================================
#  Planarity
# =============================================================================

(defn petersen
  "The Petersen graph: 10 vertices, 15 edges, 3-regular, girth 5. G(5,2)"
  []
  (def g (make-graph @{}))
  (for i 0 10 (add-node g (string i)))
  (for i 0 5
    (add-edge g (string i) (string (mod (+ i 1) 5)))
    (add-edge g (string (+ 5 i)) (string (+ 5 (mod (+ i 2) 5))))
    (add-edge g (string i) (string (+ 5 i))))
  g)

(defn k5
  "K_5: the smallest non-planar graph that is not bipartite."
  []
  (by-degree 5 (fn [i _] true)))

(defn bipartite
  "K_m,n as an explicit nested loop.
m nodes 'l0'.. on one side, n on the other, every cross pair joined."
  [m n]
  (def g (make-graph @{}))
  (for i 0 (+ m n) (add-node g (string i)))
  (for i 0 m
    (for j 0 n
      (add-edge g (string i) (string (+ m j)))))
  g)

(defn k33
  "K_3,3: bipartite, non-planar, 6 vertices and 9 edges."
  []
  (bipartite 3 3))

(defn k4
  "K_4: the smallest graph that is both planar and non-bipartite, 4 vertices."
  []
  (by-degree 4 (fn [i _] true)))

(defn groetzsch
  "The Grotzsch graph: 11 vertices, 20 edges, triangle-free, non-planar."
  []
  # Mycielski's construction on C_5
  (def g (make-graph @{}))
  (for i 0 11 (add-node g (string i)))
  (for i 0 5
    (def j (mod (+ i 1) 5))
    (add-edge g (string i) (string j))
    (add-edge g (string i) (string (+ 5 j)))
    (add-edge g (string j) (string (+ 5 i))))
  (for i 0 5 (add-edge g (string (+ 5 i)) (string 10)))
  g)

(defn dodecahedral
  "The dodecahedral graph: 20 vertices, 30 edges, 3-regular and planar."
  []
  (def g (make-graph @{}))
  (for i 0 20 (add-node g (string i)))
  (for i 0 10
    (add-edge g (string i) (string (mod (+ i 1) 10)))
    (add-edge g (string (+ 10 i)) (string (+ 10 (mod (+ i 2) 10))))
    (add-edge g (string i) (string (+ 10 i))))
  g)

# =============================================================================
#  Bipartiteness and spectral degeneracy
# =============================================================================

(defn k44
  "K_4,4: bipartite, 8 vertices, 4-regular, 16 edges."
  [] (bipartite 4 4))

(defn hypercube
  "The n-dimensional cube Q_n: 2^n vertices, n-regular, bipartite, diameter n."
  [n]
  (def total (math/pow 2 n))
  (defn differing-bits [a b]
    (var d 0)
    (for bit 0 n
      (if (not= (% (div a (math/pow 2 bit)) 2)
               (% (div b (math/pow 2 bit)) 2))
        (++ d)))
    d)
  (by-degree total (fn [i j] (= 1 (differing-bits i j)))))

# =============================================================================
#  Structures
# =============================================================================

(defn complete-graph
  "K_n: the densest case, n(n-1)/2 edges. Every pair adjacent."
  [n] (by-degree n (fn [i _] true)))

(defn complete-bipartite
  "K_m,n: bipartite, one side of size m against n. Same graph as `bipartite`"
  [m n] (bipartite m n))

(defn circulant
  "The circulant graph C_n(S): vertex i joins i+s mod n for each s in steps."
  [n & steps]
  (def g (make-graph @{}))
  (for i 0 n (add-node g (string i)))
  (for i 0 n
    (each s steps
      (def j (mod (+ i s) n))
      (when (not= i j)
        (add-edge g (string i) (string j)))))
  g)

(defn cycle-graph
  "C_n: the circulant C_n({1}), named separately for circular layouts."
  [n] (circulant n 1))

(defn wheel
  "W_n: a hub joined to a cycle of n rim vertices. n+1 vertices."
  [n]
  (def g (make-graph @{}))
  (add-node g "hub")
  (for i 0 n
    (add-node g (string "rim" i))
    (add-edge g "hub" (string "rim" i))
    (add-edge g (string "rim" i) (string "rim" (mod (+ i 1) n))))
  g)

(defn star
  "A hub joined to n leaves. n+1 vertices and maximum degree n."
  [n]
  (def g (make-graph @{}))
  (add-node g "hub")
  (for i 0 n
    (add-node g (string "leaf" i))
    (add-edge g "hub" (string "leaf" i)))
  g)

(defn path-graph
  "A path on n vertices: the longest graph with diameter n-1."
  [n] (by-degree n (fn [i j] (= 1 (- j i)))))

(defn caterpillar
  "A path of `spine` vertices with `legs` leaves hanging off each."
  [spine legs]
  (def g (make-graph @{}))
  (var next-leg 0)
  (for i 0 spine
    (add-node g (string "s" i))
    (when (pos? i) (add-edge g (string "s" (- i 1)) (string "s" i)))
    (for _ 0 legs
      (add-node g (string "l" next-leg))
      (add-edge g (string "s" i) (string "l" next-leg))
      (++ next-leg)))
  g)

(defn barabasi-albert
  "A scale-free graph: m edges per new node, attached preferentially to existing high-degree nodes."
  [n m]
  (def g (make-graph @{}))
  (def repeated @[])
  (for i 0 (min m n) (add-node g (string i)))
  (for i 0 (min m n)
    (for j 0 (min m n)
      (when (< i j)
        (add-edge g (string i) (string j))
        (array/push repeated (string j))
        (array/push repeated (string i)))))
  (for i (min m n) n
    (add-node g (string i))
    (var added 0)
    (var at 0)
    (while (< added m)
      (def target (in repeated (mod at (length repeated))))
      (set at (+ at 1))
      (unless (has-edge? g (string i) target)
        (add-edge g (string i) target)
        (array/push repeated (string i))
        (array/push repeated target)
        (++ added)))
    (when (zero? added) (break)))
  g)

(defn watts-strogatz
  "A ring lattice of n vertices with each edge rewired to a neighbour elsewhere with probability p."
  [n k p]
  (def g (make-graph @{}))
  (for i 0 n (add-node g (string i)))
  (for i 0 n
    (each d (range 1 (inc k))
      (add-edge g (string i) (string (mod (+ i d) n)))))
  (var s 12345)
  (for i 0 n
    (each d (range 1 (inc k))
      (set s (mod (+ (* s 1103515245) 12345) 2147483648))
      (when (< (mod (div s 65536) 1000) (* p 1000))
        (def from (string i))
        (def to (string (mod (+ i d) n)))
        (def alt (string (mod (div s 1024) n)))
        (when (and (not= from to) (not (has-edge? g from alt)))
          (remove-edge g from to)
          (add-edge g from alt)))))
  g)

(defn dumbbell
  "Two K_4's joined by a single bridge edge."
  []
  (def g (make-graph @{}))
  (for i 0 4 (add-node g (string "a" i)))
  (for i 0 4 (add-node g (string "b" i)))
  (for i 0 4
    (for j (inc i) 4
      (add-edge g (string "a" i) (string "a" j))
      (add-edge g (string "b" i) (string "b" j))))
  (add-edge g "a0" "b0")
  g)

(defn toroidal-grid
  "An m by n grid with wrap-around in both directions."
  [m n]
  (def g (make-graph @{}))
  (def nm (fn [i j] (string "r" i "c" j)))
  (for i 0 m (for j 0 n (add-node g (nm i j))))
  (for i 0 m
    (for j 0 n
      (add-edge g (nm i j) (nm i (mod (+ j 1) n)))
      (add-edge g (nm i j) (nm (mod (+ i 1) m) j))))
  g)

(defn isolated-nodes
  "A graph of n nodes with no edges at all."
  [n] (undirected (names 0 n) @[]))

# =============================================================================
#  A catalogue
# =============================================================================

# Each entry is [key description builder]
(def catalogue
  [["petersen" "3-regular, girth 5, non-planar, non-bipartite; the non-planar case that is not K_5" (fn [] (petersen))]
   ["k5" "smallest non-planar graph, odd and dense" (fn [] (k5))]
   ["k33" "smallest non-planar graph that is bipartite; the Kuratowski obstruction that is not K_5" (fn [] (k33))]
   ["k4" "smallest planar graph that is not bipartite" (fn [] (k4))]
   ["g4" "C_4: a cycle as a circulant" (fn [] (cycle-graph 4))]
   ["g6" "C_6: even cycle, so bipartite, so spectrally degenerate" (fn [] (cycle-graph 6))]
   ["c5" "C_5: odd cycle, so not bipartite, so a non-degenerate spectrum where K_4,4 is degenerate" (fn [] (cycle-graph 5))]
   ["g7" "C_7: odd cycle, a single ring, diameter 3" (fn [] (cycle-graph 7))]
   ["c3" "C_3: a triangle" (fn [] (cycle-graph 3))]
   ["k44" "bipartite and regular: the cleanest spectral degeneracy" (fn [] (k44))]
   ["k33-n" "K_3,3, named as a bipartite size check" (fn [] (complete-bipartite 3 3))]
   ["k23" "K_2,3: the smaller bipartite case" (fn [] (complete-bipartite 2 3))]
   ["grotz" "triangle-free and maximal planar" (fn [] (groetzsch))]
   ["dodecahedral" "planar, cubic, girth 5, 20 vertices" (fn [] (dodecahedral))]
   ["q4" "the 4-cube: bipartite, 4-regular, diameter 4" (fn [] (hypercube 4))]
   ["q3" "the 3-cube: bipartite, 3-regular, 8 vertices" (fn [] (hypercube 3))]
   ["k5x" "K_5 as a complete-graph fixture" (fn [] (complete-graph 5))]
   ["k6x" "K_6: the densest case here" (fn [] (complete-graph 6))]
   ["wheel6" "W_6: a degree-6 hub on a ring" (fn [] (wheel 6))]
   ["star5" "a hub and five leaves" (fn [] (star 5))]
   ["path6" "a 6-vertex path, the extreme diameter" (fn [] (path-graph 6))]
   ["path2" "a single edge: the smallest non-trivial graph" (fn [] (path-graph 2))]
   ["caterpillar-4x2" "a tree that is neither a path nor a star" (fn [] (caterpillar 4 2))]
   ["ba-12-2" "scale-free: the only fixture with a heavy-tailed degree distribution" (fn [] (barabasi-albert 12 2))]
   ["ws-12-2-0.3" "small-world: a ring rewired" (fn [] (watts-strogatz 12 2 0.3))]
   ["dumbbell" "two K_4's on a bridge: the smallest graph with a cut edge" (fn [] (dumbbell))]
   ["torus-3x3" "a torus grid, which is planar on a torus and not in the plane" (fn [] (toroidal-grid 3 3))]
   ["isolated-3" "no edges at all" (fn [] (isolated-nodes 3))]
   ["isolated-1" "a single node" (fn [] (isolated-nodes 1))]])


(defn build
  "The catalogue entry for `key`, or an error naming the keys that exist."
  [key]
  (var found nil)
  (each entry catalogue
    (when (= key (entry 0)) (set found entry)))
  (if (nil? found)
    (error (string "no standard graph named " key))
    ((found 2))))

(defn keys []
  (map (fn [entry] (entry 0)) catalogue))

# =============================================================================
#  Zachary's karate club network
# =============================================================================

(defn karate-club
  []
  (def path "test/data/zachary-karate.txt")
  (def g (make-graph @{}))
  (def factions @{})
  (var section :none)
  (each raw (string/split "\n" (slurp path))
    (def line (string/trim raw))
    (cond
      (or (zero? (length line)) (= (line 0) (in "#" 0))) nil
      (= line "edges") (set section :edges)
      (= line "labels") (set section :labels)
      (= section :edges)
      (do
        (def parts (string/split " " line))
        (add-node g (parts 0))
        (add-node g (parts 1))
        (add-edge g (parts 0) (parts 1)))
      (= section :labels)
      (do
        (def parts (string/split " " line))
        (put factions (parts 0) (scan-number (parts 1))))
      true nil))
  {:graph g :factions factions})


# =============================================================================
#  Properties which tests can assert against
# =============================================================================

(defn degrees
  "Degree of every node, as {node degree}."
  [g]
  (def out @{})
  (each u (graph-nodes g) (put out u 0))
  (each-edge g false (fn [u v w]
    (put out u (inc (get out u 0)))
    (when (not= u v) (put out v (inc (get out v 0))))))
  out)

(defn edge-count
  "Undirected edges, counted once."
  [g]
  (def seen @{})
  (var n 0)
  (each-edge g false (fn [u v w]
    (def k (string (min u v) "|" (max u v)))
    (unless (get seen k)
      (put seen k true)
      (++ n))))
  n)

(defn self-loops [g]
  (var n 0)
  (each-edge g false (fn [u v w] (when (= u v) (++ n))))
  n)

(defn regular?
  "The degree every vertex has, or nil when the graph is not regular."
  [g]
  (def ds (degrees g))
  (var only nil)
  (var same true)
  (each [_ d] (pairs ds)
    (if (nil? only)
      (set only d)
      (unless (= d only) (set same false))))
  (if same only nil))

(defn bipartite-colouring
  "Two-colour a connected graph by BFS, or nil if it is not bipartite."
  [g]
  (def colour @{})
  (var start nil)
  (each u (graph-nodes g) (when (nil? start) (set start u)))
  (if (nil? start)
    @{}
    (do
      (var queue @[start])
      (put colour start 0)
      (var ok true)
      (var at 0)
      (while (and ok (< at (length queue)))
        (def u (queue at))
        (set at (inc at))
        (each [v _] (pairs (get (graph-adj g) u @{}))
          (cond
            (nil? (get colour v))
            (do (put colour v (mod (inc (get colour u 0)) 2))
                (array/push queue v))
            (not= (get colour v) (mod (inc (get colour u 0)) 2))
            (set ok false))))
      (if ok colour nil))))

(defn is-bipartite? [g] (not (nil? (bipartite-colouring g))))

# A plain reachability walk that does not use bipartite-colouring
(defn connected? [g]
  (def seen @{})
  (var start nil)
  (each u (graph-nodes g)
    (when (nil? start) (set start u)))
  (if (nil? start)
    true
    (do
      (var queue @[start])
      (put seen start true)
      (var at 0)
      (while (< at (length queue))
        (def u (queue at))
        (set at (inc at))
        (each [v _]
            (pairs (get (graph-adj g) u @{}))
          (unless (get seen v false)
            (put seen v true)
            (array/push queue v))))
      (= (length seen) (length (graph-nodes g))))))
