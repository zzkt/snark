# -*- mode: janet; -*-
# test/data/graphs: graph fixtures shared by the test suites

#  The test suite uses various consistent structures; a path of n, cycle,
#  star, complete graph, layered DAG, diamond, staircase, etc.

(import ../../graphs :prefix "")

(def idx->name (fn [i] (string i)))
(def idx->int (fn [i] i))
(def alphabet "abcdefghijklmnopqrstuvwxyz")
(def alpha->name (fn [i] (slice alphabet i (+ i 1))))

(defn- lab-of
  "The label function for a builder called with an optional trailing arg."
  [rest]
  (if (empty? rest) idx->name (rest 0)))

(defn- build
  "An undirected :graph over `labels`, with `edges` added."
  [labels edges]
  (def g (make-graph))
  (each l labels (add-node g l))
  (each [u v] edges (add-edge g u v))
  g)

(defn- build-digraph
  "A :digraph over `labels`, with weighted `edges` added."
  [labels edges]
  (def g (make-digraph @[] @{}))
  (each l labels (digraph-add-node g l))
  (each [u v] edges (digraph-add-edge g u v 1))
  g)

(defn weighted-from-pairs
  "An undirected :weighted-graph over [a b ...] with [u v weight] edges."
  [labels edges]
  (def g (make-weighted-graph))
  (each l labels (add-node g l))
  (each e edges (add-edge g (e 0) (e 1) (e 2)))
  g)

(defn multidigraph-from-pairs
  "A :multidigraph over [a b ...] with [u v] edges, all of weight 1."
  [labels edges]
  (def g (make-multidigraph @{} @{}))
  (each l labels (add-node g l))
  (each [u v] edges (add-edge g u v))
  g)

(defn chain-edges
  "The edges of a path/chain on n labels. n-1 edges joining i to i+1."
  [n & rest]
  (def f (lab-of rest))
  (seq [i :range [0 (- n 1)]] [(f i) (f (inc i))]))

(defn path
  "n nodes in a line."
  [n & rest]
  (def f (lab-of rest))
  (build (seq [i :range [0 n]] (f i))
         (seq [i :range [0 (- n 1)]] [(f i) (f (inc i))])))

(defn chain
  "n nodes in a line, as a :digraph: node i -> i+1."
  [n & rest]
  (build-digraph (seq [i :range [0 n]] ((lab-of rest) i))
                 (chain-edges n)))

(defn cycle
  "n nodes in a ring."
  [n & rest]
  (def f (lab-of rest))
  (build (seq [i :range [0 n]] (f i))
         (seq [i :range [0 n]] [(f i) (f (mod (+ i 1) n))])))

(defn star
  "A hub joined to n leaves. n+1 nodes."
  [n & rest]
  (def f (lab-of rest))
  (def hub (f 0))
  (build (seq [i :range [0 n]] (f i))
         (seq [i :range [1 (+ n 1)]] [hub (f i)])))

(defn complete
  "The complete graph on n nodes, K_n."
  [n & rest]
  (def f (lab-of rest))
  (build (seq [i :range [0 n]] (f i))
         (seq [i :range [0 n] j :range [(inc i) n]] [(f i) (f j)])))

(defn from-pairs
  "An undirected graph over [a b ...] with the given [u v] edges."
  [labels edges]
  (build labels edges))

(defn petersen
  "The Petersen graph: 10 nodes, 15 edges, cubic."
  [& rest]
  (def f (lab-of rest))
  (def i1 (fn [i] (f i)))
  (def i2 (fn [i] (f (+ 5 i))))
  # outer 5-cycle
  (def outer (seq [i :range [0 5]] [(i1 i) (i1 (mod (+ i 1) 5))]))
  # inner pentagram: step by 2 around the five inner nodes
  (def inner (seq [i :range [0 5]] [(i2 i) (i2 (mod (+ i 2) 5))]))
  # spokes
  (def spokes (seq [i :range [0 5]] [(i1 i) (i2 i)]))
  (build (seq [i :range [0 10]] (f i))
         (seq [e :in (array/concat outer inner spokes)] e)))

(defn digraph-from-pairs
  "A :digraph over [a b ...] with the given [u v] edges, each of weight 1."
  [labels edges]
  (build-digraph labels edges))

(defn layered
  "A layered DAG of `layers` levels x `per` nodes, every node joined to every
  node of the level above."
  [layers per & rest]
  (def f (lab-of rest))
  (def nm (fn [l i] (string "L" l "N" i)))
  (def labels (seq [l :range [0 layers] i :range [0 per]] (nm l i)))
  (def edges @[])
  (for l 1 layers
    (for i 0 per
      (for j 0 per (array/push edges [(nm (- l 1) j) (nm l i)]))))
  (build-digraph labels edges))

(defn diamond
  "s -> m1..m(n-2) -> t, the 1 -> N -> 1."
  [n & rest]
  (def src (if (empty? rest) "s" ((rest 0) 0)))
  (def snk (if (empty? rest) "t" ((rest 0) 1)))
  (def mid (if (empty? rest)
              (fn [i] (string "m" i))
              (fn [i] ((rest 0) (+ i 2)))))
  (def labels @[src snk])
  (for i 1 (- n 1) (array/push labels (mid i)))
  (def edges @[])
  (for i 1 (- n 1) (do (array/push edges [src (mid i)])
                       (array/push edges [(mid i) snk])))
  (build-digraph labels edges))

(defn staircase
  "n nodes, each joined to the next span."
  [n & rest]
  (def span (if (> (length rest) 1) (rest 1) 3))
  (def f (lab-of rest))
  (def labels (seq [i :range [0 n]] (f i)))
  (def edges @[])
  (for i 0 n
    (for d 1 span (when (< (+ i d) n) (array/push edges [(f i) (f (+ i d))]))))
  (build-digraph labels edges))

(defn tree
  "A balanced ternary tree of `depth` levels below the root."
  [depth & rest]
  (def g (make-graph @{}))
  (def root (if (empty? rest) "r" ((rest 0) 0)))
  (add-node g root)
  (var frontier @[root])
  (var level 1)
  (while (<= level depth)
    (var nextf @[])
    (each p frontier
      (var s 0)
      (while (< s 3)
        (def nm (string p "." s))
        (add-node g nm)
        (add-edge g p nm)
        (array/push nextf nm)
        (++ s)))
    (set frontier nextf)
    (++ level))
  g)

(defn lcg-dag
  "A random-looking but deterministic :digraph of n nodes and m arcs. using `seed`."
  [n m seed & rest]
  (def f (lab-of rest))
  (def g (make-digraph @[] @{}))
  (for i 0 n (digraph-add-node g (f i)))
  (var s seed)
  (for _ 0 m
    (set s (mod (+ (* s 1103515245) 12345) 2147483648))
    (def a (mod (div s 65536) n))
    (set s (mod (+ (* s 1103515245) 12345) 2147483648))
    (def b (mod (div s 65536) n))
    (unless (= a b) (digraph-add-edge g (f a) (f b) 1)))
  g)

(defn lcg-graph
  "As lcg-dag, undirected and deduplicated into a :multigraph."
  [n m seed & rest]
  (def f (lab-of rest))
  (def g (make-multigraph @{}))
  (for i 0 n (add-node g (f i)))
  (var s seed)
  (for _ 0 m
    (set s (mod (+ (* s 1103515245) 12345) 2147483648))
    (def a (mod (div s 65536) n))
    (set s (mod (+ (* s 1103515245) 12345) 2147483648))
    (def b (mod (div s 65536) n))
    (unless (= a b) (add-edge g (f a) (f b))))
  g)
