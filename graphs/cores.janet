# -*- mode: janet; -*-
# graphs/cores: k-core decomposition and related notions

# Batagelj-Zaveršnik peeling for core numbers, standard decomposition
# subgraphs: core, shell, crust, corona and onion layers.

(import deft :prefix "")
(import ./types :prefix "")
(import ./simple-graph :prefix "")
(import ./util :prefix "")
(import ./heap :prefix "")

(def max-safe-int
  "2^53-1 is the largest integer exactly representable as a 64-bit float."
  9007199254740991)

# ==============================================================================
#  Core numbers (Batagelj-Zaveršnik)
# ==============================================================================

(define core-number
  ```Find core number of each node of graph `g`.

The core number is the largest k such that the node lies in a subgraph where
every node has degree at least k (induced by that subgraph).

Returns {node core-number}
```
  [g :any-graph] :table
  (def nodes (nodes g))
  (def deg @{})
  (var maxdeg 0)
  (each u nodes
    (def d (length (neighbours g u)))
    (put deg u d)
    (when (> d maxdeg) (set maxdeg d)))
  (def buckets (array/new (+ maxdeg 1)))
  (for i 0 (+ maxdeg 1) (array/push buckets @[]))
  (def pos @{})
  (each u nodes
    (put pos u (length (buckets (get deg u))))
    (array/push (buckets (get deg u)) u))
  (def active @{})
  (def core @{})
  (each u nodes (put active u true) (put core u 0))
  (var processed 0)
  (var k 0)
  (while (< processed (length nodes))
    # lowest non-empty bucket is the current core number
    (while (and (< k (+ maxdeg 1)) (empty? (buckets k)))
      (++ k))
    (when (< k (+ maxdeg 1))
      (def v (array/pop (buckets k)))
      (++ processed)
      (put core v k)
      (put active v false)
      (each w (neighbours g v)
        (when (and (get active w) (> (get deg w) k))
          # move w down one degree
          (def d (get deg w))
          (def bin (buckets d))
          (def wpos (get pos w))
          (def last (array/pop bin))
          (when (not= last w)
            (put bin wpos last)
            (put pos last wpos))
          (put deg w (- d 1))
          (put pos w (length (buckets (- d 1))))
          (array/push (buckets (- d 1)) w)))))
  core)


# ==============================================================================
#  Decompositions
# ==============================================================================

(defn- induced-subgraph
  "New graph on the keep-set with all induced edges among keep nodes."
  [g keep]
  (def result (make-graph @{}))
  (each u (keys keep) (graph-add-node result u))
  (each u (keys keep)
    (each v (neighbours g u)
      (when (and (get keep v) (not= u v) (<= (hash u) (hash v)))
        (graph-add-edge result u v))))
  result)

(defn- induced-by
  ```The induced subgraph on the nodes for which (`pred` u) holds.```
  [g pred]
  (def keep @{})
  (each u (nodes g)
    (when (pred u) (put keep u true)))
  (induced-subgraph g keep))

(defn- induced-by-core-number
  ```Induced subgraph on nodes whose core number `k` satisfies `pred`.```
  [g k pred]
  (def cn (core-number g))
  (induced-by g (fn [u] (pred (get cn u) k))))

(define k-core
  "The maximal induced subgraph of `g` where every node has degree at least `k`."
  [g :any-graph k :number] :graph
  (induced-by-core-number g k >=))

(define k-shell
  "The subgraph of nodes in `g` with core number exactly `k`."
  [g :any-graph k :number] :graph
  (induced-by-core-number g k =))

(define k-crust
  "The subgraph of nodes in `g` with core number <= `k` (the 'crust' of the graph)."
  [g :any-graph k :number] :graph
  (induced-by-core-number g k <=))

(define k-corona
  ```The subgraph of nodes in the k-core of `g` whose k-core neighbours are at
most `k`. i.e. the boundary of the k-core.```
  [g :any-graph k :number] :graph
  (def cn (core-number g))
  (induced-by g
    (fn [u]
      (when (= (get cn u) k)
        (var kcore-neighbours 0)
        (each v (neighbours g u)
          (when (>= (get cn v) k) (++ kcore-neighbours)))
        (<= kcore-neighbours k)))))

(define onion-layers
  ```Onion decomposition of graph `g`.

Repeatedly peel, round by round, minimum-degree nodes. Each round peels every
remaining node whose degree is at most the core threshold, where the threshold
starts at 1 and is raised to the lowest remaining degree that exceeds it.
Layer 1 is the first peel. The decomposition should be O((V + E) log Δ).

Returns {node onion-layer}
```
  [g :any-graph] :table
  (def nodes (nodes g))
  (def degree @{})
  (var maxdeg 0)
  (each u nodes
    (def d (length (neighbours g u)))
    (put degree u d)
    (when (> d maxdeg) (set maxdeg d)))
  # Bucket by degree
  (def buckets (array/new (+ maxdeg 1)))
  (for i 0 (+ maxdeg 1) (array/push buckets @[]))
  (def pos @{})
  (each u nodes
    (put pos u (length (buckets (get degree u))))
    (array/push (buckets (get degree u)) u))
  # non-empty mask and min-heap of buckets that currently hold nodes
  (def nonempty (array/new (+ maxdeg 1)))
  (for i 0 (+ maxdeg 1) (array/push nonempty false))
  (def occupied (make-binary-min-heap @[]))
  (each u nodes
    (def d (get degree u))
    (unless (nonempty d)
      (put nonempty d true)
      (heap-push occupied d)))
  (def remaining @{})
  (def layer @{})
  (each u nodes (put remaining u true) (put layer u 0))
  (var current-core 1)
  (var current-layer 0)
  (var left (length nodes))
  (while (> left 0)
    # lowest currently non-empty bucket
    (var lo (if (> (heap-size occupied) 0) (heap-peek occupied) nil))
    (while (and (not (nil? lo)) (not (nonempty lo)))
      (heap-pop occupied)
      (set lo (if (> (heap-size occupied) 0) (heap-peek occupied) nil)))
    (when (nil? lo)
      (error "onion-layers: No non-empty buckets."))
    (when (> lo current-core) (set current-core lo))
    (++ current-layer)
    # every bucket at or below the threshold
    (def worklist @[])
    (def this-round @{})
    (while (> (heap-size occupied) 0)
      (def idx (heap-peek occupied))
      (when (and (nonempty idx) (> idx current-core)) (break))
      (if (nonempty idx)
        (do
          (heap-pop occupied)
          (put nonempty idx false)
          (each u (buckets idx)
            (put this-round u true) (array/push worklist u))
          (put buckets idx @[]))
        (heap-pop occupied)))
    (each v worklist
      (put layer v current-layer)
      (put remaining v nil)
      (-- left)
      (each w (neighbours g v)
        (when (and (get remaining w) (not (get this-round w)))
          # move w down one degree
          (def d (get degree w))
          (def bin (buckets d))
          (def wpos (get pos w))
          (def last (array/pop bin))
          (when (not= last w)
            (put bin wpos last)
            (put pos last wpos))
          # the bucket is empty when w is the last node
          (when (zero? (length bin))
            (put nonempty d false))
          (def nd (- d 1))
          (put degree w nd)
          (put pos w (length (buckets nd)))
          (array/push (buckets nd) w)
          (unless (nonempty nd)
            (put nonempty nd true)
            (heap-push occupied nd))))))
  layer)
