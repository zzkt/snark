# -*- mode: janet; -*-
# graph/test/planarity: is-planar?

(import ./helper :prefix "")
(import ../graphs :prefix "")
(import ./data/fixtures :prefix "")
(import ./data/standard-graphs :as sg)

(print "* is-planar? using graph fixtures")

# Planar family
(each [k g] (pairs fixtures)
  (cassert-eq (string "is-planar? " k)
              (is-planar? g)
              (get planar-status k)))

# Checks on the extremes
(cassert-eq "empty graph is planar" (is-planar? g-empty) true)
(cassert-eq "single edge is planar" (is-planar? g-single-edge) true)
(cassert-eq "tree is planar" (is-planar? g-tree7) true)
(cassert-eq "graph with articulation is handled blockwise" (is-planar? g-bowtie) true)
(cassert-eq "K5 (Kuratowski) is not planar" (is-planar? g-k5) false)
(cassert-eq "K3,3 (Kuratowski) is not planar" (is-planar? g-k33) false)
(cassert-eq "subdivision of K3,3 is not planar" (is-planar? g-k33-subdivision) false)
(cassert-eq "Petersen graph is not planar" (is-planar? g-petersen) false)

# Compare the planar-status table against fixtures
(cassert-eq "fixture/planar-status agree on count"
            (length fixtures) (length planar-status))

(var sync-ok true)
(each k (keys planar-status)
  (when (not (get fixtures k)) (set sync-ok false)))

(cassert-eq "planar-status keys all have fixtures" sync-ok true)


(print "\n* mixed-type and "/"-containing labels")

(def mg5 (make-graph))
(add-nodes mg5 @[1 "a/b" "c" "d" "e"])
(each e @[[1 "a/b"] [1 "c"] [1 "d"] [1 "e"]
          ["a/b" "c"] ["a/b" "d"] ["a/b" "e"]
          ["c" "d"] ["c" "e"] ["d" "e"]]
      (add-edge mg5 (first e) (in e 1)))

(cassert-eq "mixed-label K5 is not planar" (is-planar? mg5) false)


(def pp2 (make-graph))
(add-nodes pp2 @[1 "a" "b"])
(add-edges pp2 [1 "a"] ["a" "b"] ["b" 1])

(cassert-eq "mixed-label triangle is planar"
            (is-planar? pp2) true)

# Labels that contain "/"
(defn with-colliding-labels [edges]
  (def g (make-graph))
  (add-nodes g "a" "b" "c" "a/b" "b/c")
  (each e edges (add-edge g (e 0) (e 1)))
  g)

(def cg1 (with-colliding-labels
           [["a/b" "c"] ["a" "b/c"] ["a" "b"]
            ["b" "c"] ["a/b" "a"] ["b/c" "c"]]))

(cassert-eq "colliding edge keys: still planar"
            (is-planar? cg1) true)

# Euler
(cassert-eq "colliding edge keys: embeds to the Euler face count"
            (= (length (planar-faces cg1))
               (- (+ 2 (length (structural-edges cg1)))
                  (length (nodes cg1)))) true)

(def cg2 (with-colliding-labels
           [["a/b" "c"] ["a" "b/c"]
            ["a" "b"] ["b" "c"] ["c" "a/b"] ["a" "b/c"]]))

(cassert-eq "colliding edge keys: multi-edge labels stay planar"
            (is-planar? cg2) true)

# DMP greedy dead-ends
(defn edges-to-graph [edges]
  (def g (make-graph))
  (def vs (table))
  (each e edges (put vs (e 0) true) (put vs (e 1) true))
  (each v (sort (keys vs)) (add-node g v))
  (each e edges (add-edge g (e 0) (e 1)))
  g)

(def dmp-witness-edges
  [[0 4] [0 6] [1 2] [1 4] [1 5] [1 6]
   [2 3] [3 5] [3 6] [4 5] [5 6]])

(def dw (edges-to-graph dmp-witness-edges))
(cassert-eq "DMP min-face: greedy dead-end graph is planar"
            (is-planar? dw) true)
(cassert-eq "DMP min-face: embeds"
            (not (nil? (planar-faces dw))) true)

# the same edge set in a different order should disagree with the above
(def dw-reordered
  (edges-to-graph [[5 6] [3 5] [3 6] [1 5] [1 6] [2 3]
                   [4 5] [1 4] [0 6] [0 4] [1 2]]))

(cassert-eq "DMP min-face: verdict is independent of edge order"
            (is-planar? dw-reordered) (is-planar? dw))

# bridge of minimum admissible-face count needs no retries
(cassert-eq "DMP min-face: K4 is planar"
            (is-planar? (edges-to-graph [[0 1] [0 2] [0 3] [1 2] [1 3] [2 3]]))
            true)

(cassert-eq "DMP min-face: chorded C4 is planar"
            (is-planar? (edges-to-graph [[0 1] [1 2] [2 3] [3 0] [0 2]]))
            true)

# A non-planar block with many vertices should be disproved quickly
(def karate-g ((sg/karate-club) :graph))
(cassert-eq "DMP min-face: non-planar karate club is decided"
            (is-planar? karate-g) false)

(cassert-eq "DMP min-face: non-planar karate club has no embedding"
            (nil? (planar-faces karate-g)) true)

(print-results)
