# -*- mode: janet; -*-
# graphs/test/standard-graphs: test to test the test graphs

(import ./helper :prefix "")
(import ../graphs :prefix "")
(import ../graphs/export/common :prefix "")  # each-edge (tmp?)
(import ./data/standard-graphs :prefix "sg-")

(defn- edge-set [g]
  (def seen @{})
  (each-edge g false (fn [u v w]
                       (put seen (string (min u v) "|" (max u v)) true)))
  (sort (keys seen)))

(defn- same-shape? [a b]
  (and (arr-eq (sort (map string (graph-nodes a)))
               (sort (map string (graph-nodes b))))
       (arr-eq (edge-set a) (edge-set b))))

(defn- triangle-count [g]
  (def adj (graph-adj g))
  (var n 0)
  (each u (graph-nodes g)
    (each [v _] (pairs (get adj u @{}))
      (when (> (cmp v u) 0)
        (each [w _] (pairs (get adj v @{}))
          (when (> (cmp w v) 0)
            (when (get (get adj u @{}) w nil)
              (++ n)))))))
  n)

# =============================================================================
#  a named catalogue
# =============================================================================

# [key nodes edges degree-or-nil bipartite? triangle-free?]
(def expected
  [["petersen" 10 15 3 false false]
   ["k5" 5 10 4 false false]
   ["k33" 6 9 3 true false]
   ["k4" 4 6 3 false false]
   ["c3" 3 3 2 false false]
   ["g4" 4 4 2 true false]
   ["c5" 5 5 2 false false]
   ["g6" 6 6 2 true false]
   ["g7" 7 7 2 false false]
   ["k44" 8 16 4 true false]
   ["k33-n" 6 9 3 true false]
   ["k23" 5 6 nil true false]
   ["grotz" 11 20 nil false true]
   ["dodecahedral" 20 30 3 false false]
   ["q3" 8 12 3 true false]
   ["q4" 16 32 4 true false]
   ["k5x" 5 10 4 false false]
   ["k6x" 6 15 5 false false]
   ["wheel6" 7 12 nil false false]
   ["star5" 6 5 nil true false]
   ["path6" 6 5 nil true false]
   ["path2" 2 1 nil true false]
   ["caterpillar-4x2" 12 11 nil true false]
   ["ba-12-2" 12 nil nil false false]
   ["ws-12-2-0.3" 12 nil nil false false]
   ["dumbbell" 8 13 nil false false]
   ["torus-3x3" 9 18 4 false false]
   ["isolated-3" 3 0 nil true true]
   ["isolated-1" 1 0 nil true true]])

# Every catalogue entry should appear above
(cassert-set-eq "every named graph is compared against a known representation"
                (sort (map (fn [e] (e 0)) expected))
                (sort (sg-keys)))

(each [key want-n want-e degree bip? tri-free?] expected
  (def g (sg-build key))
  (cassert-eq (string "standard graph " key ": node count")
              (length (graph-nodes g)) want-n)
  (if want-e
    (cassert-eq (string "standard graph " key ": edge count")
                (sg-edge-count g) want-e))
  (cassert-eq (string "standard graph " key ": no self loops")
              (sg-self-loops g) 0)
  (if degree
    (cassert-eq (string "standard graph " key ": is " (string degree) "-regular")
                (sg-regular? g) degree))
  (cassert-eq (string "standard graph " key ": bipartiteness")
              (sg-is-bipartite? g) bip?)
  (if tri-free?
    (cassert-eq (string "standard graph " key ": triangle-free")
                (triangle-count g) 0)))

# =============================================================================
#  verify specific claims
# =============================================================================

# C_n is bipartite when n is even.
(each n [3 4 5 6 7 8]
  (cassert-eq (string "C_" n " is " (if (even? n) "" "not ") "bipartite")
              (sg-is-bipartite? (sg-cycle-graph n)) (even? n)))

# The hypercube is n-regular in 2^n vertices
(each n [2 3 4]
  (def g (sg-hypercube n))
  (cassert-eq (string "Q_" n ": 2^" n " vertices")
              (length (graph-nodes g)) (math/pow 2 n))
  (cassert-eq (string "Q_" n ": " n "-regular")
              (sg-regular? g) n)
  (cassert-eq (string "Q_" n ": edge count")
              (sg-edge-count g) (* 0.5 (* (math/pow 2 n) n))))

# The wheel's hub must actually be the hub and not node 0
(def w6 (sg-wheel 6))
(cassert-eq "W_6: the hub has the rim's degree"
            (get (sg-degrees w6) "hub") 6)
(cassert-eq "W_6: a rim vertex has one more than 2"
            (get (sg-degrees w6) "rim0") 3)

# The dumbbell is two K_4's joined by a single bridge
(def db (sg-dumbbell))
(cassert-eq "dumbbell: connected" (sg-connected? db) true)
(cassert-eq "dumbbell: not regular, because the bridge endpoints differ"
            (sg-regular? db) nil)
(cassert-eq "dumbbell: the bridge endpoints are the only vertices of degree 4"
            (get (sg-degrees db) "a0") 4)
(cassert-eq "dumbbell: a vertex off the bridge has degree 3"
            (get (sg-degrees db) "a1") 3)
(remove-edge db "a0" "b0")
(cassert-eq "dumbbell: cutting the bridge disconnects it" (sg-connected? db) false)

# Completeness for K_n
(def k6 (sg-complete-graph 6))
(cassert-eq "K_6: every pair present" (sg-edge-count k6) 15)
(each u ["0" "1" "2" "3" "4" "5"]
  (cassert-eq (string "K_6: " u " has degree 5")
              (get (sg-degrees k6) u) 5))

# =============================================================================
#  determinism
# =============================================================================

# No random source is used in the generators, so a rebuild must be identical.
(each entry sg-catalogue
  (def key (entry 0))
  (cassert-eq (string "standard graph " key ": deterministic")
              (same-shape? (sg-build key) (sg-build key)) true))

# =============================================================================
#  A network
# =============================================================================

(def kc (sg-karate-club))
(def kc-g (get kc :graph))
(def factions (get kc :factions))

(cassert-eq "karate club: 34 members" (length (graph-nodes kc-g)) 34)
(cassert-eq "karate club: 77 edges" (sg-edge-count kc-g) 77)
(cassert-eq "karate club: no self loops" (sg-self-loops kc-g) 0)
(cassert-eq "karate club: a label for every member"
            (length factions) 34)
(cassert-eq "karate club: 17 members in Mr. Hi's club"
            (length (filter (fn [f] (= f 1)) (values factions))) 17)
(cassert-eq "karate club: 17 members in the Officer's club"
            (length (filter (fn [f] (= f 2)) (values factions))) 17)
(cassert-eq "karate club: connected" (sg-connected? kc-g) true)
(cassert-eq "karate club: not bipartite" (sg-is-bipartite? kc-g) false)
(cassert-eq "karate club: not regular" (sg-regular? kc-g) nil)
(cassert-set-eq "karate club: the two factions cover every member"
                (sort (map string (keys factions)))
                (sort (map string (graph-nodes kc-g))))

(def n1 (get (graph-adj kc-g) "1" @{}))
(cassert-eq "karate club: member 1 has 16 neighbours" (length n1) 16)

(def sides @{})
(each [u _] (pairs n1)
  (put sides (get factions u) true))
(cassert-set-eq "karate club: member 1 has neighbours in both clubs"
                (sort (keys sides)) @[1 2])

# =============================================================================
#  Planarity
# =============================================================================

(cassert-eq "K_4 is planar" (is-planar? (sg-build "k4")) true)
(cassert-eq "K_5 is not planar" (is-planar? (sg-build "k5")) false)
(cassert-eq "K_3,3 is not planar" (is-planar? (sg-build "k33")) false)
(cassert-eq "the Petersen graph is not planar" (is-planar? (sg-build "petersen")) false)
(cassert-eq "the dodecahedral graph is planar" (is-planar? (sg-build "dodecahedral")) true)
(cassert-eq "a wheel is planar" (is-planar? (sg-build "wheel6")) true)
(cassert-eq "the 4-cube is not planar" (is-planar? (sg-build "q4")) false)
(cassert-eq "a torus grid is not planar" (is-planar? (sg-build "torus-3x3")) false)
(cassert-eq "the Grotzsch graph is not planar" (is-planar? (sg-build "grotz")) false)

# A triangle-free planar graph on V vertices has at  most 2(V - 2) edges
(def grotz (sg-build "grotz"))
(cassert-eq "Grotzsch: triangle-free, so the Euler bound applies"
            (triangle-count grotz) 0)
(cassert-eq "Grotzsch: 20 edges exceeds the 18 a triangle-free planar graph allows"
            (> (sg-edge-count grotz) (* 2 (- (length (graph-nodes grotz)) 2)))
            true)

# The two Kuratowski obstructions
(cassert-set-eq "Kuratowski's two obstructions (planarity/bipartiteness)"
                [(is-planar? (sg-build "k5"))
                 (sg-is-bipartite? (sg-build "k5"))
                 (is-planar? (sg-build "k33"))
                 (sg-is-bipartite? (sg-build "k33"))]
                @[false false false true])

(print-results)
