# -*- mode: janet; -*-
# graphs/test/graph-types.janet:  construction, node/edge data, queries, etc

(import ./helper :prefix "")
(import ../graphs :prefix "")

# Type-specific fixtures for parameterized tests
(def type-fixtures
  @[[:graph (fn [] (make-graph))
     (fn [g a b w] (add-edge g a b (or w 1))) add-node]
    [:digraph (fn [] (make-digraph))
     (fn [g a b w] (add-edge g a b (or w 1))) add-node]
    [:weighted-graph (fn [] (make-weighted-graph))
     (fn [g a b w] (add-edge g a b (or w 1.0))) add-node]
    [:multigraph (fn [] (make-multigraph))
     (fn [g a b w] (add-edge g a b (or w 1))) add-node]
    [:multidigraph (fn [] (make-multidigraph))
     (fn [g a b w] (add-edge g a b (or w 1))) add-node]])

# =============================================================================
#  Construction
# =============================================================================

(print "\n* graph types: construction")

(each [type-name make-graph add-edge add-node] type-fixtures
  (def g (make-graph))
  (cassert-eq (string type-name ": empty node count 0") (node-count g) 0)
  (cassert-eq (string type-name ": empty edge count 0") (edge-count g) 0)

  (add-node g "a")
  (add-node g "b")
  (cassert-eq (string type-name ": node count 2")
              (node-count g) 2)
  (cassert-eq (string type-name ": add-node idempotent")
              (do (add-node g "a") (node-count g)) 2)

  (add-edge g "a" "b" (if (= type-name :weighted-graph) 1.0 1))
  (cassert-eq (string type-name ": edge count 1")
              (edge-count g) 1)
  (cassert-eq (string type-name ": has edge a-b")
              (has-edge? g "a" "b") true))

# Self loops
(print "\n* graph types: self-loops")
(each [type-name make-graph add-edge add-node] type-fixtures
  (def g (make-graph))
  (add-edge g "a" "a" (if (= type-name :weighted-graph) 1.0 1))
  (cassert-eq (string type-name ": self-loop exists")
              (has-edge? g "a" "a") true))

# =============================================================================
#  Queries
# =============================================================================

(print "\n* graph types: queries")
(each [type-name make-graph add-edge add-node] type-fixtures
  (def g (make-graph))
  (add-edge g "a" "b" (if (= type-name :weighted-graph) 1.0 1))
  (add-edge g "b" "c" (if (= type-name :weighted-graph) 2.0 1))
  (add-edge g "a" "c" (if (= type-name :weighted-graph) 3.0 1))

  (cassert-eq (string type-name ": neighbours a")
              (length (neighbours g "a"))
              (if (or (= type-name :digraph) (= type-name :multidigraph)) 2 2))
  (cassert-eq (string type-name ": has node a")
              (has-node? g "a") true)
  (cassert-eq (string type-name ": has edge a-b")
              (has-edge? g "a" "b") true)
  (cassert-eq (string type-name ": missing edge a-d")
              (has-edge? g "a" "d") false))

# =============================================================================
#  Degree
# =============================================================================

(print "\n* graph types: degree")
(each [type-name make-graph add-edge add-node] type-fixtures
  (def g (make-graph))
  (add-edge g "a" "b" (if (= type-name :weighted-graph) 1.0 1))
  (add-edge g "b" "c" (if (= type-name :weighted-graph) 1.0 1))
  (add-edge g "c" "a" (if (= type-name :weighted-graph) 1.0 1))

  (cassert-eq (string type-name ": degree a") (degree g "a") 2))

# Self-loop degree
(print "\n* graph types: self-loop degree")
(each [type-name make-graph add-edge add-node] type-fixtures
  (def g (make-graph))
  (add-edge g "u" "u" (if (= type-name :weighted-graph) 1.0 1))
  (cassert-eq (string type-name ": self-loop degree 2")
              (degree g "u") 2)
  (add-edge g "u" "v" (if (= type-name :weighted-graph) 1.0 1))
  (cassert-eq (string type-name ": self-loop + edge degree 3")
              (degree g "u") 3)
  (cassert-eq (string type-name ": handshaking holds")
              (sum (degree-sequence g)) (* 2 (edge-count g))))

# Multidigraph in/out degree
(def mdg (make-multidigraph))
(add-edge mdg "a" "b")
(add-edge mdg "a" "b")
(add-edge mdg "b" "a")
(cassert-eq "mdg out-degree counts parallel" (out-degree mdg "a") 2)
(cassert-eq "mdg in-degree counts parallel" (in-degree mdg "b") 2)
(cassert-eq "mdg degree = in+out" (degree mdg "a") 3)

# =============================================================================
#  Node Data
# =============================================================================

(print "\n* graph types: node data")
(each [type-name make-graph add-edge add-node] type-fixtures
  (def g (make-graph))
  (add-node g "a")
  (cassert-eq (string type-name ": default node data")
              (node-data g "a") "a")
  (set-node-data g "a" {:kind type-name})
  (cassert-eq (string type-name ": set node data")
              (node-data g "a") {:kind type-name})
  (cassert-eq (string type-name ": set-node-data adds node")
              (do (set-node-data g "c" "label") (node-data g "c")) "label")
  (cassert-eq (string type-name ": node-data absent")
              (node-data g "zz") nil)
  (cassert-eq (string type-name ": nodes-table keys")
              (length (keys (nodes-table g))) (node-count g)))

# =============================================================================
#  Edge Data
# =============================================================================

(print "\n* graph types: edge data")
(each [type-name make-graph add-edge add-node] type-fixtures
  (def g (make-graph))
  (add-edge g "a" "b" 1.0)
  (set-edge-data g "a" "b" @{:label "test"})
  (cassert-eq (string type-name ": edge-data stored")
              (get (edge-data g "a" "b") :label) "test")
  (cassert-eq (string type-name ": edge-data-has?")
              (edge-data-has? g "a" "b") true)
  (remove-edge-data g "a" "b")
  (cassert-eq (string type-name ": forget edge data")
              (edge-data g "a" "b") nil))

# Undirected
(def g-undir (make-graph))
(add-edge g-undir "a" "b")
(set-edge-data g-undir "a" "b" @{:color "red"})
(cassert-eq "undirected read in either order"
            (get (edge-data g-undir "b" "a") :color) "red")
(cassert-eq "undirected stores one key"
            (length (keys (edge-data-table g-undir))) 1)

# Directed order
(def g-dir (make-digraph @[] @{}))
(add-edge g-dir "a" "b" 1)
(set-edge-data g-dir "a" "b" @{:v 1})
(cassert-eq "directed order sensitive"
            (edge-data g-dir "b" "a") nil)

# Multigraph parallel edges share key
(def g-multi (make-multigraph))
(add-edge g-multi "a" "b")
(add-edge g-multi "a" "b")
(set-edge-data g-multi "a" "b" @{:class "link"})
(cassert-eq "multigraph parallel edges share key"
            (length (keys (edge-data-table g-multi))) 1)

# =============================================================================
#  Removal
# =============================================================================

(print "\n* graph types: remove-edge")
(each [type-name make-graph add-edge add-node] type-fixtures
  (def g (make-graph))
  (add-edge g "a" "b" 1.0)
  (add-edge g "b" "c" 1.0)
  (remove-edge g "a" "b")
  (cassert-eq (string type-name ": remove-edge breaks edge")
              (has-edge? g "a" "b") false)
  (cassert-eq (string type-name ": remove-edge count")
              (edge-count g) 1))

(print "\n* graph types: remove-node")
(each [type-name make-graph add-edge add-node] type-fixtures
  (def g (make-graph))
  (add-edge g "a" "b" 1.0)
  (add-edge g "b" "c" 1.0)
  (remove-node g "a")
  (cassert-eq (string type-name ": remove-node node count")
              (node-count g) 2)
  (cassert-eq (string type-name ": remove-node edge count")
              (edge-count g) 1)
  (cassert-eq (string type-name ": remove-node breaks edge")
              (has-edge? g "a" "b") false)
  (cassert-eq (string type-name ": remove-node clears data")
              (node-data g "a") nil))

# remove-node clears edge data
(print "\n* graph types: remove-node clears edge data")
(each [type-name make-graph add-edge add-node] type-fixtures
  (def g (make-graph))
  (add-edge g "a" "b" 1.0)
  (add-edge g "b" "c" 1.0)
  (set-edge-data g "a" "b" @{:v 1})
  (set-edge-data g "b" "a" @{:v 2})
  (set-edge-data g "b" "c" @{:v 3})
  (remove-node g "a")
  (cassert-eq (string type-name ": remove-node clears edge data")
              (edge-data g "a" "b") nil)
  (cassert-eq (string type-name ": remove-node leaves other edge data")
              (get (edge-data g "b" "c") :v) 3))

# =============================================================================
#  One sided adjacency (simple-graph, weighted-graph)
# =============================================================================

(print "\n* graph types: one-sided adjacency")
(def g-one (make-graph :nodes @[] :adj @{"a" @{"b" true}}))
(cassert-eq "one-sided row reads as edge"
            (has-edge? g-one "a" "b") true)
(remove-edge g-one "a" "b")
(cassert-eq "remove-edge on one-sided row"
            (has-edge? g-one "a" "b") false)
(cassert-eq "remove-edge on one-sided row -> no entry remains"
            (get (get (g-one :adj) "a") "b") nil)

# Weighted-graph
(def w-one (make-weighted-graph :nodes @[] :adj @{"a" @{"b" 1.0}}))
(cassert-eq "weighted one-sided row"
            (edge-weight w-one "a" "b") 1.0)
(remove-edge w-one "a" "b")
(cassert-eq "weighted remove-edge on one-sided row"
            (edge-weight w-one "a" "b") nil)

# =============================================================================
#  Node names (string/integer)
# =============================================================================

(print "\n* graph types: node name types")
(each [type-name make-graph add-edge add-node] type-fixtures
  (def g (make-graph))
  (add-node g "a") (add-node g "b")
  (add-edge g "a" "b" 1.0)
  (cassert-eq (string type-name ": string labels accepted")
              (has-edge? g "a" "b") true)

  (def g2 (make-graph))
  (add-node g2 1) (add-node g2 2)
  (add-edge g2 1 2 1.0)
  (cassert-eq (string type-name ": integer labels accepted")
              (has-edge? g2 1 2) true))

# Mixed labels
(def g-mix (make-graph))
(add-node g-mix "a")
(add-node g-mix 7)
(add-edge g-mix "a" 7 1.0)
(cassert-eq "mixed string/integer"
            (has-edge? g-mix "a" 7) true)

# Keywords/structs rejected
(each [type-name make-graph add-edge add-node] type-fixtures
  (def g (make-graph))
  (cassert-err (string type-name ": keyword rejected")
               (add-edge g :a :b 1.0))
  (cassert-err (string type-name ": struct rejected")
               (add-edge g {:x 1} {:y 2} 1.0)))

# =============================================================================
#  Edge weight accessors
# =============================================================================

(print "\n* graph types: edge weight")
(def wg (make-weighted-graph @[] @{}))
(add-edge wg "a" "b" 2.5)
(cassert-eq "weighted-graph edge weight"
            (edge-weight wg "a" "b") 2.5)
(cassert-eq "weighted-graph symmetric weight"
            (edge-weight wg "b" "a") 2.5)

(def dg (make-digraph @[] @{}))
(add-edge dg "a" "b" 3)
(cassert-eq "digraph edge weight"
            (edge-weight dg "a" "b") 3)
(cassert-eq "digraph missing edge weight"
            (edge-weight dg "b" "a") nil)

# =============================================================================
#  Density
# =============================================================================

(print "\n* graph types: density")
(each [type-name make-graph add-edge add-node] type-fixtures
  (def g (make-graph))
  (add-edge g "a" "b" (if (= type-name :weighted-graph) 1.0 1))
  (add-edge g "b" "c" (if (= type-name :weighted-graph) 1.0 1))
  (def expected (if (or (= type-name :digraph)
                        (= type-name :multidigraph))
                  0.33333333333333 0.66666666666667))
  (cassert-eq (string type-name ": 3-node path density")
              (< (math/abs (- (density g) expected)) 1e-9) true))

# Multigraph multiplicity-aware density
(def mg-den (make-multigraph))
(add-node mg-den "a") (add-node mg-den "b")
(add-edge mg-den "a" "b") (add-edge mg-den "a" "b")
(cassert-eq "multigraph density counts parallel" (density mg-den) 2.0)

# =============================================================================
#  Cycle detection
# =============================================================================

(print "\n* graph types: cycle detection")
(each [type-name make-graph add-edge add-node] type-fixtures
  (def g (make-graph))
  (add-edge g "a" "b" 1.0)
  (add-edge g "b" "c" 1.0)
  (cassert-eq (string type-name ": path no cycle")
              (has-cycle? g) false)
  (add-edge g "c" "a" 1.0)
  (cassert-eq (string type-name ": triangle has cycle")
              (has-cycle? g) true))

# =============================================================================
#  Topological sort (directed only)
# =============================================================================

(print "\n* directed types: topological sort")
(each [type-name make-graph add-edge add-node]
    (filter (fn [t] (or (= (t 0) :digraph)
                        (= (t 0) :multidigraph))) type-fixtures)
  (def g (make-graph))
  (add-edge g "a" "b" 1.0)
  (add-edge g "b" "c" 1.0)
  (def ts (topological-sort g))
  (cassert-eq (string type-name ": topological sort")
              (set-eq ts @["a" "b" "c"]) true))

# Cyclic returns nil
(def cyc (make-digraph @[] @{}))
(add-edge cyc "a" "b" 1)
(add-edge cyc "b" "c" 1)
(add-edge cyc "c" "a" 1)
(cassert-eq "cyclic topological sort nil"
            (topological-sort cyc) nil)

# =============================================================================
#  Unified edge-weight
# =============================================================================

(print "\n* graph types: unified edge-weight")
# For weighted types
(each [type-name make-graph add-edge add-node] type-fixtures
  (when (or (= type-name :digraph)
            (= type-name :weighted-graph))
    (def g (make-graph))
    (add-edge g "a" "b" (if (= type-name :weighted-graph) 2.5 1))
    (def w (edge-weight g "a" "b"))
    (if (= type-name :weighted-graph)
      (cassert-eq (string type-name ": edge-weight returns weight") w 2.5)
      (cassert-eq (string type-name ": edge-weight returns weight for digraph") w 1))))

# For unweighted graphs, edge-weight returns nil
(each [type-name make-graph add-edge add-node] type-fixtures
  (when (not (or (= type-name :digraph) (= type-name :weighted-graph)))
    (def g (make-graph))
    (add-edge g "a" "b" 1.0)
    (cassert-eq (string type-name ": edge-weight returns nil for unweighted")
                (edge-weight g "a" "b") nil)))

(print-results)
