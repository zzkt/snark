# -*- mode: janet; -*-
# graphs/test/metrics: density, diameter, radius, center, eccentricity, etc

(import ./helper :prefix "")
(import ../graphs/simple-graph :prefix "")
(import ../graphs/digraph :as dg)
(import ../graphs/multigraph :prefix "")
(import ../graphs/metrics :prefix "")
(import ../graphs :prefix "gr-") # prefixed for generic/typed distinction
(import ./data/graphs :prefix "fx-")

(print "\n* density")

(def gempty (make-graph))
(cassert-eq "density empty" (graph-density gempty) 0.0)
(graph-add-node gempty "x")
(cassert-eq "density single node" (graph-density gempty) 0.0)

# path 0-1-2-3 -> 2 edges / (4*3) = 1/6
(def gpath (fx-path 4))
(cassert-eq "density path4" (graph-density gpath) 0.5)

# K4 -> 12/12 = 1.0
(def gk4 (fx-complete 4))
(cassert-eq "density K4" (graph-density gk4) 1.0)

(print "\n* eccentricity, diameter, radius, center, and average path length")

(cassert-eq "eccentricity path4 at node 1" (eccentricity gpath "1") 2)
(cassert-eq "eccentricity path4 at node 0" (eccentricity gpath "0") 3)
(cassert-eq "eccentricity single node nil" (eccentricity gempty "x") nil)

(cassert-eq "diameter path4" (graph-diameter gpath) 3)
(cassert-eq "diameter empty" (graph-diameter gempty) 0)

(cassert-eq "radius path4" (graph-radius gpath) 2)
(cassert-eq "radius empty" (graph-radius gempty) nil)

(cassert-arr-eq "center path4" (graph-center gpath) @["1" "2"])

(def gtruly-empty (make-graph))
(cassert-eq "center empty array"
            (= (length (graph-center gtruly-empty)) 0) true)

(cassert-set-eq "eccentricity table covers every node"
                (sort (keys (gr-graph-eccentricity gpath)))
                @["0" "1" "2" "3"])

(cassert-set-eq "eccentricity table values path4"
                (seq [k :in (keys (gr-graph-eccentricity gpath))]
                     [k (get (gr-graph-eccentricity gpath) k)])
                [["0" 3] ["1" 2] ["2" 2] ["3" 3]])

(cassert-eq "eccentricity table empty graph"
            (= (length (gr-graph-eccentricity gtruly-empty)) 0) true)

# graph-radius ignores a zero eccentricity, graph-center accepts it
(def giso (make-graph))
(gr-add-nodes  giso @["a" "b" "lonely"])
(gr-add-edges giso @[["a" "b"]])
(cassert-eq "eccentricity table: an isolated node reads 0, not absent"
            (get (gr-graph-eccentricity giso) "lonely") 0)
# Order-insensitive: these name a SET of vertices. The array comes out in the
# eccentricity table's key order, which is Janet's hash-bucket order rather than
# `(nodes g)` order -- stable for a given key set, but not part of the promise.
(cassert-set-eq "periphery path4 is the far ends" (gr-graph-periphery gpath) @["0" "3"])
(cassert-set-eq "periphery skips the isolated node" (gr-graph-periphery giso) @["a" "b"])
(cassert-eq "periphery empty array"
            (= (length (gr-graph-periphery gtruly-empty)) 0) true)


# path4 distances: ordered reachable pairs sum 20 over 12 pairs -> 5/3
(cassert-eq "average path length path4"
            (< (math/abs (- (average-path-length gpath) (/ 20 12))) 1e-12) true)
(cassert-eq "average path length single node" (average-path-length gempty) 0.0)

(print "\n* disconnected graph metrics")

# two separate edges: :0-:1 and :2-:3
(def gdisc (fx-from-pairs @["0" "1" "2" "3"] [["0" "1"] ["2" "3"]]))
(cassert-eq "diameter disconnected nil" (graph-diameter gdisc) nil)
(cassert-eq "radius disconnected = min positive ecc" (graph-radius gdisc) 1)
(cassert-eq "eccentricity disconnected at node 0" (eccentricity gdisc "0") 1)

(print "\n* directed metrics (digraph)")

(def gw (dg/make-digraph @[] @{}))
(dg/digraph-add-node gw "a")
(dg/digraph-add-node gw "b")
(dg/digraph-add-node gw "c")
(dg/digraph-add-edge gw "a" "b" 1)
(dg/digraph-add-edge gw "a" "c" 1)
(dg/digraph-add-edge gw "b" "c" 1)
# c reaches nothing -> pair c->{a,b} unreachable -> diameter nil (directed)
(cassert-eq "directed diameter wedge nil" (graph-diameter gw) nil)
(cassert-eq "directed eccentricity root nil-leaf" (eccentricity gw "c") nil)

(print "\n* clustering coefficients")

# wedge digraph a->b, a->c, b->c (out-neighbours)
(cassert-eq "cc wedge a = 0.5" (clustering-coefficient gw "a") 0.5)
(cassert-eq "cc wedge c (no out) = 0.0" (clustering-coefficient gw "c") 0.0)
(cassert-eq "avg-cc wedge = 1/6"
            (< (math/abs (- (average-clustering-coefficient gw) (/ 1 6))) 1e-12)
            true)

# global transitivity over directed neighbours: wedge a->b, a->c, b->c
# u=a has two neighbours; only pair (b,c) closes a triangle -> 1/2
(cassert-eq "global-cc directed wedge = 0.5"
            (global-clustering-coefficient gw) 0.5)

# symbol-node graph regression: graph family accepts symbol ids, so the
# metrics must not demand :string nodes.
(def gsym (make-graph))
(gr-add-nodes gsym @["a" "b" "c"])
(gr-add-edges gsym @[["a" "b"] ["a" "c"] ["b" "c"]])
(cassert-eq "cc triangle graph symbol = 1.0" (clustering-coefficient gsym "a") 1.0)
(cassert-eq "avg-cc triangle graph symbol = 1.0"
            (average-clustering-coefficient gsym) 1.0)
(cassert-eq "eccentricity symbol node triangle" (eccentricity gsym "a") 1)

(print "\n* undirected transitivity")

# triangle graph = 1.0, path4 = 0.0, empty = 0
(cassert-eq "transitivity triangle graph" (graph-transitivity gsym) 1.0)
(cassert-eq "transitivity path4" (graph-transitivity gpath) 0.0)
(cassert-eq "transitivity empty" (graph-transitivity gempty) 0)
(cassert-eq "graph-average-clustering triangle" (graph-average-clustering gsym) 1.0)
(cassert-eq "graph-average-clustering path4"
            (< (math/abs (- (graph-average-clustering gpath) 0.0)) 1e-12) true)

# self-loops are excluded from neighbour sets so transitivity stays in [0,1]:
# a <-> b plus a self-loop a->a must still give 0.0 (no triangle), not 1.0.
(def gsl (dg/make-digraph @[] @{}))
(each u @["a" "b"] (dg/digraph-add-node gsl u))
(dg/digraph-add-edge gsl "a" "b" 1)
(dg/digraph-add-edge gsl "b" "a" 1)
(dg/digraph-add-edge gsl "a" "a" 1)
(cassert-eq "global-cc self-loop excluded" (global-clustering-coefficient gsl) 0.0)
(cassert-eq "undirected transitivity self-loop excluded"
            (transitivity-undirected gsl) 0.0)

# multidigraph average clustering over a 2-edge mutual pair is 0.0 (k<2)
(def mg2 (make-multigraph))
(gr-add-edges mg2 @[["x" "y"] ["y" "x"]])
(cassert-eq "multidigraph avg clustering mutual pair"
            (multidigraph-average-clustering mg2) 0.0)

# ==============================================================================
#  girth, triangles, independence, vertex cover, spanning trees, toughness
#  Reference values are the Petersen graph (House of Graphs 660)
# ==============================================================================

(print "\n* girth and shortest cycle")

(cassert-eq "girth petersen is 5" (girth (fx-petersen)) 5)
(cassert-eq "girth C5" (girth (fx-cycle 5)) 5)
(cassert-eq "girth C6" (girth (fx-cycle 6)) 6)
(cassert-eq "girth K4 is 3" (girth (fx-complete 4)) 3)
(cassert-eq "girth triangle" (girth (fx-cycle 3)) 3)
(cassert-eq "girth of a tree is nil" (girth (fx-path 5)) nil)
(cassert-eq "girth of the empty graph is nil" (girth gempty) nil)
(cassert-eq "girth of a forest is nil"
            (girth (fx-from-pairs ["a" "b" "c" "d"]
                                  [["a" "b"] ["c" "d"]]))
            nil)
# two triangles in one graph: the shorter cycle is 3, not 6
(cassert-eq "girth is the shortest cycle, not the graph's diameter"
            (girth (fx-from-pairs (map string (range 6))
                                  [["0" "1"] ["1" "2"] ["2" "0"]
                                   ["3" "4"] ["4" "5"] ["5" "3"]])) 3)

(defn- a-cycle? [g cyc]
  (def seen @{})
  (each u cyc (put seen u true))
  (var ok (= (length cyc) (length seen)))
  (loop [i :range [0 (length cyc)]]
    (when (and ok (not (gr-has-edge? g (cyc i) (cyc (mod (+ i 1) (length cyc))))))
      (set ok false)))
  ok)

(def gcyc (shortest-cycle (fx-petersen)))
(cassert-eq "shortest-cycle returns a 5-cycle" (length gcyc) 5)
(cassert-eq "shortest-cycle closes on itself" (a-cycle? (fx-petersen) gcyc) true)
(cassert-eq "shortest-cycle of a tree is nil" (shortest-cycle (fx-path 5)) nil)

(print "\n* triangle count")

(cassert-eq "triangle-count K4 is 4" (triangle-count (fx-complete 4)) 4)
(cassert-eq "triangle-count petersen is 0" (triangle-count (fx-petersen)) 0)
(cassert-eq "triangle-count C5 is 0" (triangle-count (fx-cycle 5)) 0)
(cassert-eq "triangle-count one triangle is 1"
            (triangle-count (fx-cycle 3)) 1)
(cassert-eq "triangle-count two triangles is 2"
            (triangle-count (fx-from-pairs (map string (range 6))
                                           [["0" "1"] ["1" "2"] ["2" "0"]
                                            ["3" "4"] ["4" "5"] ["5" "3"]])) 2)
(cassert-eq "triangle-count K5 is 10" (triangle-count (fx-complete 5)) 10)
(cassert-eq "triangle-count of a tree is 0" (triangle-count (fx-path 5)) 0)
(cassert-eq "triangle-count of the empty graph is 0"
            (triangle-count (make-graph)) 0)

(print "\n* bipartite?")

(cassert-eq "bipartite? C6" (bipartite? (fx-cycle 6)) true)
(cassert-eq "bipartite? C5 is odd" (bipartite? (fx-cycle 5)) false)
(cassert-eq "bipartite? tree" (bipartite? (fx-path 5)) true)
(cassert-eq "bipartite? K4" (bipartite? (fx-complete 4)) false)
(cassert-eq "bipartite? petersen" (bipartite? (fx-petersen)) false)
(cassert-eq "bipartite? empty" (bipartite? gempty) true)
(cassert-eq "bipartite? a single edge" (bipartite? (fx-path 2)) true)

# disconnected: both parts must be checked
(cassert-eq "bipartite? sees an odd cycle in a second component"
            (bipartite? (fx-from-pairs (map string (range 8))
                                       [["0" "1"] ["1" "2"] ["2" "3"]
                                        ["3" "4"] ["4" "0"] ["5" "6"]
                                        ["6" "7"] ["7" "5"]])) false)

(cassert-eq "bipartite? two even components"
            (bipartite? (fx-from-pairs (map string (range 8))
                                       [["0" "1"] ["1" "2"] ["2" "3"]
                                        ["3" "0"] ["4" "5"] ["5" "6"]
                                        ["6" "7"] ["7" "4"]])) true)


(print "\n* independence number and maximum independent set")

(cassert-eq "independence-number petersen is 4"
            (independence-number (fx-petersen)) 4)
(cassert-eq "independence-number K4 is 1"
            (independence-number (fx-complete 4)) 1)
(cassert-eq "independence-number C6 is 3"
            (independence-number (fx-cycle 6)) 3)
(cassert-eq "independence-number C5 is 2"
            (independence-number (fx-cycle 5)) 2)
(cassert-eq "independence-number path5 is 3"
            (independence-number (fx-path 5)) 3)
(cassert-eq "independence-number of the empty graph is 0"
            (independence-number (make-graph)) 0)

# alpha and returned set must agree
(defn- independent? [g vs]
  (var ok true)
  (loop [i :range [0 (length vs)]]
    (loop [j :range [(+ i 1) (length vs)]]
      (when (and ok (gr-has-edge? g (vs i) (vs j))) (set ok false))))
  ok)

(defn- covers? [g vs]
  (def chosen @{})
  (each u vs (put chosen u true))
  (var ok true)
  (loop [u :in (gr-nodes g)]
    (loop [v :in (gr-neighbours g u)]
      (when (and ok (not (or (get chosen u) (get chosen v)))) (set ok false))))
  ok)
(each g [(fx-petersen) (fx-cycle 6) (fx-path 7) (fx-complete 4)
         (fx-star 4) (fx-from-pairs ["p"] [])]
  (def vs (maximum-independent-set g))

  (cassert-eq "maximum-independent-set size equals independence-number"
              (length vs) (independence-number g))
  (cassert-eq "maximum-independent-set is independent"
              (independent? g vs) true))

# isolated vertices belong in every maximum independent set.
(def giso (fx-from-pairs ["a" "b" "iso"] [["a" "b"]]))
(def viso (maximum-independent-set giso))
(cassert-eq "maximum independent set with an isolated vertex has size 2"
            (length viso) 2)
(cassert-eq "the isolated vertex is in the maximum independent set"
            (some |(= $ "iso") viso) true)
(cassert-eq "the maximum independent set with an isolated vertex is independent"
            (independent? giso viso) true)


(print "\n* vertex cover")

(cassert-eq "vertex-cover-number petersen is 6"
            (vertex-cover-number (fx-petersen)) 6)
(cassert-eq "vertex-cover-number K4 is 3" (vertex-cover-number (fx-complete 4)) 3)
(cassert-eq "vertex-cover-number C6 is 3" (vertex-cover-number (fx-cycle 6)) 3)
(each g [(fx-petersen) (fx-cycle 5) (fx-path 6) (fx-complete 4)]
  (def cv (minimum-vertex-cover g))

  (cassert-eq "minimum-vertex-cover size equals vertex-cover-number"
              (length cv) (vertex-cover-number g))
  (cassert-eq "minimum-vertex-cover meets every edge" (covers? g cv) true))

# Gallai: alpha + tau = |V|
(cassert-eq "Gallai: alpha + tau = |V| on petersen"
            (+ (independence-number (fx-petersen))
               (vertex-cover-number (fx-petersen)))
            (length (gr-nodes (fx-petersen))))


(print "\n* spanning tree count")

(cassert-eq "spanning-tree-count petersen is 2000"
            (spanning-tree-count (fx-petersen)) 2000)
(cassert-eq "spanning-tree-count Cn is n"
            (spanning-tree-count (fx-cycle 7)) 7)
(cassert-eq "spanning-tree-count K4 is 16"
            (spanning-tree-count (fx-complete 4)) 16)
(cassert-eq "spanning-tree-count of a tree is 1"
            (spanning-tree-count (fx-path 6)) 1)

# a forest with two components is disconnected (no spanning tree)
(cassert-eq "spanning-tree-count of a two-component forest is 0"
            (spanning-tree-count (fx-from-pairs (map string (range 4))
                                                [["0" "1"] ["2" "3"]])) 0)

(cassert-eq "spanning-tree-count of a disconnected graph is 0"
            (spanning-tree-count
             (fx-from-pairs (map string (range 6))
                            [["0" "1"] ["2" "3"] ["4" "5"]])) 0)

(cassert-eq "spanning-tree-count of the empty graph is 1"
            (spanning-tree-count gempty) 1)
(cassert-eq "spanning-tree-count K5 is 125"
            (spanning-tree-count (fx-complete 5)) 125)


(print "\n* toughness")

(cassert-eq "toughness petersen is 4/3"
            (toughness (fx-petersen)) (/ 4.0 3.0))
(cassert-eq "toughness C5 is 1"
            (toughness (fx-cycle 5)) 1)
(cassert-eq "toughness K4 is infinite"
            (toughness (fx-complete 4)) math/inf)
(cassert-eq "toughness of a disconnected graph is 0"
            (toughness (fx-from-pairs
                        (map string (range 6))
                        [["0" "1"] ["2" "3"] ["4" "5"]])) 0.0)

# a star is cut by its centre into k components -> 1/k
(cassert-eq "toughness of a 3-star is 1/3"
            (toughness (fx-star 3)) (/ 1.0 3.0))

# ==============================================================================
#  Reference values from graphs/metrics
# ==============================================================================

(print "\n* reference values: complete graphs")

(loop [n :range [1 7]]
  (cassert-eq (string "independence-number K" n " is 1")
              (independence-number (fx-complete n)) 1))

(loop [n :range [1 7]]
  (cassert-eq (string "toughness K" n " is infinite")
              (toughness (fx-complete n)) math/inf))

# Gallai
(loop [n :range [2 7]]
  (def g (fx-complete n))
  (def cover (minimum-vertex-cover g))
  (cassert-eq (string "Gallai on K" n ": alpha + tau = |V|")
              (+ (independence-number g) (vertex-cover-number g)) n)
  (cassert-eq (string "minimum-vertex-cover of K" n " has n-1 vertices")
              (length cover) (dec n))
  (cassert-eq (string "minimum-vertex-cover of K" n " meets every edge")
              (covers? g cover) true))


(print "\n* reference values: girth on self-loops and parallel edges")

(cassert-eq "girth of a self-loop-only graph is nil"
            (girth (fx-from-pairs ["a" "b" "c"] [["a" "a"] ["b" "b"]])) nil)

(cassert-eq "shortest-cycle of a self-loop-only graph is nil"
            (shortest-cycle (fx-from-pairs ["a" "b" "c"]
                                           [["a" "a"] ["b" "b"]])) nil)

#  C4 with every edge doubled is still girth 4.
(def gdbl (make-multigraph))
(gr-add-edges gdbl @[["a" "b"] ["b" "c"] ["c" "d"] ["d" "a"]
                  ["a" "b"] ["b" "c"] ["c" "d"] ["d" "a"]])

(cassert-eq "girth collapses parallel edges (doubled C4 is 4)"
            (girth gdbl) 4)


(print "\n* Petersen spanning-tree derivation")

(def lap (gr-graph-laplacian (fx-petersen)))
(def lap-n (length lap))

(cassert-eq "petersen Laplacian trace = 30 (spectrum 0, 2^5, 5^4)"
            (sum (seq [i :range [0 lap-n]] (get (lap i) i))) 30)

# trace(L^2) is the squared Frobenius norm, L being symmetric.
(var lap-trace-sq 0.0)
(each row lap (each x row (+= lap-trace-sq (* x x))))

(cassert-eq "petersen Laplacian trace(L^2) = 120 (spectrum 0, 2^5, 5^4)"
            lap-trace-sq 120)

(cassert-eq "matrix-tree: 2^5 * 5^4 / 10 = 2000"
            (/ (* (math/pow 2 5) (math/pow 5 4)) 10) 2000)

(print-results)
