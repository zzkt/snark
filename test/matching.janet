# -*- mode: janet; -*-
# matching: matching predicates, greedy maximal matching, clique enumeration

(import ./helper :prefix "")
(import ../graphs :prefix "")
(import ./data/graphs :prefix "fx-")

(defn mem? [arr v]
  (var f false)
  (each a arr (when (= a v) (set f true)))
  f)

(def path4 (make-graph))
(add-nodes path4 @["a" "b" "c" "d"])
(add-edges path4 @[["a" "b"] ["b" "c"] ["c" "d"]])

(cassert-eq "single edge is a matching" (is-matching? path4 @[@["a" "b"]]) true)
(cassert-eq "disjoint edges are a matching" (is-matching? path4 @[@["a" "b"] @["c" "d"]]) true)
(cassert-eq "shared endpoint not a matching" (is-matching? path4 @[@["a" "b"] @["b" "c"]]) false)
(cassert-eq "duplicate edge not a matching" (is-matching? path4 @[@["a" "b"] @["a" "b"]]) false)
(cassert-eq "empty matching is a matching" (is-matching? path4 @[]) true)
(cassert-eq "pair that is not an edge is not a matching" (is-matching? path4 @[@["a" "d"]]) false)
(cassert-eq "pair on absent vertices is not a matching" (is-matching? path4 @[@["x" "y"]]) false)
(cassert-eq "bogus pair is not maximal" (is-maximal-matching? path4 @[@["a" "d"]]) false)
(cassert-eq "perfect on 4-path" (is-perfect-matching? path4 @[@["a" "b"] @["c" "d"]]) true)
(cassert-eq "incomplete not perfect" (is-perfect-matching? path4 @[@["a" "b"]]) false)
(cassert-eq "non-matching not perfect" (is-perfect-matching? path4 @[@["a" "b"] @["b" "c"]]) false)

(def mm (maximal-matching path4))
(cassert-eq "maximal matching is a matching" (is-matching? path4 mm) true)
(cassert-eq "maximal matching is maximal" (is-maximal-matching? path4 mm) true)
(cassert-eq "maximal matching on 4-path covers >=1 edge" (> (length mm) 0) true)

(def empty-g (make-graph))
(cassert-eq "empty maximal matching" (length (maximal-matching empty-g)) 0)

(cassert-eq "empty non-matching is not maximal" (is-maximal-matching? path4 @[]) false)
(cassert-eq "path4 non-matching is not maximal" (is-maximal-matching? path4 @[@["a" "b"]]) false)

# clique enumeration
(def tri (make-graph))
(add-nodes tri @["x" "y" "z" "w"])
(add-edges tri @[["x" "y"] ["y" "z"] ["x" "z"] ["z" "w"]])

(cassert-eq "maximal cliques count (triangle + pendant)" (length (find-cliques tri)) 2)
(cassert-eq "node clique number of hub" (node-clique-number tri "x") 3)
(cassert-eq "node clique number of pendant" (node-clique-number tri "w") 2)
(cassert-eq "all cliques count" (number-of-cliques tri) 10)

(def no-edge (make-graph))
(each n @["p" "q"] (add-node no-edge n))
(cassert-eq "maximal cliques of edgeless" (length (find-cliques no-edge)) 2)
(cassert-eq "all cliques of edgeless graph" (number-of-cliques no-edge) 3)

# clique dedupe
(def weird (make-graph))
(add-nodes weird @["a,b" "x" 7 "sym"])
(each a @["a,b" "x" 7 "sym"]
  (each b @["a,b" "x" 7 "sym"]
    (when (not= a b) (add-edge weird a b))))

(cassert-eq "labeled maximal cliques dedupe to one" (length (find-cliques weird)) 1)
(cassert-eq "labeled all cliques = 2^4" (length (find-all-cliques weird)) 16)

# labels 1 and "1" must not be merged
(def mixed (make-graph))
(add-node mixed 1)
(add-node mixed "1")
(add-edge mixed 1 "1")
(def mc (find-cliques mixed))
(cassert-eq "mixed-type maximal clique size 2" (length (mc 0)) 2)


(print "\n* bipartite matching (Hall, Thm 8.3.4; Kuhn)")

(def kb21 (make-graph))
(add-nodes kb21 @["a" "b" 1 2 3])
(add-edges kb21 ["a" 1][ "b" 1])

(cassert-eq "K_{2,3} fails Hall" (satisfies-hall-condition? kb21 @["a" "b"] @[1 2 3]) false)
(cassert-eq "K_{2,3} max matching size 1" (length (maximum-bipartite-matching kb21 @["a" "b"] @[1 2 3])) 1)

(def k22 (make-graph))
(each n @[1 2 "x" "y"] (add-node k22 n))
(add-edge k22 1 "x") (add-edge k22 1 "y")
(add-edge k22 2 "x") (add-edge k22 2 "y")
(def k22s (bipartite-sets k22))

(cassert-eq "K_{2,2} sides cover all 4 vertices"
  (+ (length (k22s :x)) (length (k22s :y))) 4)
(cassert-eq "K_{2,2} max matching size 2"
  (length (maximum-bipartite-matching k22 (k22s :x) (k22s :y))) 2)
(cassert-eq "K_{2,2} perfect matching"
            (bipartite-perfect-matching? k22 (k22s :x) (k22s :y)) true)
(cassert-eq "K_{2,2} satisfies Hall"
            (satisfies-hall-condition? k22 (k22s :x) (k22s :y)) true)

(def sq (make-graph))
(add-nodes sq @["a" "b" "c" "d"])
(add-edges sq ["a" "b"] ["b" "c"] ["c" "d"] ["d" "a"])

(def sqs (bipartite-sets sq))
(cassert-eq "C4 is bipartite"
            (not (nil? sqs)) true)
(cassert-eq "C4 has a perfect matching"
            (bipartite-perfect-matching? sq (sqs :x) (sqs :y)) true)

(def tri2 (make-graph))
(add-nodes tri2 @["p" "q" "r"])
(add-edges tri2 ["p" "q"] ["q" "r"] ["p" "r"])

(cassert-eq "triangle is not bipartite"
            (nil? (bipartite-sets tri2)) true)

(print "\n* vertex cover & independent set (König)")

# Path P4: min vertex cover size 2, max independent set size 2
(def p4c (make-graph))
(add-nodes p4c "a" "b" "c" "d")
(add-edges p4c ["a" "b"] ["b" "c"] ["c" "d"])

(def p4s (bipartite-sets p4c))
(def p4cov (minimum-vertex-cover p4c (p4s :x) (p4s :y)))
(cassert-eq "P4 min vertex cover size 2" (length p4cov) 2)

(def p4is (maximum-independent-set p4c (p4s :x) (p4s :y)))
(cassert-eq "P4 max independent set size 2"
            (length p4is) 2)
(cassert-eq "P4 cover is a vertex cover"
            (all (fn [e] (or (mem? p4cov (e 0)) (mem? p4cov (e 1))))
                 @[["a" "b"] ["b" "c"] ["c" "d"]]) true)

# C4 (even cycle): cover size 2, independent set size 2
(def c4c (make-graph))
(add-nodes c4c @["a" "b" "c" "d"])
(add-edges c4c ["a" "b"] ["b" "c"] ["c" "d"] ["d" "a"])

(def c4sides (bipartite-sets c4c))
(cassert-eq "C4 min vertex cover size 2"
  (length (minimum-vertex-cover c4c (c4sides :x) (c4sides :y))) 2)
(cassert-eq "C4 max independent set size 2"
  (length (maximum-independent-set c4c (c4sides :x) (c4sides :y))) 2)

# K_{2,2}: cover size 2
(def k22c (make-graph))
(add-nodes k22c 1 2 "x" "y")
(add-edges k22c [1 "x"] [1 "y"] [2 "x"] [2 "y"])

(def k22cs (bipartite-sets k22c))
(cassert-eq "K_{2,2} min vertex cover size 2"
  (length (minimum-vertex-cover k22c (k22cs :x) (k22cs :y))) 2)

# Isolated vertex is not in any cover
(def iso (make-graph))
(add-nodes iso @["a" "b" "c"])
(add-edge iso "a" "b")
(def isos (bipartite-sets iso))
(def isocov (minimum-vertex-cover iso (isos :x) (isos :y)))

(cassert-eq "isolated vertex not in cover"
            (mem? isocov "c") false)
(cassert-eq "isolated in independent set"
            (mem? (maximum-independent-set iso (isos :x) (isos :y)) "c") true)

(print "\n* general (non-bipartite) matching — Edmonds blossom")

# empty graph
(cassert-eq "empty graph max matching 0"
            (length (maximum-matching (make-graph))) 0)

# single isolated vertex
(def solo (make-graph))
(add-node solo "z")
(cassert-eq "isolated vertex max matching 0"
            (length (maximum-matching solo)) 0)

# single edge
(def se (make-graph))
(add-nodes se "x" "y")
(add-edge se "x" "y")

(cassert-eq "single edge max matching 1"
            (length (maximum-matching se)) 1)
(cassert-eq "single edge matching is valid"
            (is-matching? se (maximum-matching se)) true)

# triangle K3
(cassert-eq "K3 max matching 1" (length (maximum-matching tri2)) 1)

# path P4 (4 vertices, perfect matching exists)
(cassert-eq "P4 max matching 2" (length (maximum-matching path4)) 2)

# cycle C5 (5 vertices, matching covers 4 of 5)
(def c5t (make-graph))
(add-nodes c5t @["1" "2" "3" "4" "5"])
(add-edges c5t ["1" "2"] ["2" "3"] ["3" "4"] ["4" "5"] ["5" "1"])

(cassert-eq "C5 max matching 2" (length (maximum-matching c5t)) 2)

# K4 (4 vertices, complete, perfect matching)
(def k4t (fx-complete 4 fx-idx->int))
(cassert-eq "K4 max matching 2"
            (length (maximum-matching k4t)) 2)
(cassert-eq "K4 is perfect matching graph"
            (is-perfect-matching-graph? k4t) true)

# K5 (5 vertices, covers 4 of 5)
(def k5t (fx-complete 5 fx-idx->int))
(cassert-eq "K5 max matching 2"
            (length (maximum-matching k5t)) 2)
(cassert-eq "K5 not perfect (odd)"
            (is-perfect-matching-graph? k5t) false)

# K3 with outer ring has a perfect matching ({a-b, c-d, e-f})
(def k3ring (make-graph))
(add-nodes k3ring "a" "b" "c" "d" "e" "f")
(add-edges k3ring [["a" "b"] ["b" "c"] ["c" "a"]
                   ["c" "d"] ["d" "e"] ["e" "f"] ["f" "a"]])

(cassert-eq "K3-ring matching 3"
            (length (maximum-matching k3ring)) 3)
(cassert-eq "K3-ring matching is valid"
            (is-matching? k3ring (maximum-matching k3ring)) true)

# Petersen graph

(def petersen (fx-petersen))

(cassert-eq "Petersen max matching 5"
            (length (maximum-matching petersen)) 5)
(cassert-eq "Petersen matching is valid"
            (is-matching? petersen (maximum-matching petersen)) true)
(cassert-eq "Petersen is perfect"
            (is-perfect-matching-graph? petersen) true)


#  blossom contraction on disjoint odd cycles
(defn- disjoint-cycles [k m]
  (def g (make-graph))
  (var base 0)
  (for c 0 k
    (for i 0 m (add-node g (+ base i)))
    (for i 0 m
      (add-edge g (+ base i) (+ base (mod (+ i 1) m))))
    (set base (+ base m)))
  g)

(each k [1 2 3 4]
  (each m [3 5 7 9]
    (def g (disjoint-cycles k m))
    (def want (* k (div m 2)))
    (def mm (maximum-matching g))
    (cassert-eq (string k "xC" m " max matching " want)
                (length mm) want)
    (cassert-eq (string k "xC" m " matching is valid")
                (is-matching? g mm) true)))

# Explicit size checks
(def two-c3 (disjoint-cycles 2 3))
(cassert-eq "2 disjoint triangles: size 2"
            (length (maximum-matching two-c3)) 2)
(cassert-eq "2 disjoint triangles: valid"
            (is-matching? two-c3 (maximum-matching two-c3)) true)
(cassert-eq "2 disjoint triangles: not perfect"
            (is-perfect-matching-graph? two-c3) false)

# A C5 matching must be made of real edges
(def c5t2 (disjoint-cycles 1 5))
(cassert-eq "C5 size 2"
            (length (maximum-matching c5t2)) 2)
(cassert-eq "C5 matching is valid"
            (is-matching? c5t2 (maximum-matching c5t2)) true)


(defn hall-star [n]
  (def g (make-graph))
  (var xs @[])
  (for i 0 n
    (def x (string "x" i))
    (array/push xs x)
    (add-node g x))
  (add-node g "y")
  (add-edge g (xs 0) "y")
  [g xs])

(each n [2 3 8 30 31 32 33 63 64]
  (def [sg xs] (hall-star n))
  (cassert-eq (string "star |X|=" n " fails Hall (32-bit shift boundary)")
    (satisfies-hall-condition? sg xs @["y"]) false))

# And in the positive direction at the same sizes
(defn hall-complete [n]
  (def g (make-graph))
  (var xs @[])
  (var ys @[])
  (for i 0 n
    (def x (string "x" i))
    (def y (string "y" i))
    (array/push xs x)
    (array/push ys y)
    (add-node g x)
    (add-node g y)
    (for j 0 n (add-edge g x (string "y" j))))
  [g xs ys])

(each n [2 5 30 31 32 33]
  (def [cg xs ys] (hall-complete n))
  (cassert-eq (string "K_{" n "," n "} satisfies Hall")
    (satisfies-hall-condition? cg xs ys) true))

(print-results)
