# Examples


(import graphs :prefix "")



# The path on four nodes, =a b c d=, which is bipartite


(def p4 (make-graph))
(add-nodes p4 ["a" "b" "c" "d"])
(add-edges p4 [["a" "b"] ["b" "c"] ["c" "d"]])

(def mm (maximum-matching p4))
(def sides (bipartite-sets p4))

(printf "max matching size (P4): %p" (length mm))

(printf "blossom vs bipartite agree: %p"
        (= (length mm)
           (length (maximum-bipartite-matching
                    p4
                    (get sides :x) (get sides :y)))))

(printf "min vertex cover (König): %p"
        (length (minimum-vertex-cover p4 (get sides :x) (get sides :y))))



# #+RESULTS:
# : max matching size (P4): 2
# : blossom vs bipartite agree: true
# : min vertex cover (König): 2

# Cliques of a triangle. The empty clique, three singletons, three edges, and the triangle itself.


(def tri (make-graph))
(add-nodes tri ["a" "b" "c"])
(add-edges tri [["a" "b"] ["b" "c"] ["a" "c"]])

(printf "cliques: %p" (number-of-cliques tri))
(printf "clique number of node 'a' is %p" (node-clique-number tri "a"))



# #+RESULTS:
# : cliques: 8
# : clique number of node 'a' is 3

# Matching validation and greedy maximal matching on a path


(def p4 (make-graph))
(add-nodes p4 ["a" "b" "c" "d"])
(add-edges p4 [["a" "b"] ["b" "c"] ["c" "d"]])

(def greedy (maximal-matching p4))

(printf "greedy maximal matching: %s"
        (string/join (map (fn [e] (string (e 0) "-" (e 1))) greedy) " "))
(printf "is matching? %p" (is-matching? p4 greedy))
(printf "is maximal matching? %p" (is-maximal-matching? p4 greedy))
(printf "is perfect matching? %p" (is-perfect-matching? p4 greedy))
(printf "is perfect matching graph? %p" (is-perfect-matching-graph? p4))



# #+RESULTS:
# : greedy maximal matching: a-b c-d
# : is matching? true
# : is maximal matching? true
# : is perfect matching? true
# : is perfect matching graph? true

# Bipartite matching, Hall's condition, and König duality.


(def bip (make-graph))
(add-nodes bip ["x1" "x2" "x3" "y1" "y2" "y3"])
(add-edges bip [["x1" "y1"] ["x1" "y2"] ["x2" "y2"]
                ["x2" "y3"] ["x3" "y1"] ["x3" "y3"]])

(def sides (bipartite-sets bip))
(def mm (maximum-bipartite-matching bip (get sides :x) (get sides :y)))
(def mvc (minimum-vertex-cover bip (get sides :x) (get sides :y)))
(def mis (maximum-independent-set bip (get sides :x) (get sides :y)))

(printf "bipartite sides: x=%s | y=%s"
        (string/join (get sides :x) ",") (string/join (get sides :y) ","))
(printf "max bipartite matching size: %p" (length mm))
(printf "perfect matching? %p"
        (bipartite-perfect-matching? bip (get sides :x) (get sides :y)))
(printf "Hall condition satisfied? %p"
        (satisfies-hall-condition? bip (get sides :x) (get sides :y)))
(printf "min vertex cover: %p" mvc)
(printf "max independent set: %p" mis)



# #+RESULTS:
# : bipartite sides: x=x1,x2,x3 | y=y1,y2,y3
# : max bipartite matching size: 3
# : perfect matching? true
# : Hall condition satisfied? true
# : min vertex cover: @["x1" "x2" "x3"]
# : max independent set: @["y1" "y2" "y3"]

# Maximal and all cliques on a graph with overlapping triangles


(def g2 (make-graph))
(add-nodes g2 ["a" "b" "c" "d"])
(add-edges g2 [["a" "b"] ["b" "c"] ["c" "a"] ["c" "d"] ["d" "a"]])

(def maxc (find-cliques g2))
(def allc (find-all-cliques g2))

(printf "maximal cliques (%p): %s"
        (length maxc)
        (string/join
         (map (fn [c] (string "{" (string/join (map string c) ",") "}"))
              maxc) " "))

(printf "all cliques count: %p" (length allc))
