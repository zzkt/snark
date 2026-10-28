# Example


(import graphs :prefix "")



# A triangle with a pendant edge (where node =d= has no triangle but still counts towards the mean)


(def g (make-graph))
(add-nodes g ["a" "b" "c" "d"])
(add-edges g [["a" "b"] ["b" "c"] ["c" "a"] ["c" "d"]])

(printf "density: %.3f" (density g))
(printf "diameter: %p, radius: %p" (graph-diameter g) (graph-radius g))
(printf "avg path length: %.3f" (average-path-length g))
(printf "global clustering: %.3f" (global-clustering-coefficient g))
(printf "mean clustering: %.3f" (average-clustering-coefficient g))



# #+RESULTS:
# : density: 0.667
# : diameter: 2, radius: 1
# : avg path length: 1.333
# : global clustering: 0.600
# : mean clustering: 0.583

# Note: =global-clustering-coefficient= weights each closed triple equally, while =average-clustering-coefficient= averages the per-node coefficients, which is why the two can differ.

# Graph center and periphery on a path graph


(def p5 (make-graph))
(add-nodes p5 ["a" "b" "c" "d" "e"])
(add-edges p5 [["a" "b"] ["b" "c"] ["c" "d"] ["d" "e"]])

(printf "center: %s" (string/join (graph-center p5) ", "))

(printf "periphery: %s" (string/join (graph-periphery p5) ", "))

(printf "eccentricities: a=%p b=%p c=%p d=%p e=%p"
        (eccentricity p5 "a") (eccentricity p5 "b") (eccentricity p5 "c")
        (eccentricity p5 "d") (eccentricity p5 "e"))



# #+RESULTS:
# : center: c
# : periphery: a, e
# : eccentricities: a=4 b=3 c=2 d=3 e=4

# Local clustering coefficient and undirected transitivity on a graph with triangles


(def g2 (make-graph))
(add-nodes g2 ["a" "b" "c" "d" "e"])
(add-edges g2 [["a" "b"] ["b" "c"] ["c" "a"]
               ["c" "d"] ["d" "e"] ["e" "c"]])

(printf "clustering-coefficient(c): %.3f"
        (clustering-coefficient g2 "c"))

(printf "average-clustering: %.3f"
        (average-clustering-coefficient g2))

(printf "global clustering (directed): %.3f"
        (global-clustering-coefficient g2))

(printf "graph-transitivity: %.3f"
        (graph-transitivity g2))

(printf "graph-average-clustering: %.3f"
        (graph-average-clustering g2))



# #+RESULTS:
# : clustering-coefficient(c): 0.333
# : average-clustering: 0.867
# : global clustering (directed): 0.600
# : graph-transitivity: 0.600
# : graph-average-clustering: 0.867

# All-pairs shortest path lengths on a small graph (including self-distances)


(def g4 (make-graph))
(add-nodes g4 ["a" "b" "c" "d"])
(add-edges g4 [["a" "b"] ["b" "c"] ["a" "c"]
               ["c" "d"] ["a" "c"] ["a" "d"]])

(def dists (graph-shortest-path-lengths g4))
(printf "shortest-path-lengths\n %s"
        (string/join (map (fn [[u v d]] (string u "->" v ": " d))
                          (seq [[u du] :pairs dists [v d] :pairs du] @[u v d])) "\n "))
