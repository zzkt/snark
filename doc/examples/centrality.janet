# Examples


(import graphs :prefix "")



# A three-leaf star where the centre is the only vertex that is central by every degree-based metric.


(def star (make-graph))
(add-nodes star ["c" "l1" "l2" "l3"])
(add-edges star [["c" "l1"] ["c" "l2"] ["c" "l3"]])

(def deg (degree-centrality star))
(def eig (eigenvector-centrality star))
(def harm (harmonic-centrality star))

(printf "degree\t c=%p l1=%.5f" (get deg "c") (get deg "l1"))
(printf "eigenvector\t c=%p l1=%.5f" (get eig "c") (get eig "l1"))
(printf "harmonic\t c=%p l1=%p" (get harm "c") (get harm "l1"))



# #+RESULTS:
# : degree	 c=1 l1=0.33333
# : eigenvector	 c=1 l1=0.57735
# : harmonic	 c=3 l1=2

# PageRank on a small directed graph. scores are listed in descending order


(def d (make-digraph))
(add-nodes d ["a" "b" "c"])
(add-edges d [["a" "b"] ["b" "c"] ["c" "a"] ["b" "a"]])

(def pr (pagerank d))
(printf "pagerank (descending): %s"
        (string/join (map |(string/format "%.3f" $0)
                          (sort (values pr) >)) " "))



# #+RESULTS:
# : pagerank (descending): 0.397 0.388 0.215

# In-degree and out-degree centrality on a directed graph (source and sink)


(def d2 (make-digraph))
(add-nodes d2 ["s" "m" "t"])
(add-edges d2 [["s" "m"] ["m" "t"] ["s" "t"]])

(def din (degree-in-centrality d2))
(def dout (degree-out-centrality d2))

(printf "in-centrality:  s=%.3f m=%.3f t=%.3f"
        (get din "s") (get din "m") (get din "t"))

(printf "out-centrality: s=%.3f m=%.3f t=%.3f"
        (get dout "s") (get dout "m") (get dout "t"))



# #+RESULTS:
# : in-centrality:  s=0.000 m=0.500 t=1.000
# : out-centrality: s=1.000 m=0.500 t=0.000

# Closeness centrality on a path graph where middle nodes are closest to all others


(def p4 (make-graph))
(add-nodes p4 ["a" "b" "c" "d"])
(add-edges p4 [["a" "b"] ["b" "c"] ["c" "d"]])

(def cc (closeness-centrality-all p4))
(printf "closeness: a=%.3f b=%.3f c=%.3f d=%.3f"
        (get cc "a") (get cc "b") (get cc "c") (get cc "d"))



# #+RESULTS:
# : closeness: a=0.500 b=0.750 c=0.750 d=0.500

# Katz centrality on a small graph using default parameters (alpha = 1/(max_degree+1)


(def g (make-graph))
(add-nodes g ["a" "b" "c" "d"])
(add-edges g [["a" "b"] ["b" "c"] ["c" "d"] ["d" "a"] ["a" "c"]])

(def katz (katz-centrality g))
(printf "katz: a=%.3f b=%.3f c=%.3f d=%.3f"
        (get katz "a") (get katz "b") (get katz "c") (get katz "d"))



# #+RESULTS:
# : katz: a=3.000 b=2.500 c=3.000 d=2.500

# Percolation centrality on a graph with source nodes.


(def g2 (make-graph))
(add-nodes g2 ["s1" "s2" "m1" "m2" "t"])
(add-edges g2 [["s1" "m1"] ["s2" "m2"] ["m1" "t"] ["m2" "t"] ["m1" "m2"]])

(def settlement @{"s1" 1.0 "s2" 0.5 "m1" 0.0 "m2" 0.0 "t" 0.0})
(def perc (percolation-centrality g2 settlement))

(printf "percolation: s1=%.3f s2=%.3f m1=%.3f m2=%.3f t=%.3f"
        (get perc "s1") (get perc "s2") (get perc "m1") (get perc "m2") (get perc "t"))



# #+RESULTS:
# : percolation: s1=0.000 s2=0.000 m1=0.778 m2=0.556 t=0.000

# Betweenness centrality on a bridge graph where the two bridge nodes score equally.


(def bridge (make-graph))
(add-nodes bridge ["a" "b" "c" "d" "e"])
(add-edges bridge [["a" "b"] ["b" "c"] ["c" "d"] ["d" "e"] ["b" "d"]])

(def bc (betweenness-centrality bridge))
(printf "betweenness\n a=%.3f\n b=%.3f\n c=%.3f\n d=%.3f\n e=%.3f"
        (get bc "a") (get bc "b") (get bc "c") (get bc "d") (get bc "e"))
