

#===============================================================================
#  file: doc/examples/centrality.janet
#===============================================================================

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


#===============================================================================
#  file: doc/examples/clustering.janet
#===============================================================================

# Examples

# The classic “two triangles joined by a single edge” instance. Note that each undirected edge is written twice (once per direction) to satisfy the symmetry requirement


(import graphs :prefix "")

(def g (make-digraph))
(add-nodes g "a" "b" "c" "d" "e" "f")
(add-edges g [["a" "b"] ["b" "a"]
              ["a" "c"] ["c" "a"]
              ["b" "c"] ["c" "b"]
              ["d" "e"] ["e" "d"]
              ["d" "f"] ["f" "d"]
              ["c" "d"] ["d" "c"]])

(def community (louvain g))

(printf "Louvain communities: %s"
        (string/join
         (map |(string/join (sort $) ",")
              (values (group-by (fn [n] (get community n))
                                (keys community)))) " | "))



# #+RESULTS:
# : Louvain communities: a,b,c | d,e,f

# Leiden refinement on the same graph which may split or reassign communities from Louvain.


(def g2 (make-digraph))
(add-nodes g2 ["a" "b" "c" "d" "e" "f"])
(add-edges g2 [["a" "b"] ["b" "a"]
               ["a" "c"] ["c" "a"]
               ["b" "c"] ["c" "b"]
               ["d" "e"] ["e" "d"]
               ["d" "f"] ["f" "d"]
               ["c" "d"] ["d" "c"]])

(def community2 (leiden g2))

(printf "Leiden communities: %s"
        (string/join
         (map |(string/join (sort $) ",")
              (values (group-by (fn [n] (get community2 n))
                                  (keys community2)))) " | "))


#===============================================================================
#  file: doc/examples/colouring.janet
#===============================================================================

# Example


(import graphs :prefix "")

(def c4 (make-graph))
(add-nodes c4 ["a" "b" "c" "d"])
(add-edges c4 [["a" "b"] ["b" "c"] ["c" "d"] ["d" "a"]])

(def col (greedy-colour c4))
(printf "χ(C4) = %p  proper? %p" (chromatic-number c4) (proper-colouring? c4 col))

# ex(7, K4): a K_4-free graph on 7 vertices has at most 16 edges.
(printf "ex(7, K4) = %p" (turan-number 7 3))
(printf "2-partite Turán is bipartite: %p" (= 2 (chromatic-number (make-turan-graph 7 2))))


#===============================================================================
#  file: doc/examples/connectivity.janet
#===============================================================================

# Examples


(import graphs :prefix "")



# A path of four nodes. one component, three bridges, two cut vertices.


(def g (make-graph))
(add-nodes g "a" "b" "c" "d")
(add-edges g ["a" "b"] ["b" "c"] ["c" "d"])

(printf "components: %p" (length (connected-components g)))
(printf "bridges: %p" (length (bridges g)))
(printf "articulation points: %s"
        (string/join (sort (map string (articulation-points g))) " "))



# #+RESULTS:
# : components: 1
# : bridges: 3
# : articulation points: b c

# LCA on a rooted tree where =r= is the root, =a= and =b= are its children, and =c= is a child of =a=


(def t (make-digraph))
(add-nodes t "r" "a" "b" "c")
(add-edges t ["r" "a" 1] ["r" "b" 1] ["a" "c" 1])

(def ans (tarjan-lca t "r" @[["c" "b"] ["c" "a"]]))

(printf "LCA(c,b) = %s, LCA(c,a) = %s" (get ans ["c" "b"]) (get ans ["c" "a"]))


#===============================================================================
#  file: doc/examples/core.janet
#===============================================================================

# Constructing graphs


(import graphs :prefix "")



# Every graph type has a constructor which takes zero or more parameters. Nodes (vertices) and edges can be added individually with =add-node= and/or =add-edge=. To add multiple nodes/edges to a graph, it can be more convenient to use =add-nodes= and/or =add-edges=.


# create a new empty graph
(def g (make-graph))

# add some nodes and edges
(add-nodes g "a" "b" "c" "d")
(add-edges g ["a" "b"] ["a" "c"] ["b" "c"] ["c" "d"])

(printf "nodes: %p\nedges: %p" (node-count g) (edge-count g))
(printf "degree(a) = %p" (degree g "a"))
(printf "neighbours(a) = %s" (string/join (map string (neighbours g "a")) ", "))



# #+RESULTS:
# : nodes: 4
# : edges: 4
# : degree(a) = 2
# : neighbours(a) = b, c

# The constructors can accept initialisation data, which is often the tersest option for small graphs.


(def d (make-digraph @["s" "t"] @{"s" @{"t" 3}}))

(printf "digraph with %p nodes, edge s->t weight is %p"
        (node-count d) (edge-weight d "s" "t"))


#===============================================================================
#  file: doc/examples/cores.janet
#===============================================================================

# Example


(import graphs :prefix "")



# Two triangles sharing the node =c=, plus the triangle =c d e= where every node has core number 2


(def g (make-graph))
(add-nodes g "a" "b" "c" "d" "e")
(add-edges g ["a" "b"] ["a" "c"] ["b" "c"] ["c" "d"] ["d" "e"] ["c" "e"])

(def cn (core-number g))
(printf "core numbers: %s"
        (string/join (map |(string (get cn $0)) (sort (nodes g) >)) " "))



# #+RESULTS:
# : core numbers: 2 2 2 2 2

# Because every node has core number 2, the shell, the peeled prefix (=k-crust=) and the k-core all coincide, while the corona picks out the four nodes with at most two neighbours inside the core.


(def k2 (k-core g 2))

(printf "2-core: %d of %d nodes" (node-count k2) (node-count g))
(printf "2-shell: %d" (node-count (k-shell g 2)))
(printf "2-crust: %d" (node-count (k-crust g 2)))
(printf "2-corona: %s" (string/join (sort (map string (nodes (k-corona g 2)))) " "))


#===============================================================================
#  file: doc/examples/cycles.janet
#===============================================================================

# Examples


(import graphs :prefix "")



# A triangle with a pendant edge. one cycle, thus one-element basis.


(def g (make-graph))
(add-nodes g "a" "b" "c" "d")
(add-edges g ["a" "b"] ["b" "c"] ["c" "a"] ["c" "d"])

(printf "has cycle? %p" (has-cycle? g))
(printf "cycle basis size: %p" (length (graph-cycle-basis g)))



# #+RESULTS:
# : has cycle? true
# : cycle basis size: 1

# An undirected quadrilateral with a chord. two fundamental cycles.


(def sq (make-graph))
(add-nodes sq "a" "b" "c" "d")
(add-edges sq ["a" "b"] ["b" "c"] ["c" "d"] ["d" "a"] ["a" "c"])

(def basis (graph-cycle-basis sq))
(printf "basis (%p cycles): %s" (length basis)
        (string/join (map |(string/join (map string $) "-") basis) "  "))


#===============================================================================
#  file: doc/examples/dag.janet
#===============================================================================

# Examples


(import graphs :prefix "")



# A diamond with a shortcut. =a → b → c = and =a → d → c=


(def g (make-digraph))
(add-nodes g ["a" "b" "c" "d"])
(add-edges g [["a" "b" 1] ["b" "c" 1] ["a" "d" 1] ["d" "c" 1]])

(printf "acyclic? %p" (is-acyclic? g))

(printf "ancestors(c) = %s"
        (string/join (sort (map string (ancestors g "c"))) ","))

(printf "longest path: %s" (string/join (dag-longest-path g) " -> "))

(printf "generations: %p" (length (topological-generations g)))



# #+RESULTS:
# : acyclic? true
# : ancestors(c) = a,b,d
# : longest path: a -> b -> c
# : generations: 3

# Transitive closure for reachability


(def h (make-digraph))
(add-nodes h ["a" "b" "c"])
(add-edges h [["a" "b" 1] ["b" "c" 1]])

(def tc (transitive-closure h))
(printf "node 'a' reaches %p nodes (incl. itself): %s"
        (length (keys (get tc "a")))
        (string/join (sort (map string (keys (get tc "a")))) ", "))



# #+RESULTS:
# : node 'a' reaches 3 nodes (incl. itself): a, b, c

# Transitive reduction removes redundant edges while preserving reachability


(def h2 (make-digraph))
(add-nodes h2 ["a" "b" "c" "d"])
(add-edges h2 [["a" "b" 1] ["b" "c" 1] ["a" "c" 1] ["c" "d" 1]])

(def tr (transitive-reduction h2))
(printf "transitive reduction: %s"
        (string/join (map (fn [e] (string (e 0) "->" (e 1))) tr) ", "))



# #+RESULTS:
# : transitive reduction: a->b, b->c, c->d

# Descendants of a node (all reachable nodes in the forward direction, excluding the source)


(def g2 (make-digraph))
(add-nodes g2 ["a" "b" "c" "d" "e"])
(add-edges g2 [["a" "b" 1] ["a" "c" 1] ["b" "d" 1] ["c" "d" 1] ["d" "e" 1]])

(printf "descendants(a) = %s"
        (string/join (sort (map string (descendants g2 "a"))) ", "))



# #+RESULTS:
# : descendants(a) = b, c, d, e

# DAG longest path length (number of edges on the critical path). Multiple longest paths may exist but only one is returned.


(def g3 (make-digraph))
(add-nodes g3 ["s" "a" "b" "c" "t"])
(add-edges g3 [["s" "a" 1] ["s" "b" 1] ["a" "c" 1] ["b" "c" 1] ["c" "t" 1]])

(printf "longest path length: %p" (dag-longest-path-length g3))
(printf "longest path: %s" (string/join (dag-longest-path g3) "->"))



# #+RESULTS:
# : longest path length: 3
# : longest path: s->b->c->t

# Antichains on a diamond DAG (maximal sets of pairwise incomparable nodes)


(def diamond (make-digraph))
(add-nodes diamond "s" "a" "b" "t")
(add-edges diamond ["s" "a" 1] ["s" "b" 1] ["a" "t" 1] ["b" "t" 1])

(def ac (antichains diamond))
(printf "antichains (%p): %s"
        (length ac)
        (string/join
         (map (fn [a] (string "{" (string/join (map string a) ",") "}")) ac) " "))



# #+RESULTS:
# : antichains (6): {} {s} {a} {b} {t} {a,b}

# Source and sink nodes in a DAG


(def g4 (make-digraph))
(add-nodes g4 "s" "a" "b" "t")
(add-edges g4 ["s" "a" 1] ["s" "b" 1] ["a" "t" 1] ["b" "t" 1])

(printf "sources: %s" (string/join (source-nodes g4) ", "))
(printf "sinks:   %s" (string/join (sink-nodes g4) ", "))



# #+RESULTS:
# : sources: s
# : sinks:   t

# Feedback arc set on a cyclic digraph (edges whose reversal breaks all cycles)


(def cyclic (make-digraph))
(add-nodes cyclic "a" "b" "c")
(add-edges cyclic ["a" "b" 1] ["b" "c" 1] ["c" "a" 1])

(def fas (feedback-arc-set cyclic))
(printf "feedback arc set: %s"
        (string/join (map (fn [e] (string (e 0) "->" (e 1))) fas) " "))



# #+RESULTS:
# : feedback arc set: c->a

# Aperiodicity test on a directed cycle (periodic) vs. a graph with a chord (aperiodic)


(def c3 (make-digraph))
(add-nodes c3 "a" "b" "c")
(add-edges c3 ["a" "b" 1] ["b" "c" 1] ["c" "a" 1])

(def c3chord (make-digraph))
(add-nodes c3chord "a" "b" "c")
(add-edges c3chord ["a" "b" 1] ["b" "c" 1] ["c" "a" 1] ["a" "c" 1])

(printf "C3 is-aperiodic? %p" (is-aperiodic? c3))
(printf "C3+chord is-aperiodic? %p" (is-aperiodic? c3chord))



# #+RESULTS:
# : C3 is-aperiodic? false
# : C3+chord is-aperiodic? true

# Colliders and v-structures in a causal DAG


(def causal (make-digraph))
(add-nodes causal "x" "y" "z" "w")
(add-edges causal ["x" "z" 1] ["y" "z" 1] ["z" "w" 1])

(printf "colliders: %s" (string/join (colliders causal) ", "))

(printf "v-structures: %s"
        (string/join (map (fn [vs] (string (vs 0) "->" (vs 1) "<-" (vs 2)))
                          (v-structures causal)) " "))


#===============================================================================
#  file: doc/examples/export.janet
#===============================================================================

# Examples


(import graphs :prefix "")

(def w (make-digraph))
(add-nodes w "a" "b")
(add-edges w ["a" "b" 1])

(print (to-dot w))



# A TikZ picture from a =:weighted-graph= which can be used for the phylogram family


(def t (make-weighted-graph))
(add-nodes t "root" "a" "b")
(add-edges t ["root" "a" 2] ["root" "b" 1])

(print (to-tikz t :layout "phylogram" :root "root" :label-edges false))


#===============================================================================
#  file: doc/examples/flow-cut.janet
#===============================================================================

# Examples


(import graphs :prefix "")



# A two-hop network, =0 → 1 → 2=, whose bottleneck is the capacity-3 arc


(def net (make-digraph))
(add-nodes net "0" "1" "2")
(add-edges net ["0" "1" 3] ["1" "2" 4])

(def r (dinic net "0" "2"))
(printf "max flow 0->2: %p" (get r :flow))

(def mc (min-cut net "0" r))
(printf "min cut capacity: %p  cut set: %p" (get mc :capacity) (get mc :cut-set))



# #+RESULTS:
# : max flow 0->2: 3
# : min cut capacity: 3  cut set: @["1" "2"]

# Cut statistics on a diamond


(def sq (make-graph))
(add-nodes sq "a" "b" "c" "d")
(add-edges sq ["a" "b"] ["a" "c"] ["b" "d"] ["c" "d"])

(printf "edge-expansion {a}: %p" (edge-expansion sq @["a"]))
(printf "cut-size {a}: %p" (cut-size sq @["a"]))
(printf "conductance {a}: %.3f" (conductance sq @["a"]))



# #+RESULTS:
# : edge-expansion {a}: 2
# : cut-size {a}: 2
# : conductance {a}: 1.000

# Dinic is faster on dense graphs but Edmonds-Karp is simpler


(def net2 (make-digraph))
(add-nodes net2 "0" "1" "2")
(add-edges net2 ["0" "1" 3] ["1" "2" 4])

(def r2 (edmonds-karp net2 "0" "2"))
(printf "edmonds-karp max flow 0->2 is %p" (get r2 :flow))
(printf "edmonds-karp min cut capacity is %p with cut set %p"
        (get (min-cut net2 "0" r2) :capacity)
        (get (min-cut net2 "0" r2) :cut-set))



# #+RESULTS:
# : edmonds-karp max flow 0->2 is 3
# : edmonds-karp min cut capacity is 3 with cut set @["1" "2"]

# Full cut statistics on a cycle C5 with node set {a, b}


(def c5 (make-graph))
(add-nodes c5 "a" "b" "c" "d" "e")
(add-edges c5 ["a" "b"] ["b" "c"] ["c" "d"] ["d" "e"] ["e" "a"])

(printf "volume {a b}: %p" (volume c5 @["a" "b"]))

(printf "cut-size {a b}: %p" (cut-size c5 @["a" "b"]))

(printf "edge-boundary {a b}: %s"
        (string/join (map (fn [e] (string (e 0) "-" (e 1)))
                          (edge-boundary c5 @["a" "b"])) " "))

(printf "node-boundary {a b}: %s"
        (string/join (node-boundary c5 @["a" "b"]) " "))

(printf "edge-expansion {a b}: %.3f" (edge-expansion c5 @["a" "b"]))

(printf "node-expansion {a b}: %.3f" (node-expansion c5 @["a" "b"]))

(printf "boundary-expansion {a b}: %.3f" (boundary-expansion c5 @["a" "b"]))

(printf "mixing-expansion {a b}: %.3f" (mixing-expansion c5 @["a" "b"]))

(printf "normalized-cut-size {a b}: %.3f" (normalized-cut-size c5 @["a" "b"]))

(printf "conductance {a b}: %.3f" (conductance c5 @["a" "b"]))


#===============================================================================
#  file: doc/examples/hamiltonian.janet
#===============================================================================

# Examples


(import graphs :prefix "")

(def g (make-graph))
(add-nodes g ["a" "b" "c" "d"])
(add-edges g [["a" "b"] ["b" "c"] ["c" "d"] ["d" "a"]])

(printf "possible cycle? %p" (hamiltonian-possible? g))
(printf "cycle: %p" (hamiltonian-cycle g))
(printf "path: %p" (hamiltonian-path g))


#===============================================================================
#  file: doc/examples/import.janet
#===============================================================================

# Examples


(import graphs :prefix "")

(def g (graph-from-adjacency-list @[@[1 2] @[0 2] @[0 1]]
                                  :labels @["a" "b" "c"]))

(printf "nodes: %p, edges: %p" (node-count g) (edge-count g))
(printf "labels: %s" (string/join (map string (nodes g)) " "))


#===============================================================================
#  file: doc/examples/layout.janet
#===============================================================================

# Examples


(import graphs :prefix "")



# A small DAG ranked via the TALA pipeline


(def dag (make-digraph))
(add-nodes dag "s" "b1" "b2" "c1" "c2" "t")
(add-edges dag [["s" "b1"] ["s" "b2"] ["b1" "c1"]
                ["b2" "c2"] ["c1" "t"] ["c2" "t"]])

(def res (layout-dag dag))
(printf "tala eligible? %p  levels: %p positioned: %p"
        (res :eligible) (res :level-count) (length (res :pos)))



# #+RESULTS:
# : tala eligible? true  levels: 4 positioned: 6


(def tb (graph-layout dag "dag" :direction "TB"))
(def lr (graph-layout dag "dag" :direction "LR"))
(printf "TB eligible? %p\nLR eligible? %p" (tb :eligible) (lr :eligible))

(def ring (make-digraph))
(add-nodes ring "n1" "n2" "n3" "n4")
(add-edges ring [["n1" "n2"] ["n2" "n3"] ["n3" "n4"] ["n4" "n1"]])

(printf "ring force eligible? %p" ((graph-layout ring "force") :eligible))
(printf "ring circle eligible? %p" ((graph-layout ring "circle") :eligible))
(printf "unknown layout => %p" (try (graph-layout ring "wonky") ([e] e)))



# #+RESULTS:
# : TB eligible? true
# : LR eligible? true
# : ring force eligible? true
# : ring circle eligible? true
# : unknown layout => "graph-layout: unknown layout 'wonky'"

# The phylogram family draws weighted trees as rectilinear dendrograms


(def t (make-weighted-graph))
(add-nodes t "root" "a" "b" "c")
(add-edges t ["root" "a" 2] ["root" "b" 1] ["b" "c" 3])

(each name ["phylogram" "phylogenetic-tree" "unrooted-phylogram"]
  (def r (graph-layout t name :root "root"))
  (printf "%s => eligible? %p, positioned: %p" name (r :eligible) (length (r :pos))))


#===============================================================================
#  file: doc/examples/matching.janet
#===============================================================================

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


#===============================================================================
#  file: doc/examples/metrics.janet
#===============================================================================

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


#===============================================================================
#  file: doc/examples/other.janet
#===============================================================================

# Example


(import graphs :prefix "")

(def h (make-binary-min-heap))
(heap-push h 3) (heap-push h 1) (heap-push h 2)

(printf "min-heap pops: %s"
        (string/join (map string [(heap-pop h) (heap-pop h) (heap-pop h)]) " "))


#===============================================================================
#  file: doc/examples/petri-net.janet
#===============================================================================

# Example


(import graphs/petri-net :prefix "")
(import graphs/export/petri-net :prefix "ex/")



# A net can be constructed using =make-petri-net=, which provides a range of options, or using =petri-net-from-arcs= which is simpler in most cases.


(def p0 (petri-net-from-arcs
         @["p1" "p2"]             # places
         @["t1" "t2"]             # transitions
         @[["p1" "t1" 1]          # arcs [source target weight]
           ["t1" "p2" 1]
           ["p2" "t2" 1]
           ["t2" "p1" 1]]))

(printf "A Petri-net with %p places and %p transitions."
        (petri-net-place-count p0) (petri-net-transition-count p0))



# #+RESULTS:
# : A Petri-net with 2 places and 2 transitions.

# A producer net. =t1= consumes a token from =p1= and produces two into =p2=


(def pn (petri-net-from-arcs
          @["p1" "p2"] @["t1"]
          @[["p1" "t1"] ["t1" "p2" 2]]
          @{"p1" 1}))

(printf "enabled: %p" (petri-net-enabled-transitions pn))

(petri-net-fire pn "t1")

(printf "marking after firing: %p" (petri-net-marking pn))

(printf "bounded? %p\nsafe? %p" (petri-net-bounded? pn) (petri-net-safe? pn))



# #+RESULTS:
# : enabled: @["t1"]
# : marking after firing: @{"p1" 0 "p2" 2}
# : bounded? true
# : safe? false

# Firing preserves the S-invariant =2·p1 + p2=


(def pn2 (petri-net-from-arcs
           @["p1" "p2"] @["t1"]
           @[["p1" "t1"] ["t1" "p2" 2]]))

(def inv (petri-net-invariant-space pn2))

(printf "S-invariant basis: %p" (get inv :places))
(printf "consistent with {p2 2}? %p" (petri-net-consistent? pn2 @{"p2" 2}))



# #+RESULTS:
# : S-invariant basis: @[@[2 1]]
# : consistent with {p2 2}? false

# Reachability. A token shuttles =x → y → x= around a cycle.


(def cy (petri-net-from-arcs
          @["x" "y"] @["s" "f"]
          @[["x" "s"] ["s" "y"] ["y" "f"] ["f" "x"]]
          @{"x" 1}))

(printf "firing sequence to {y 1}: %p" (petri-net-firing-sequence cy @{"y" 1}))

(def marking->str (fn [m] (string/join (map (fn [kv] (string (kv 0) "=" (kv 1)))
                                             (pairs m)) "|")))
(printf "reachable markings: %s"
        (string/join (map marking->str (petri-net-reachable-markings cy)) " "))

(printf "live? %p\ndeadlock? %p" (petri-net-live? cy) (petri-net-deadlock? cy))



# #+RESULTS:
# : firing sequence to {y 1}: @["s"]
# : reachable markings: x=1|y=0 x=0|y=1
# : live? true
# : deadlock? false

# Unbounded nets have infinite entries in the coverability tree


(def unb (petri-net-from-arcs
           @["p"] @["t"]
           @[["p" "t"] ["t" "p" 2]]
           @{"p" 1}))

(printf "bounded? %p" (petri-net-bounded? unb))

(def ct (petri-net-coverability-tree unb))

(printf "coverability tree contains omega? %p"
        (some (fn [n] (some (fn [c] (= c math/inf)) (values (get n :marking))))
              (get ct :nodes)))



# #+RESULTS:
# : bounded? false
# : coverability tree contains omega? true

# Reproducible simulation using a =:seed=


(def sim (petri-net-from-arcs
           @["a" "b"] @["x"]
           @[["a" "x"] ["x" "b"] ["b" "x"]]
           @{"a" 1}))

(petri-net-simulate sim :steps 5 :seed 7)
(printf "tokens conserved: %p" (petri-net-token-count sim))



# #+RESULTS:
# : tokens conserved: 1

# Render with Graphviz


(def pn3 (petri-net-from-arcs
           @["p1" "p2"] @["t1"]
           @[["p1" "t1"] ["t1" "p2" 2]]
           @{"p1" 1}))

(print (ex/petri-net-to-dot pn3))


#===============================================================================
#  file: doc/examples/planarity.janet
#===============================================================================

# Example


(import graphs :prefix "")



# =K₄= is the largest complete planar graph, =K₅= is the smallest complete graph that is not.


(defn k-n [n]
  (def g (make-graph))
  (add-nodes g (seq [i :range [0 n]] i))
  (add-edges g (seq [i :range [0 n] j :range [0 n] :when (< i j)] @[i j]))
  g)

(printf "is K4 planar? %p" (is-planar? (k-n 4)))
(printf "is K5 planar? %p" (is-planar? (k-n 5)))

(def faces (planar-faces (k-n 4)))
(printf "K4 embedding: %p faces, boundary lengths %s"
        (length faces)
        (string/join (map |(string (length ($ :vs))) faces) " "))


#===============================================================================
#  file: doc/examples/random-graphs.janet
#===============================================================================

# Example


(import graphs :prefix "")

(def g (random-graph 8 0.5 42))
(printf "G(8, 1/2) edges: %p (density %.2f)" (edge-count g) (density g))

(def gm (random-graph-edges 8 10 7))
(printf "G(8, 10 edges) actual: %p" (edge-count gm))

(def ba (barabasi-albert 20 3 2 7))
(printf "BA(20) max degree: %p"
        (reduce (fn [mx u] (max mx (degree ba u))) 0 (nodes ba)))


#===============================================================================
#  file: doc/examples/sequence.janet
#===============================================================================

# Example


(import graphs :prefix "")



# These predicates take a bare degree sequence, no graph is constructed.


(printf "[3 3 3 3 3 3] graphical? %p" (is-graphical? @[3 3 3 3 3 3]))

(printf "[3 3 2 1] graphical? %p" (is-graphical? @[3 3 2 1]))

(printf "[1 1 1 1] (C4) graphical? %p [multi] %p"
        (is-graphical? @[1 1 1 1]) (is-multigraphical? @[3 3 2 2 2]))


#===============================================================================
#  file: doc/examples/shortest-paths.janet
#===============================================================================

# Example


(import graphs :prefix "")



# Dijkstra choses a path with lower total weight over a path with fewer edges.
# e.g. =A → B → C= path weight is 3, while the direct =A → C= path weight is 5.


(def wg (make-weighted-graph))
(add-nodes wg ["A" "B" "C"])
(add-edges wg [["A" "B" 1] ["B" "C" 2] ["A" "C" 5]])

(def sp (weighted-graph-shortest-path wg "A" "C"))

(printf "shortest path from A to C: %s (distance %p)"
        (string/join (get sp 1) "->") (get sp 0))

(printf "distance table from A: %s"
        (string/join
         (map |(string/format "%.0f" $0)
              (values (weighted-graph-all-shortest-paths wg "A"))) " "))



# #+RESULTS:
# : shortest path from A to C: A->B->C (distance 3)
# : distance table from A: 1 0 3


# MST and distance-matrix


(def mg (make-weighted-graph))
(add-nodes mg ["A" "B" "C" "D"])
(add-edges mg [["A" "B" 1] ["B" "C" 2] ["C" "D" 3]
               ["A" "C" 6] ["B" "D" 5]])

(def mst (weighted-graph-mst mg))
(printf "MST edge count (n-1): %p" (weighted-graph-edge-count mst))

(def dm (make-distance-matrix @["a" "b" "c"] @[[0 4 8] [4 0 2] [8 2 0]]))
(printf "nearest pair distance: %p" (get (distance-matrix-nearest-pair dm) 2))



# #+RESULTS:
# : MST edge count (n-1): 3
# : nearest pair distance: 2

# All-pairs shortest paths via Floyd–Warshall on a weighted graph


(def wg2 (make-weighted-graph))
(add-nodes wg2 ["A" "B" "C" "D"])
(add-edges wg2 [["A" "B" 1] ["B" "C" 2] ["C" "D" 3]
                ["A" "C" 6] ["B" "D" 5]])

(def dm2 (weighted-graph->distance-matrix wg2))
(printf "distance matrix labels: %s" (string/join (get dm2 :names) ", "))
(printf "A->D distance: %.0f" (get-in dm2 [:matrix 0 3]))
(printf "B->C distance: %.0f" (get-in dm2 [:matrix 1 2]))


#===============================================================================
#  file: doc/examples/similarity.janet
#===============================================================================

# Example


(import graphs :prefix "")



# A triangle, where every pair of nodes has Jaccard similarity 1/3


(def g (make-graph))
(add-nodes g ["a" "b" "c" "d"])
(add-edges g [["a" "b"] ["a" "c"] ["b" "c"]])

(printf "J(a,b) = %.3f" (jaccard-similarity g "a" "b"))

(printf "knn(a) = %p" (knn g "a" 1))

(printf "knn(a) cosine = %p" (knn g "a" 1 :cosine))


#===============================================================================
#  file: doc/examples/spanning-tree.janet
#===============================================================================

# Examples


(import graphs :prefix "")



# check the output of =count-spanning-trees= against Cayley's formula $|V|^(|V|-2) = 4² = 16$ for =K₄=


(def k4 (make-graph))
(add-nodes k4 ["a" "b" "c" "d"])
(add-edges k4 [["a" "b"] ["a" "c"] ["a" "d"] ["b" "c"] ["b" "d"] ["c" "d"]])

(printf "spanning trees of K4 (Cayley: 4^2): %p" (count-spanning-trees k4))



# #+RESULTS:
# : spanning trees of K4 (Cayley: 4^2): 16

# Eulerian circuits of the bidirected triangle (BEST)


(def k3 (make-digraph))
(add-nodes k3 ["0" "1" "2"])
(add-edges k3 [["0" "1" 1] ["0" "2" 1]
               ["1" "0" 1] ["1" "2" 1]
               ["2" "0" 1] ["2" "1" 1]])

(printf "Eulerian circuits of bidirected K3: %p" (count-eulerian-circuits k3))


#===============================================================================
#  file: doc/examples/strings.janet
#===============================================================================

# Example


(import graphs/suffix-tree :as st)
(import graphs/cst :as cst)

(def text "banana$")

(printf "suffix array: %s"
        (string/join (map string (st/suffix-array text)) " "))

(printf "occurrences of 'ana': %s"
        (string/join (map string (st/suffix-search (st/suffix-tree text) "ana")) " "))

(printf "CST occurrences of 'ana': %s"
        (string/join (map string (cst/suffix-search (cst/cst text) "ana")) " "))

(printf "root children (CST): %p" (length (cst/cst-children (cst/cst text) 0 6)))


#===============================================================================
#  file: doc/examples/tournaments.janet
#===============================================================================

# Example


(import graphs :prefix "")

(def t (make-digraph))
(add-nodes t ["a" "b" "c"])
(add-edges t [["a" "b" 1] ["a" "c" 1] ["b" "c" 1]])

(printf "tournament? %p" (is-tournament? t))

(printf "score sequence: %s" (string/join (map string (tournament-score-sequence t)) " "))

(printf "valid? %p" (valid-tournament-score-sequence? (tournament-score-sequence t)))

(printf "kings: %s" (string/join (tournament-kings t) " "))

(printf "transitive? %p  regular? %p" (transitive-tournament? t) (regular-tournament? t))



# #+RESULTS:
# : tournament? true
# : score sequence: 0 1 2
# : valid? true
# : kings: a
# : transitive? true  regular? false

# A 3-cycle is regular and strongly connected yet still has a Hamiltonian path


(def c3 (make-digraph))
(add-nodes c3 ["a" "b" "c"])
(add-edges c3 [["a" "b" 1] ["b" "c" 1] ["c" "a" 1]])

(printf "scores: %p" (tournament-score-sequence c3))

(printf "regular? %p\nstrong? %p"
        (regular-tournament? c3) (strong-tournament? c3))

(printf "Hamiltonian path length: %p"
        (length (tournament-hamiltonian-path c3)))


#===============================================================================
#  file: doc/examples/traversal.janet
#===============================================================================

# Example


(import graphs :prefix "")

(def g (make-graph))
(add-nodes g ["a" "b" "c" "d" "e"])
(add-edges g [["a" "b"] ["b" "c"] ["a" "d"] ["b" "e"]])

(printf "bfs from a: %p" (string/join (map string (bfs g "a")) "-"))
(printf "connected? %p" (connected? g))



# #+RESULTS:
# : bfs from a: "a-b-d-c-e"
# : connected? true

# Strong connectivity and the semi-connectivity order


(def d (make-digraph))
(add-nodes d ["a" "b" "c" "d" "e"])
(add-edges d [["a" "b" 1] ["b" "c" 1]
              ["c" "a" 1] ["c" "d" 1] ["d" "e" 1]])

(printf "SCCs: %p components" (length (kosaraju-scc d)))
(printf "semi-connected? %p" (semi-connected? d))
(printf "topological order (d): %p" (topological-sort d))

(def d2 (make-digraph))
(add-nodes d2 ["a" "b" "c" "d" "e"])
(add-edges d2 [["a" "b" 1] ["a" "c" 1]
               ["b" "c" 1] ["b" "d" 1]
               ["c" "d" 1] ["c" "e" 1]])

(printf "topological order (d2): %p" (topological-sort d2))
