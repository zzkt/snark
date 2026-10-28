# -*- mode: janet; -*-
# graphstest/unified-api: unified dispatcher

(import ./helper :prefix "")
(import ../graphs :prefix "")
(import ../graphs/multigraph :prefix "mg-")
(import ../graphs/pagerank :prefix "pg-")
(import ../graphs/layout :prefix "ly-")

(cassert-eq "eulerian-cycle is public"
            (function? eulerian-cycle) true)

(def tri (make-graph @{}))
(add-edges tri [["a" "b"] ["b" "c"] ["c" "a"]])
(cassert-arr-eq "eulerian-tour simple cycle"
                (eulerian-tour tri) @["a" "b" "c"])

(defn triangle [t]
  (case t
    :digraph (do (def g (make-digraph))
                 (add-edges g ["a" "b"] ["b" "c"] ["c" "a"]) g)
    :weighted-graph (do (def g (make-weighted-graph @[] @{}))
                        (add-edges g ["a" "b" 1.0] ["b" "c" 2.0] ["c" "a" 3.0]) g)
    :graph (do (def g (make-graph @{}))
               (add-edges g ["a" "b"] ["b" "c"] ["c" "a"])g)
    :multigraph (do (def g (make-multigraph @{}))
                    (add-edges g ["a" "b"] ["b" "c"] ["c" "a"]) g)
    :multidigraph (do (def g (make-multidigraph @{} @{}))
                      (add-edges g ["a" "b"] ["b" "c"] ["c" "a"]) g)))

(defn path [t]
  (case t
    :digraph (do (def g (make-digraph))
                 (add-edges g ["a" "b"]  ["b" "c"]) g)
    :weighted-graph (do (def g (make-weighted-graph))
                        (add-edges g ["a" "b" 1.0] ["b" "c" 2.0]) g)
    :graph (do (def g (make-graph @{}))
               (add-edges g ["a" "b"]  ["b" "c"]) g)
    :multigraph (do (def g (make-multigraph))
                    (add-edges g ["a" "b"]  ["b" "c"]) g)
    :multidigraph (do (def g (make-multidigraph))
                      (add-edges g ["a" "b"]  ["b" "c"]) g)))

(each t [:graph :multigraph :multidigraph :digraph :weighted-graph]
  (cassert-eq (string "has-cycle? true on " t " triangle")
              (has-cycle? (triangle t)) true)
  (cassert-eq (string "has-cycle? false on " t " path")
              (has-cycle? (path t)) false))

# digraph remove edge/node through the unified dispatcher
(def dg (triangle :digraph))
(cassert-eq "dg remove-edge edge-count 3→2"
            (edge-count (remove-edge dg "a" "b")) 2)
(cassert-eq "dg remove-edge breaks edge"
            (has-edge? dg "a" "b") false)
(cassert-eq "dg remove-node node-count 3→2"
            (node-count (remove-node dg "a")) 2)
(cassert-eq "dg remove-node cleanup edge-count 1"
            (edge-count dg) 1)
(cassert-eq "dg remove-node neighbours b cleaned"
            (length (neighbours dg "b")) 1)

# weighted-graph remove-node through the unified dispatcher
(def wg (triangle :weighted-graph))
(cassert-eq "wg remove-node node-count 3→2"
            (node-count (remove-node wg "a")) 2)
(cassert-eq "wg remove-node breaks edge"
            (has-edge? wg "a" "b") false)
(cassert-eq "wg remove-node cleanup edge-count 1"
            (edge-count wg) 1)
(cassert-eq "wg remove-node degree b 1"
            (degree wg "b") 1)

# simple-graph remove edge/node through the unified dispatcher
(def sgg (triangle :graph))
(cassert-eq "graph remove-edge edge-count 3→2"
            (edge-count (remove-edge sgg "a" "b")) 2)
(cassert-eq "graph remove-node count 3→2"
            (node-count (remove-node sgg "a")) 2)
(cassert-eq "graph remove-node breaks edge"
            (has-edge? sgg "a" "b") false)

# multigraph/multidigraph remove edge/node
(def mg (triangle :multigraph))
(cassert-eq "mg remove-edge edge-count 3→2"
            (edge-count (remove-edge mg "a" "b")) 2)

(def mgr (make-multigraph))
(add-edges mgr ["a" "b" ] ["b" "c"])

(cassert-eq "mg remove-node count 3→2"
            (node-count (remove-node mgr "a")) 2)
(cassert-eq "mg remove-node breaks edge"
            (has-edge? mgr "a" "b") false)
(cassert-eq "mg remove-node edge-count 2→1"
            (edge-count mgr) 1)

(def mdg (triangle :multidigraph))
(cassert-eq "mdg remove-edge edge-count 3→2"
            (edge-count (remove-edge mdg "a" "b")) 2)

(def mdgr (make-multidigraph))
(add-edges mdgr ["a" "b" ] ["b" "c"])

(cassert-eq "mdg remove-node count 3→2"
            (node-count (remove-node mdgr "a")) 2)
(cassert-eq "mdg remove-node breaks edge"
            (has-edge? mdgr "a" "b") false)
(cassert-eq "mdg remove-node edge-count 2→1"
            (edge-count mdgr) 1)

# node-data / nodes-table / set-node-data via unified dispatcher
(each t @[:digraph :weighted-graph :graph :multigraph :multidigraph]
      (def g (triangle t))
      (set-node-data g "a" {:kind t})
      (cassert-eq (string t " unified set-node-data")
                  (node-data g "a") {:kind t})
      (cassert-eq (string t " unified node-data default")
                  (node-data g "b") "b")
      (cassert-eq (string t " unified nodes-table entry")
                  (in (nodes-table g) "a") {:kind t})
      (cassert-eq (string t " unified node-data absent node")
                  (node-data g "zz") nil))

# digraph/weighted-graph
(def dgnd (make-digraph))
(digraph-add-edge dgnd "x" "y" 1)

(cassert-eq "digraph add-edge seeds node-data"
            (digraph-node-data dgnd "x") "x")
(digraph-set-node-data dgnd "x" {:w 9})
(cassert-eq "digraph wrapper set-node-data"
            (digraph-node-data dgnd "x") {:w 9})

# degree consistency
(def mdg3 (make-multidigraph))
(add-edges mdg3 ["a" "b"] ["a" "b"] ["a" "b"]) # parallel edges

(cassert-eq "mdg degree counts multiplicity (b=3)"
            (degree mdg3 "b") 3)
(cassert-eq "mdg degree = in+out (a=3)"
            (degree mdg3 "a") 3)
(cassert-eq "mdg degree == md-deg (locks delegation)"
            (degree mdg3 "b") (mg-multidigraph-degree mdg3 "b"))
(cassert-arr-eq "mdg degree-sequence uses degree"
                (degree-sequence mdg3) @[3 3])

# weighted-graph self-loop
(def wgsl (make-weighted-graph))
(add-edge wgsl "u" "u" 5)

(cassert-eq "wg degree self-loop consistent"
            (degree wgsl "u") 2)
(cassert-eq "wg degree == wg-degree (locks delegation)"
            (degree wgsl "u") (weighted-graph-degree wgsl "u"))

# graph self-loop keeps +2 convention
(def gsl (make-graph))
(add-edge gsl "u" "u")
(cassert-eq "graph degree self-loop +2 unified"
            (degree gsl "u") 2)

# clustering-coefficient
(def ccg (make-digraph))
(add-nodes ccg @["a" "b" "c"])
(add-edges ccg ["a" "b"] ["a" "c"] ["b" "c"])

(cassert-eq "cc wedge a (2 out-neighbours) = 0.5"
            (clustering-coefficient ccg "a") 0.5)
(cassert-eq "cc wedge b (1 out-neighbour) = 0.0"
            (clustering-coefficient ccg "b") 0.0)
(cassert-eq "avg-cc wedge = 1/6"
            (< (math/abs (- (average-clustering-coefficient ccg) (/ 1 6))) 1e-12)
            true)
(cassert-eq "cc triangle graph = 1.0"
            (clustering-coefficient (triangle :graph) "a") 1.0)
(cassert-eq "avg-cc triangle graph = 1.0"
            (average-clustering-coefficient (triangle :graph)) 1.0)


# unified degree family on :multigraph with non-numeric labels must not cast to :number
(def mgdeg (make-multigraph))
(add-edges mgdeg ["x" "y"] ["x" "y"] ["x" "x"])

(cassert-eq "mg degree string label w/ parallel+self-loop"
            (degree mgdeg "x") 4)
(cassert-eq "mg in-degree string label"
            (in-degree mgdeg "x") 4)
(cassert-eq "mg out-degree string label"
            (out-degree mgdeg "x") 4)
(cassert-eq "mg degree absent string label"
            (degree mgdeg "z") 0)
(cassert-arr-eq "mg degree-sequence string labels"
                (degree-sequence mgdeg) @[4 2])
(cassert-eq "mg average-degree string labels"
            (< (math/abs (- (average-degree mgdeg) 3.0)) 1e-12) true)


# unified mutators must not silently no-op on :tree
(def b8t (make-tree :digraph (make-digraph)
                    :root "0" :parent @{}))
(add-edge b8t "0" "1")
(add-node b8t "2")

(cassert-eq "tree add-node adds to backing digraph"
            (deep= (sort (nodes b8t)) @["0" "1" "2"]) true)

(add-edge b8t "1" "2")
(cassert-eq "tree add-edge adds edge"
            (has-edge? b8t "1" "2") true)
(cassert-eq "tree add-edge records parent"
            (get (tree-parent b8t) "2") "1")

(remove-edge b8t "1" "2")
(cassert-eq "tree remove-edge removes edge"
            (has-edge? b8t "1" "2") false)
(cassert-eq "tree remove-edge clears parent"
            (get (tree-parent b8t) "2") nil)

(remove-node b8t "1")
(cassert-eq "tree remove-node removes node/edge"
            (has-edge? b8t "0" "1") false)
(cassert-eq "tree remove-node clears parent"
            (get (tree-parent b8t) "1") nil)
(cassert-eq "tree remove-node count"
            (node-count b8t) 2)


# density on 3-node path (2 edges)
# - undirected types use 2E/(n(n-1)) → gives 2/3
# - directed types use E/(n(n-1)) → 1/3
(each t @[:graph :multigraph :weighted-graph]
      (cassert-eq (string t " density 3-node path ≈ 2/3")
                  (< (math/abs (- (density (path t)) 0.66666666666667)) 1e-9)
                  true))
(each t @[:digraph :multidigraph]
      (cassert-eq (string t " density 3-node path ≈ 1/3")
                  (< (math/abs (- (density (path t)) 0.33333333333333)) 1e-9)
                  true))
(cassert-eq "density agrees with graph-density on path (weighted)"
            (< (math/abs (- (density (path :weighted-graph))
                            (graph-density (path :weighted-graph)))) 1e-9)
            true)
(cassert-eq "density agrees with graph-density on path (digraph)"
            (< (math/abs (- (density (path :digraph))
                            (graph-density (path :digraph)))) 1e-9)
            true)

# single-node & empty graphs have density 0
(cassert-eq "density empty graph = 0"
            (density (make-graph @{})) 0.0)

(def dn1g (make-graph @{}))
(add-node dn1g "a")
(cassert-eq "density single-node graph = 0"
            (density dn1g) 0.0)

(def dn1d (make-digraph))
(add-node dn1d "a")
(cassert-eq "density single-node digraph = 0"
            (density dn1d) 0.0)

# multiplicity-aware density. 2E/(n(n-1)) counts each parallel edge
(def mgden (make-multigraph))
(add-nodes mgden "a" "b")
(add-edges mgden ["a" "b"] ["a" "b"])
(cassert-eq "multigraph density counts parallel edges (2·2/2=2)"
            (density mgden) 2.0)


# degree-centrality. weighted-graph is undirected → simple degree
(def dcwg (path :weighted-graph))
(def dct (degree-centrality dcwg))
(cassert-eq "degree-centrality weighted a = 0.5" (get dct "a") 0.5)
(cassert-eq "degree-centrality weighted b = 1.0" (get dct "b") 1.0)
(cassert-eq "degree-centrality weighted c = 0.5" (get dct "c") 0.5)

# degree-centrality. :digraph still uses total (in+out) degree
(def dcdg (path :digraph))
(def dcd (degree-centrality dcdg))
(cassert-eq "degree-centrality digraph a (out1, in0 → 1/(3-1)) = 0.5"
            (get dcd "a") 0.5)
(cassert-eq "degree-centrality digraph b (out1, in1 → 2/2) = 1.0"
            (get dcd "b") 1.0)


# unified API should not trust stale :node-count fields
(def ncp (make-weighted-graph @["x" "y" "z"] @{}))
(cassert-eq "pre-seeded weighted-graph node-count"
            (node-count ncp) 3)
(cassert-eq "pre-seeded weighted-graph nodes-table keys"
            (length (keys (nodes ncp))) 3)


#  edge-data

(def ed-d (make-digraph))
(add-nodes ed-d ["a" "b" "c"])
(add-edge ed-d "a" "b" 1)
(add-edge ed-d "b" "a" 2)
(set-edge-data ed-d "a" "b" @{:label "paid" :color "#27AE60"})

(cassert-eq "edge-data: stored and read back"
            (get (edge-data ed-d "a" "b") :label) "paid")
(cassert-eq "edge-data: the reverse of a directed edge is a different edge"
            (edge-data ed-d "b" "a") nil)
(cassert-eq "edge-data: has? is true for an annotated edge"
            (edge-data-has? ed-d "a" "b") true)
(cassert-eq "edge-data: has? is false for the other direction"
            (edge-data-has? ed-d "b" "a") false)
(cassert-eq "edge-data: the key is an ordered pair"
            (get (get (edge-data-table ed-d) (tuple "a" "b")) :color) "#27AE60")
(cassert-eq "edge-data: the reverse direction is a separate key"
            (get (edge-data-table ed-d) (tuple "b" "a")) nil)

(def ed-u (make-graph))
(each n ["a" "b"] (add-node ed-u n))
(add-edge ed-u "a" "b")
(set-edge-data ed-u "a" "b" @{:color "red"})
(cassert-eq "edge-data: undirected read in either order"
            (get (edge-data ed-u "b" "a") :color) "red")
(cassert-eq "edge-data: undirected stores one canonical key"
            (length (keys (edge-data-table ed-u))) 1)
(cassert-eq "edge-data: the undirected key is min then max"
            (get (get (edge-data-table ed-u) (tuple "a" "b")) :color) "red")

(def ed-w (make-weighted-graph))
(add-nodes ed-w ["a" "b"])
(add-edge ed-w "a" "b" 2.5)
(set-edge-data ed-w "a" "b" @{:style "dashed"})
(cassert-eq "edge-data: works on a weighted graph"
            (get (edge-data ed-w "a" "b") :style) "dashed")

(def ed-m (make-multigraph))
(add-nodes ed-m ["a" "b"])
(add-edges ed-m ["a" "b"] ["a" "b"])
(set-edge-data ed-m "a" "b" @{:class "link"})
(cassert-eq "edge-data: works on a multigraph"
            (get (edge-data ed-m "a" "b") :class) "link")
(cassert-eq "edge-data: parallel edges share one key"
            (length (keys (edge-data-table ed-m))) 1)

(def ed-md (make-multidigraph))
(add-nodes ed-md ["a" "b"])
(add-edge ed-md "a" "b")
(set-edge-data ed-md "a" "b" @{:v 1})
(cassert-eq "edge-data: works on a multidigraph"
            (get (edge-data ed-md "a" "b") :v) 1)
(cassert-eq "edge-data: multidirected is order-sensitive"
            (edge-data ed-md "b" "a") nil)

# the store can hold any value
(set-edge-data ed-d "a" "b" "plain label")
(cassert-eq "edge-data: any value may be stored"
            (edge-data ed-d "a" "b") "plain label")

# annotating a missing edge does not invent one
(cassert-eq "edge-data: setting on a missing edge stores nothing"
            (do (set-edge-data ed-d "a" "zzz" @{:v 1})
                (edge-data-has? ed-d "a" "zzz")) false)
(cassert-eq "edge-data: and does not change the edge count"
            (ed-d :edge-count) 2)

# remove data
(set-edge-data ed-d "a" "b" @{:v 1})
(remove-edge-data ed-d "a" "b")
(cassert-eq "edge-data: remove clears the entry" (edge-data ed-d "a" "b") nil)

# remove-edge and remove-node must not leave the store stale
(def ed-r (make-digraph))
(each n ["a" "b" "c"] (add-node ed-r n))
(add-edge ed-r "b" "a" 1) (add-edge ed-r "a" "c" 1) (add-edge ed-r "c" "c" 1)
(set-edge-data ed-r "b" "a" @{:v 1}) (set-edge-data ed-r "a" "c" @{:v 2}) (set-edge-data ed-r "c" "c" @{:v 3})
(remove-edge ed-r "a" "c")
(cassert-eq "edge-data: remove-edge clears that edge" (edge-data ed-r "a" "c") nil)
(cassert-eq "edge-data: remove-edge leaves the others" (get (edge-data ed-r "b" "a") :v) 1)
(remove-node ed-r "a")
(cassert-eq "edge-data: remove-node clears the in-arc" (edge-data ed-r "b" "a") nil)
(cassert-eq "edge-data: remove-node leaves an unrelated self-loop"
            (get (edge-data ed-r "c" "c") :v) 3)

(def ed-ru (make-graph @{}))
(each n ["a" "b"] (add-node ed-ru n))
(add-edge ed-ru "a" "b")
(set-edge-data ed-ru "a" "b" @{:v 1})
(remove-node ed-ru "a")
(cassert-eq "edge-data: undirected remove-node clears it"
            (length (keys (edge-data-table ed-ru))) 0)

# node labels

(print "\n* unified: node-name types")

(def t-dg (make-digraph))
(add-node t-dg "a") (add-node t-dg "b") (add-edge t-dg "a" "b" 2)
(cassert-eq "name types: a digraph takes strings" (edge-weight t-dg "a" "b") 2)

(def t-di (make-digraph))
(add-node t-di 1) (add-node t-di 2) (add-edge t-di 1 2 3)

(cassert-eq "name types: a digraph takes integers" (edge-weight t-di 1 2) 3)
(cassert-eq "name types: strings and integers coexist in one graph"
            (do (def m (make-digraph))
                (add-node m "a") (add-node m 7) (add-edge m "a" 7 1)
                (m :edge-count)) 1)

(cassert-eq "name types: a graph takes integers"
            (do (def g (make-graph @{}))
                (add-node g 1) (add-node g 2) (add-edge g 1 2)
                (g :edge-count)) 1)

(cassert-eq "name types: a weighted graph takes integers"
            (do (def w (make-weighted-graph @[] @{}))
                (add-node w 1) (add-node w 2) (add-edge w 1 2 2.5)
                (edge-weight w 1 2)) 2.5)

(cassert-eq "name types: a multigraph takes integers"
            (do (def mg (make-multigraph @{}))
                (add-node mg 1) (add-node mg 2) (add-edge mg 1 2) (add-edge mg 1 2)
                (mg :edge-count)) 2)

(cassert-eq "name types: a multidigraph takes integers"
            (do (def md (make-multidigraph))
                (add-node md 1) (add-node md 2) (add-edge md 1 2)
                (md :edge-count)) 1)

# mixed labels stay comparable
(cassert-eq "name types: mixed string/integer undirected edges"
            (do (def x (make-graph @{}))
                (add-node x "a") (add-node x 2) (add-edge x "a" 2)
                (= (type (to-dot x)) :string)) true)

(cassert-eq "name types: edge-data canonicalises mixed labels"
            (do (def x (make-graph @{}))
                (add-node x "a") (add-node x 2) (add-edge x "a" 2)
                (set-edge-data x "a" 2 @{:k "v"})
                (get (edge-data x 2 "a") :k)) "v")

# exporters stringify labels
(def t-d (make-digraph))
(add-node t-d 1) (add-node t-d 2) (add-edge t-d 1 2 1)
(cassert-eq "name types: to-dot accepts integer names"
            (= (type (to-dot t-d)) :string) true)
(cassert-eq "name types: to-gexf accepts integer names"
            (= (type (to-gexf t-d)) :string) true)
(cassert-eq "name types: to-cytoscape accepts integer names"
            (= (type (to-cytoscape t-d)) :string) true)
(cassert-eq "name types: to-tikz accepts integer names"
            (= (type (to-tikz t-d)) :string) true)

# a tree root is a node name
(cassert-eq "name types: a tree takes an integer root"
            (do (def tg (make-digraph))
                (add-node tg 1) (add-node tg 2) (add-edge tg 1 2)
                (tree-root (make-tree tg 1 @{2 1}))) 1)

# anything outside the union is consistently rejected
(cassert-err "name types: a keyword name is rejected by :digraph"
             (add-node (make-digraph @[] @{}) :k))
(cassert-err "name types: a keyword name is rejected by :graph"
             (add-node (make-graph @{}) :k))
(cassert-err "name types: a keyword name is rejected by :weighted-graph"
             (add-node (make-weighted-graph @[] @{}) :k))
(cassert-err "name types: a keyword name is rejected by :multigraph"
             (add-node (make-multigraph @{}) :k))
(cassert-err "name types: a keyword name is rejected by :multidigraph"
             (add-node (make-multidigraph @{}) :k))
(cassert-err "name types: a float name is rejected"
             (add-node (make-digraph @[] @{}) 1.5))
(cassert-err "name types: a struct name is rejected"
             (add-node (make-digraph @[] @{}) @{}))


#  :multidigraph

(cassert-eq "multidigraph algorithms: pagerank runs"
            (do (def m1 (make-multidigraph))
                (each n ["a" "b" "c"] (add-node m1 n))
                (add-edge m1 "a" "b") (add-edge m1 "b" "c")
                (= (type (pg-pagerank m1)) :table)) true)

(defn- ran? [f]
  (not= nil (try (do (f) true) ([_] nil))))

(cassert-eq "multidigraph algorithms: every layout runs"
            (do (def m2 (make-multidigraph))
                (add-nodes m2 ["a" "b" "c" "d"])
                (add-edges m2 ["a" "b"] ["b" "c"] ["c" "d"] ["a" "c"])
                (and (ran? (fn [] (ly-layout-force m2)))
                     (ran? (fn [] (ly-layout-circle m2)))
                     (ran? (fn [] (ly-layout-radial m2)))
                     (ran? (fn [] (ly-layout-orthogonal m2)))
                     (ran? (fn [] (ly-layout-spectral m2)))
                     (ran? (fn [] (ly-layout-phylogram m2)))
                     (ran? (fn [] (ly-layout-dag m2))))) true)

(cassert-eq "multidigraph algorithms: to-tikz with a layout runs"
            (do (def m3 (make-multidigraph))
                (add-nodes m3 ["a" "b" "c"])
                (add-edges m3 ["a" "b"] ["b" "c"])
                (= (type (to-tikz m3 :layout "phylogram")) :string)) true)

(cassert-eq "multidigraph algorithms: the graph is not mutated by a layout"
            (do (def m4 (make-multidigraph))
                (add-nodes m4 ["a" "b" "c"])
                (add-edges m4 ["a" "b"] ["b" "c"])
                (ly-layout-force m4)
                [(m4 :node-count) (m4 :edge-count)]) [3 2])


# remove nodes/edges
(def rn (make-graph @{}))
(add-nodes rn ["a" "b" "c" "d"])
(add-nodes rn "e")
(add-edges rn [["a" "b"] ["b" "c"] ["c" "d"]])
(add-edges rn ["d" "e"])
(remove-nodes rn "b" "d")

(cassert-eq "remove-nodes drops nodes"
            (set-eq (nodes rn) ["a" "c" "e"]) true)
(cassert-eq "remove-nodes drops incident edges"
            (has-edge? rn "a" "b") false)
(cassert-eq "remove-nodes drops chain edges"
            (has-edge? rn "b" "c") false)
(cassert-eq "remove-nodes drops end edges"
            (has-edge? rn "c" "d") false)
(cassert-eq "remove-nodes drops edges to removed nodes"
            (has-edge? rn "d" "e") false)

(def re (make-digraph))
(add-nodes re ["a" "b" "c"])
(add-edges re [["a" "b"] ["b" "c"] ["c" "a"]])
(remove-edges re ["a" "b"] ["c" "a"])

(cassert-eq "remove-edges drops specified edges"
            (has-edge? re "a" "b") false)
(cassert-eq "remove-edges drops other specified edges"
            (has-edge? re "c" "a") false)
(cassert-eq "remove-edges keeps remaining edges"
            (has-edge? re "b" "c") true)
(cassert-eq "remove-edges leaves endpoints"
            (and (has-node? re "a") (has-node? re "b")) true)

(def rmw (make-weighted-graph))
(add-nodes rmw ["x" "y" "z"])
(add-edge rmw "x" "y" 2)
(add-edge rmw "y" "z" 3)
(remove-edges rmw ["x" "y"])

(cassert-eq "remove-edges works on weighted"
            (has-edge? rmw "x" "y") false)
(cassert-eq "remove-edges keeps other weighted edges"
            (has-edge? rmw "y" "z") true)

(def rmg (make-multigraph))
(add-edge rmg "a" "b")
(add-edge rmg "a" "b")
(remove-edges rmg ["a" "b"])

(cassert-eq "remove-edges on multigraph clears all parallels"
            (has-edge? rmg "a" "b") false)


(print-results)
