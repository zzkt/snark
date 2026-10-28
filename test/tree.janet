# -*- mode: janet; -*-
# graphs/test/tree: trees and foliage

(import ./helper :prefix "")
(import ../graphs :prefix "")
(import ./data/graphs :prefix "fx-")

(print "\n* tree construction")

# a tree rooted at "0" with children 1 and 2
(def g (make-digraph))
(add-nodes g "0" "1" "2""3")
(add-edges g ["0" "1" 1]  ["0" "2" 1]  ["1" "3" 1])
(def tr (make-tree g "0" @{"1" "0" "2" "0" "3" "1"}))

(cassert-eq "tree-root returns root"
            (tree-root tr) "0")
(cassert-eq "tree-parent returns entire parent map"
            (arr-eq (tree-parent tr) @{"1" "0" "2" "0" "3" "1"}) true)
(cassert-eq "tree->digraph returns the backing digraph"
            (digraph-node-count (tree->digraph tr)) 4)
(cassert-eq "tree->digraph has edges"
            (digraph-edge-count (tree->digraph tr)) 3)

# uniform keyword construction
(def tr2 (make-tree :digraph g
                    :root "0"
                    :parent @{"1" "0" "2" "0" "3" "1"}))

(cassert-eq "make-tree keyword root"
            (tree-root tr2) "0")
(cassert-eq "make-tree keyword digraph"
            (digraph-node-count (tree->digraph tr2)) 4)

(print "\n* weighted tree construction")

# a weighted tree rooted at "root"
(def wg (make-weighted-graph @[] @{}))
(add-edges wg @[["root" "g" 4] ["root" "r1" 1] ["r1" "r2" 1] ["r2" "b" 1]
                ["r2" "r3" 1] ["r3" "c" 2] ["r3" "d" 2] ["r3" "f" 3]
                ["r1" "a" 2] ["r1" "e" 3]])

# explicit parent map c.f. make-tree
(def wt (make-weighted-tree wg "root"
                            @{"g" "root" "r1" "root" "r2" "r1" "a" "r1" "e" "r1"
                              "b" "r2" "r3" "r2" "c" "r3" "d" "r3" "f" "r3"}))

(cassert-eq "make-weighted-tree root"
            (weighted-tree-root wt) "root")

(cassert-eq "make-weighted-tree parent map"
            (arr-eq (sorted (keys (weighted-tree-parent wt)))
                    (sorted (keys @{"g" 1 "r1" 1 "r2" 1 "a" 1 "e" 1 "b" 1 "r3" 1
                                    "c" 1 "d" 1 "f" 1}))) true)

(cassert-eq "weighted-tree->graph exposes 11 nodes"
            (weighted-tree-node-count wt) 11)

(cassert-eq "weighted-tree-edge-weight reads branch length"
            (weighted-tree-edge-weight wt "root" "g") 4)

# derived parent map from root
(def wt2 (make-weighted-tree wg "root"))
(cassert-eq "derived parent of g is root"
            (get (weighted-tree-parent wt2) "g") "root")
(cassert-eq "derived parent of b is r2"
            (get (weighted-tree-parent wt2) "b") "r2")
(cassert-eq "derived parent of f is r3"
            (get (weighted-tree-parent wt2) "f") "r3")

# keyword construction for :weighted-tree
(def wt3 (make-weighted-tree
          :graph wg
          :root "root"
          :parent @{"g" "root" "r1" "root" "r2" "r1"}))

(cassert-eq "make-weighted-tree keyword root"
            (weighted-tree-root wt3) "root")
(cassert-eq "make-weighted-tree keyword parent of g"
            (get (weighted-tree-parent wt3) "g") "root")
(cassert-eq "make-weighted-tree keyword parent of r2"
            (get (weighted-tree-parent wt3) "r2") "r1")

# keyword form
(def wt4 (make-weighted-tree :graph wg :root "root"))
(cassert-eq "keyword-derived parent of g is root"
            (get (weighted-tree-parent wt4) "g") "root")
(cassert-eq "keyword-derived parent of b is r2"
            (get (weighted-tree-parent wt4) "b") "r2")

# branch-length layout consumes the backing graph
(def pos (layout-phylogram (weighted-tree->graph wt) :root "root"))

(cassert-eq "phylogram root sits at x 0"
            (first (get (pos :pos) "root")) 0)
(cassert-eq "phylogram g sits at x 4"
            (first (get (pos :pos) "g")) 4)
(cassert-eq "phylogram c sits at cumulative depth 5"
            (first (get (pos :pos) "c")) 5)
(cassert-eq "phylogram b at depth 3"
            (first (get (pos :pos) "b")) 3)
(cassert-eq "phylogram r1 at depth 1"
            (first (get (pos :pos) "r1")) 1)
(cassert-eq "phylogram f at deepest leaf"
            (first (get (pos :pos) "f")) 6)
(cassert-eq "phylogram g is first leaf row"
            (last (get (pos :pos) "g")) 0)
(cassert-eq "phylogram r1 row is mean of [0 7/3]"
            (last (get (pos :pos) "r1")) (/ 7 3))
(cassert-eq "phylogram has 10 tree edges"
            (length (pos :edges)) 10)
(cassert-eq "phylogram is orthogonal"
            (get pos :orthogonal) true)

# rooted phylogenetic-tree
(def aligned (layout-phylogenetic-tree
              (weighted-tree->graph wt) :root "root"))

(cassert-eq "aligned g flushed to x 6"
            (first (get (aligned :pos) "g")) 6)
(cassert-eq "aligned a flushed to x 6"
            (first (get (aligned :pos) "a")) 6)
(cassert-eq "aligned b flushed to x 6"
            (first (get (aligned :pos) "b")) 6)
(cassert-eq "aligned f flushed to x 6"
            (first (get (aligned :pos) "f")) 6)
(cassert-eq "aligned internal r3 stays at x 3"
            (first (get (aligned :pos) "r3")) 3)

# unrooted phylogram
(def unrooted (layout-unrooted-phylogram
               (weighted-tree->graph wt) :root "root"))

(cassert-eq "unrooted root at hub x 0"
            (first (get (unrooted :pos) "root")) 0)
(cassert-eq "unrooted g positive side x 4"
            (first (get (unrooted :pos) "g")) 4)
(cassert-eq "unrooted r1 negative side x -1"
            (first (get (unrooted :pos) "r1")) -1)
(cassert-eq "unrooted f deepest negative x -6"
            (first (get (unrooted :pos) "f")) -6)
(cassert-eq "unrooted a negative x -3"
            (first (get (unrooted :pos) "a")) -3)
(cassert-eq "unrooted has 10 edges"
            (length (unrooted :edges)) 10)

# unrooted aligned
(def ualign (layout-unrooted-phylogenetic-tree
             (weighted-tree->graph wt) :root "root"))

(cassert-eq "ualign side-A f flush -6"
            (first (get (ualign :pos) "f")) -6)
(cassert-eq "ualign side-A a flush -6"
            (first (get (ualign :pos) "a")) -6)
(cassert-eq "ualign side-A d flush -6"
            (first (get (ualign :pos) "d")) -6)
(cassert-eq "ualign side-B g stays 4"
            (first (get (ualign :pos) "g")) 4)

# vertical
(def vert (layout-phylogram (weighted-tree->graph wt)
                            :root "root" :vertical true))

(cassert-eq "vertical root x = phylo y"
            (first (get (vert :pos) "root"))
            (last (get (pos :pos) "root")))

(cassert-eq "vertical root y = phylo x"
            (last (get (vert :pos) "root"))
            (first (get (pos :pos) "root")))

# mirror
(def mir (layout-phylogram (weighted-tree->graph wt)
                           :root "root" :mirror true))

(cassert-eq "mirror root x = 0"
            (first (get (mir :pos) "root")) 0)
(cassert-eq "mirror g x = -4"
            (first (get (mir :pos) "g")) -4)
(cassert-eq "mirror f x = -6"
            (first (get (mir :pos) "f")) -6)

# TB = vertical. BT = vertical and mirrorred
(def tb (layout-phylogram (weighted-tree->graph wt)
                          :root "root" :vertical true))

(cassert-eq "TB root y = 0"
            (last (get (tb :pos) "root")) 0)

(cassert-eq "TB leaves grow down"
            (> (last (get (tb :pos) "f")) 0) true)

(def bt (layout-phylogram (weighted-tree->graph wt) :root "root"
                          :vertical true :mirror true))

(cassert-eq "BT root y = 0"
            (last (get (bt :pos) "root")) 0)

(cassert-eq "BT leaves grow up"
            (< (last (get (bt :pos) "f")) 0) true)

# missing weights
(def sm (make-graph))
(add-edges sm ["r" "x"] ["x" "y"])

(def sm-pos (layout-phylogram sm :root "r"))
(cassert-eq "simple graph root at 0"
            (first (get (sm-pos :pos) "r")) 0)
(cassert-eq "simple graph x at 1"
            (first (get (sm-pos :pos) "x")) 1)
(cassert-eq "simple graph y at 2"
            (first (get (sm-pos :pos) "y")) 2)

# disconnected w. leftover stacked below leaves
(def disc (make-weighted-graph))
(add-nodes disc @["r" "x" "lo"])
(add-edge disc "r" "x" 1)

(def disc-pos (layout-phylogram disc :root "r"))
(cassert-eq "disconnected lo at axis x 0"
            (first (get (disc-pos :pos) "lo")) 0)
(cassert-eq "disconnected lo stacked below"
            (last (get (disc-pos :pos) "lo")) 1)

# singleton
(def sg (make-weighted-graph))
(add-node sg "z")

(def sg-pos (layout-phylogram sg))
(cassert-eq "singleton has 1 node"
            (length (sg-pos :pos)) 1)
(cassert-eq "singleton root set"
            (get sg-pos :root) "z")
(cassert-eq "singleton at origin"
            (first (get (sg-pos :pos) "z")) 0)

# empty graph
(def eg (make-weighted-graph))
(def eg-pos (layout-phylogram eg))

(cassert-eq "empty has 0 nodes"
            (length (eg-pos :pos)) 0)
(cassert-eq "empty is eligible"
            (get eg-pos :eligible) true)

# 2-node tree
(def tw (make-weighted-graph))
(add-edge tw "p" "q" 3)
(def tw-pos (layout-phylogram tw :root "p"))

(cassert-eq "2-node p at 0"
            (first (get (tw-pos :pos) "p")) 0)
(cassert-eq "2-node q at 3"
            (first (get (tw-pos :pos) "q")) 3)
(cassert-eq "2-node 1 edge"
            (length (tw-pos :edges)) 1)


# generic API can be used to construct a weighted tree
(def wtg (make-weighted-graph))
(add-nodes wtg "r" "c" "d")
(add-edges wtg ["r" "c" 4] ["c" "d" 6])
(def wt (make-weighted-tree wtg "r"))

(cassert-eq "generic nodes sees the weighted tree"
            (length (nodes wt)) 3)
(cassert-eq "generic node-count"
            (node-count wt) 3)
(cassert-eq "generic edge-count"
            (edge-count wt) 2)
(cassert-eq "generic has-node? root"
            (has-node? wt "r") true)
(cassert-eq "generic has-node? leaf"
            (has-node? wt "d") true)
(cassert-eq "generic has-node? absent"
            (has-node? wt "nope") false)
(cassert-eq "generic has-edge? forward"
            (has-edge? wt "r" "c") true)
(cassert-eq "generic has-edge? backward"
            (has-edge? wt "c" "r") true)
(cassert-eq "generic has-edge? absent"
            (has-edge? wt "r" "d") false)
(cassert-eq "generic degree of root"
            (degree wt "r") 1)
(cassert-eq "generic degree of middle"
            (degree wt "c") 2)
(cassert-eq "generic neighbours of root"
            (length (neighbours wt "r")) 1)
(cassert-eq "generic in-degree falls back to degree (as for :tree)"
            (in-degree wt "c") 2)
(cassert-eq "generic edge-weight reads the weight"
            (edge-weight wt "r" "c") 4)
(cassert-eq "generic edge-weight other way"
            (edge-weight wt "c" "r") 4)
(cassert-eq "generic nodes-table"
            (length (nodes-table wt)) 3)
(cassert-eq "generic node-data"
            (node-data wt "r") "r")

# generic traversal reaches every node
(cassert-eq "generic bfs from the root visits all nodes"
            (length (bfs wt "r")) 3)

# mutators use weighted graph and keep :parent in sync
(add-node wt "newleaf")
(cassert-eq "add-node reaches the backing graph"
            (has-node? wt "newleaf") true)
(cassert-eq "add-node bumps the count"
            (node-count wt) 4)

(add-edge wt "r" "newleaf" 2)
(cassert-eq "add-edge reaches the backing graph"
            (has-edge? wt "r" "newleaf") true)
(cassert-eq "add-edge bumps the count"
            (edge-count wt) 3)
(cassert-eq "add-edge records the weight"
            (edge-weight wt "r" "newleaf") 2)

(remove-edge wt "r" "newleaf")
(cassert-eq "remove-edge drops the edge"
            (has-edge? wt "r" "newleaf") false)
(cassert-eq "remove-edge drops the count"
            (edge-count wt) 2)

(remove-node wt "newleaf")
(cassert-eq "remove-node drops the node"
            (has-node? wt "newleaf") false)
(cassert-eq "remove-node drops the count"
            (node-count wt) 3)

(print-results)
