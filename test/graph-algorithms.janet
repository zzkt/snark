# -*- mode: janet; -*-
# graphs/test/graph-algorithms: traversal, connectivity, cycles, clustering, centrality, etc

(import ./helper :prefix "")
(import ../graphs :prefix "")

# traversal order on a branching graph

(print "\n* traversal order (branching)")

#   a - b - c
#   |   |
#   d - e
(def gbranch (make-graph @{}))
(add-edges gbranch @[["a" "b"] ["b" "c"] ["a" "d"] ["b" "e"] ["d" "e"]])

(cassert-eq "branching bfs visits all" (length (bfs gbranch "a")) 5)
(cassert-eq "branching dfs visits all" (length (dfs gbranch "a")) 5)
(cassert-eq "branching bfs root" (first (bfs gbranch "a")) "a")
(cassert-eq "branching connected" (connected? gbranch) true)

(def bfs-branch (sort (bfs gbranch "a")))
(cassert-eq "branching bfs node set"
            (deep= bfs-branch @["a" "b" "c" "d" "e"]) true)


# connected components (undirected and weakly-directed)

(print "\n* connected components")

(def gcomp (make-graph))
(add-edges gcomp @[["a" "b"] ["c" "d"] ["x" "x"] ["y" "z"]])

(def comps (connected-components gcomp))
(cassert-eq "components count" (length comps) 4)
(cassert-eq "component size 2" (deep= (sort (map length comps)) @[1 2 2 2]) true)

(def mdgwc (make-multidigraph))
(add-edges mdgwc @[["a" "b"] ["b" "a"] ["c" "d"]])

(def wcomp (multidigraph-connected-components mdgwc))
(cassert-eq "weak comps count" (length wcomp) 2)
(cassert-eq "weak comp sizes" (deep= (sort (map length wcomp)) @[2 2]) true)


# directed traversal

(print "\n* directed traversal")

(def dpath (make-multidigraph))
(add-edges dpath @[["a" "b"] ["b" "c"] ["a" "d"] ["d" "c"]])

(cassert-eq "directed bfs visits all reachable" (length (bfs dpath "a")) 4)
(cassert-eq "directed bfs order root" (first (bfs dpath "a")) "a")
(cassert-eq "directed dfs visits all reachable" (length (dfs dpath "a")) 4)
(cassert-eq "directed bfs respects direction" (length (bfs dpath "b")) 2)


# directed shortest path

(print "\n* directed shortest path")

(cassert-eq "directed shortest a-c"
            (deep= (multidigraph-shortest-path dpath "a" "c") @["a" "b" "c"]) true)
(cassert-eq "directed shortest unreachable"
            (multidigraph-shortest-path dpath "c" "a") nil)
(cassert-eq "directed shortest self"
            (deep= (multidigraph-shortest-path dpath "a" "a") @["a"]) true)

(def dloop (make-multidigraph))
(add-edges dloop @[["a" "b"] ["a" "c"] ["c" "b"] ["b" "d"]])
(cassert-eq "directed shortest direct edge"
            (deep= (multidigraph-shortest-path dloop "a" "b") @["a" "b"]) true)
(cassert-eq "directed shortest length3"
            (length (multidigraph-shortest-path dloop "a" "d")) 3)


# cycle basis (undirected)

(print "\n* cycle basis")

(def gchord (make-graph))
(add-edges gchord @[["a" "b"] ["b" "c"] ["c" "a"] ["a" "d"] ["d" "b"]])

(def basis (graph-cycle-basis gchord))
(cassert-eq "cycle basis non-empty"
            (> (length basis) 0) true)
(cassert-eq "cycle basis each has cycle"
            (all (fn [cyc] (>= (length cyc) 3)) basis) true)


# k-cycles (undirected, counted by path search)

(print "\n* k-cycles (undirected)")

(cassert-eq "k-cycle count: triangles"
            (graph-k-cycle-count gchord 3) 2)
(cassert-eq "k-cycle count: chorded quadrilateral"
            (graph-k-cycle-count gchord 4) 1)

# K4 has four triangles and three 4-cycles (despite the chords)
(def gk4 (make-graph))
(add-edges gk4 @[["a" "b"] ["a" "c"] ["a" "d"]
                 ["b" "c"] ["b" "d"] ["c" "d"]])
(cassert-eq "k-cycle count: K4 triangles"
            (graph-k-cycle-count gk4 3) 4)
(cassert-eq "k-cycle count: K4 quadrilaterals"
            (graph-k-cycle-count gk4 4) 3)

# Chordless cycles of length 4 and 5 are counted once each.
(def gc4 (make-graph))
(add-edges gc4 @[["a" "b"] ["b" "c"] ["c" "d"] ["d" "a"]])

(def gc5 (make-graph))
(add-edges gc5 @[["a" "b"] ["b" "c"] ["c" "d"] ["d" "e"] ["e" "a"]])

(cassert-eq "k-cycle count: C4" (graph-k-cycle-count gc4 4) 1)
(cassert-eq "k-cycle count: C5" (graph-k-cycle-count gc5 5) 1)

# Degenerate lengths are zero rather than a spurious cycle
(cassert-eq "k-cycle count: k < 3 is zero" (graph-k-cycle-count gk4 2) 0)
(cassert-eq "k-cycle count: k > n is zero" (graph-k-cycle-count gk4 5) 0)


(print "\n* simple cycles (directed)")

(def mdgc (make-multidigraph))
(add-edges mdgc @[["a" "b"] ["b" "c"] ["c" "a"]])

(def simple (multidigraph-simple-cycles mdgc))
(cassert-eq "directed simple cycle found" (> (length simple) 0) true)

(def mdgdl (make-multidigraph))
(add-edges mdgdl @[["a" "b"] ["b" "c"]])
(cassert-eq "dag has no simple cycle"
            (empty? (multidigraph-simple-cycles mdgdl)) true)

(def mdcyc (make-multidigraph))
(each p @[["a\x1fb" "x"] ["a" "b\x1fx"]]
  (add-edge mdcyc (p 0) (p 1))
  (add-edge mdcyc (p 1) (p 0)))

(def cyc3 (multidigraph-simple-cycles mdcyc))
(cassert-eq "separator-hostile labels keep both simple cycles"
            (length cyc3) 2)

(each c cyc3 (cassert-eq "hostile cycle is a 2-cycle" (length c) 2))


# transitivity

(print "\n* transitivity")

(def gtri2 (make-graph))
(add-edges gtri2 @[["a" "b"] ["b" "c"] ["c" "a"]])
(cassert-eq "transitivity triangle" (graph-transitivity gtri2) 1.0)

# Path a-b-c: one triad (middle b), zero triangles -> 0
(def gpt2 (make-graph))
(add-edges gpt2 @[["a" "b"] ["b" "c"]])
(cassert-eq "transitivity path" (graph-transitivity gpt2) 0.0)

# Empty graph
(cassert-eq "transitivity empty" (graph-transitivity (make-graph)) 0)


# average clustering

(print "\n* average clustering")

(def gstar2 (make-graph))
(add-edges gstar2 @[["center" "a"] ["center" "b"] ["center" "c"]])
(cassert-eq "average clustering star"
            (graph-average-clustering gstar2) 0.0)

(def mdgavg (make-multidigraph))
(add-edges mdgavg @[["a" "b"] ["b" "c"] ["c" "a"]])
(cassert-eq "directed avg clustering triangle"
            (multidigraph-average-clustering mdgavg) 1.0)

# degree centrality (directed)

(print "\n* directed degree centrality")

(def mdgcent2 (make-multidigraph))
(add-edges mdgcent2 @[["a" "b"] ["a" "c"] ["b" "a"]])

(let [dc (multidigraph-degree-centrality mdgcent2)
      a (in dc "a")]
  (cassert-eq "directed a out" (in a :out) 1.0)
  (cassert-eq "directed a in" (in a :in) (/ 1.0 2))
  (cassert-eq "directed a total" (in a :total) 1.5))

(cassert-eq "directed in-cent a"
            (in (multidigraph-in-degree-centrality mdgcent2) "a") (/ 1.0 2))


(print "\n* some edge cases")

# Semi-connectivity (directed)
(def sc-cyc (make-digraph))
(add-edge sc-cyc "a" "b" 1)
(add-edge sc-cyc "b" "a" 1)
(cassert-eq "semi-connected 2-cycle"
            (semi-connected? sc-cyc) true)

# empty digraph
(cassert-eq "semi-connected empty"
            (semi-connected? (make-digraph)) true)

# single edge
(def sc-edge (make-digraph))
(add-nodes sc-edge "a" "b")
(add-edge sc-edge "a" "b" 1)
(cassert-eq "semi-connected single edge"
            (semi-connected? sc-edge) true)

# spanning chain 1->2->3
(def sc-line (make-digraph))
(add-edges sc-line ["1" "2" 1] ["2" "3" 1])
(cassert-eq "semi-connected line"
            (semi-connected? sc-line) true)

# fork s->1, s->2 (no path 1->2 or 2->1)
(def sc-fork (make-digraph))
(add-edge sc-fork "s" "1" 1)
(add-edge sc-fork "s" "2" 1)
(cassert-eq "semi-connected fork"
            (semi-connected? sc-fork) false)

# disconnected: a->b and c->d
(def sc-disc (make-digraph))
(add-edge sc-disc "a" "b" 1)
(add-edge sc-disc "c" "d" 1)
(cassert-eq "semi-connected disconnected"
            (semi-connected? sc-disc) false)

# chain plus an offshoot, nothing reachable from the chain
(def sc-t (make-digraph))
(add-edges sc-t ["s" "a" 1] ["a" "b" 1] ["s" "c" 1])
(cassert-eq "semi-connected chain+offshoot"
            (semi-connected? sc-t) false)

# Single node directed
(def mdg1 (make-multidigraph))
(add-node mdg1 "x")
(cassert-eq "directed single bfs"
            (deep= (bfs mdg1 "x") @["x"]) true)
(cassert-eq "directed single shortest self"
            (deep= (multidigraph-shortest-path mdg1 "x" "x") @["x"]) true)
(cassert-eq "single transitivity zero"
            (graph-transitivity (do (def g (make-graph))
                                    (add-node g "x") g)) 0)

# Disconnected shortest path nil for both directions
(def mdgdisc2 (make-multidigraph))
(add-edges mdgdisc2 @[["a" "b"] ["c" "d"]])
(cassert-eq "disconnected shortest nil"
            (multidigraph-shortest-path mdgdisc2 "a" "c") nil)


(print-results)
