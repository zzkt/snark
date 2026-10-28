# -*- mode: janet; -*-
# graphs/test/clustering: Louvain/Leiden weights and community-detection

(import ./helper :prefix "")
(import ../graphs :prefix "")

(print "\n* aggregate weight bookkeeping")

(def g (make-digraph))
(add-nodes g @["a" "b" "c" "d"])

(add-edges g @[["a" "b"] ["b" "a"] ["b" "c"]
               ["c" "b"] ["c" "d"] ["d" "c"]])
(def comm @{"a" "1" "b" "1" "c" "2" "d" "2"})
(def [g2 names] (aggregate g comm))

# Each self-loop must be the internal arc weight
(cassert-eq "aggregate self-loop {1} = internal weight 2"
            (digraph-edge-weight g2 "1" "1") 2.0)

(cassert-eq "aggregate self-loop {2} = internal weight 2"
            (digraph-edge-weight g2 "2" "2") 2.0)

# inter-community arcs keep their original weights
(cassert-eq "aggregate inter arc 1->2 = 1"
            (digraph-edge-weight g2 "1" "2") 1.0)

(cassert-eq "aggregate inter arc 2->1 = 1"
            (digraph-edge-weight g2 "2" "1") 1.0)

# total weight is preserved: 2+2+1+1 = 6 = the six unit arcs
(cassert-eq "aggregate total weight invariant"
            (total-weight g2) (total-weight g))

# community id -> super-node name mapping
(cassert-eq "name-of maps 1 -> \"1\"" (get names "1") "1")
(cassert-eq "name-of maps 2 -> \"2\"" (get names "2") "2")

# inter-community rows accumulate weights from every member
(def gf (make-digraph))
(add-nodes gf @["a" "b" "c"])
(add-edges gf ["a" "c" 1] ["b" "c" 2])
(def [g2f _] (aggregate gf @{"a" "1" "b" "1" "c" "2"}))
(cassert-eq "aggregate role rows accumulate (3 = 1 + 2)"
            (digraph-edge-weight g2f "1" "2") 3.0)

# intra-community self-loops on the original graph count toward internal weight
(def gs (make-digraph))
(add-nodes gs @["a" "b" "s"])
(add-edges gs ["a" "b" 1] ["b" "a" 1] ["s" "s" 3])
(def [g2s _] (aggregate gs @{"a" "1" "b" "1" "s" "1"}))

(cassert-eq "aggregate includes original self-loops (5 = 1+1+3)"
            (digraph-edge-weight g2s "1" "1") 5.0)

(cassert-eq "aggregate single-community total preserved"
            (total-weight g2s) (total-weight gs))

(print "\n* Louvain/Leiden")

# partition covers every original node exactly once
(def part (louvain g))
(cassert-eq "Louvain maps every node"
            (length (keys part)) 4)

(cassert-eq "Louvain partition has 2 communities"
            (length (distinct (values part))) 2)

(def part2 (leiden g))
(cassert-eq "Leiden maps every node"
            (length (keys part2)) 4)

(cassert-eq "Leiden communities non-trivial"
            (< (length (distinct (values part2))) 4) true)


# single isolated node -> singleton partition
(def lone (make-digraph @["lonely"] @{}))
(cassert-eq "Louvain on an isolated node is a singleton"
            (length (keys (louvain lone))) 1)

(print-results)
