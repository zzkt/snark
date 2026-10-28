# -*- mode: janet; -*-
# graphs/test/tournament: recognition, Rédei Hamiltonian paths, kings (Maurer),
#                         strong/regular/transitive classes, Landau score sequences.

(import ./helper :prefix "")
(import ../graphs :prefix "")
(import ./data/graphs :prefix "fx-")

(print "\n* tournament recognition")

(def t1 (make-digraph))
(add-nodes t1 @["a" "b" "c"])
(add-edges t1 ["a" "b" 1] ["a" "c" 1] ["b" "c" 1])
(cassert-eq "transitive tournament recognised"
            (is-tournament? t1) true)

(def c3 (make-digraph))
(add-nodes c3 @["a" "b" "c"])
(add-edges c3 ["a" "b" 1] ["b" "c" 1] ["c" "a" 1])
(cassert-eq "3-cycle recognised"
            (is-tournament? c3) true)

(def noarc (make-digraph))
(add-nodes noarc "a" "b")
(cassert-eq "missing arc not a tournament"
            (is-tournament? noarc) false)

(def both (make-digraph))
(add-nodes both "a" "b")
(add-edges both ["a" "b" 1]  ["b" "a" 1])
(cassert-eq "antiparallel arcs not a tournament"
            (is-tournament? both) false)

(def one (make-digraph))
(add-nodes one "a" "b")
(add-edge one "a" "b" 1)
(cassert-eq "single arc is a tournament"
            (is-tournament? one) true)

(print "\n* score sequences (Landau)")

(cassert-arr-eq "t1 scores (0,1,2)"
                (tournament-score-sequence t1) @[0 1 2])
(cassert-arr-eq "c3 scores (1,1,1)"
                (tournament-score-sequence c3) @[1 1 1])
(cassert-eq "(0,1,2,3) is valid"
            (valid-tournament-score-sequence? @[0 1 2 3]) true)
(cassert-eq "(1,1,1,1) is invalid"
            (valid-tournament-score-sequence? @[1 1 1 1]) false)
(cassert-eq "(0,0,1,1) is invalid"
            (valid-tournament-score-sequence? @[0 0 1 1]) false)

(print "\n* adjacency matrix")

(def m1 (tournament-matrix t1))
(cassert-eq "matrix diagonal is 0" (get-in m1 [0 0]) 0)
(cassert-eq "matrix a→b entry 1" (get-in m1 [0 1]) 1)
(cassert-eq "matrix b→a entry 0" (get-in m1 [1 0]) 0)

(print "\n* Hamiltonian paths")

(cassert-eq "t1 hamp length 3" (length (tournament-hamiltonian-path t1)) 3)
(cassert-eq "c3 hamp length 3" (length (tournament-hamiltonian-path c3)) 3)

(print "\n* kings")

(cassert-arr-eq "t1 kings = the winner"
                (tournament-kings t1) @["a"])
(cassert-eq "winner is a king"
            (is-tournament-king? t1 "a") true)
(cassert-eq "loser is not a king"
            (is-tournament-king? t1 "c") false)
(cassert-eq "3-cycle: all three vertices are kings"
            (length (tournament-kings c3)) 3)

# kings are not always the max-out-degree vertices: "c" has out-degree 1
# (where max is 2) yet is a king, via c -> d -> b / c -> d -> a
(def kmix (make-digraph))
(add-nodes kmix @["a" "b" "c" "d"])
(add-edges kmix @[["a" "b" 1] ["a" "c" 1] ["b" "c" 1]
                  ["c" "d" 1] ["d" "a" 1] ["d" "b" 1]])

(cassert-arr-eq "kings can have below-max out-degree"
                (tournament-kings kmix) @["a" "c" "d"])
(cassert-eq "below-max vertex c is a king"
            (is-tournament-king? kmix "c") true)
(cassert-eq "vertex b is not a king"
            (is-tournament-king? kmix "b") false)

(print "\n* structural classes")

(cassert-eq "t1 transitive" (transitive-tournament? t1) true)
(cassert-eq "t1 not strong" (strong-tournament? t1) false)
(cassert-eq "t1 not regular" (regular-tournament? t1) false)
(cassert-eq "c3 strong" (strong-tournament? c3) true)
(cassert-eq "c3 regular" (regular-tournament? c3) true)
(cassert-eq "c3 not transitive" (transitive-tournament? c3) false)


(print-results)
