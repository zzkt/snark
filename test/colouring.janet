# -*- mode: janet; -*-
# graphs/test/colouring: greedy colouring, exact chromatic, and Turán

(import ./helper :prefix "")
(import ../graphs :prefix "")
(import ./data/graphs :prefix "fx-")

(print "\n* greedy colouring")

(def c4 (fx-cycle 4 fx-alpha->name))
(def gc4 (greedy-colour c4))
(cassert-eq "greedy C4 colouring is proper"
            (proper-colouring? c4 gc4) true)

(cassert-eq "greedy C4 uses 2 colours"
            (apply max (values gc4)) 2)

(def gk5 (greedy-colour (fx-complete 5 fx-idx->int)))
(cassert-eq "greedy K5 uses 5 colours"
            (apply max (values gk5)) 5)

(print "\n* proper-colouring?")

(cassert-eq "uncoloured vertex rejected"
            (proper-colouring? c4 @{}) false)

(cassert-eq "proper 2-colouring of C4 accepted"
  (proper-colouring? c4 @{"a" 1 "b" 2 "c" 1 "d" 2}) true)


(print "\n* chromatic-number")

(cassert-eq "empty graph χ = 0"
            (chromatic-number (make-graph @{})) 0)

(cassert-eq "single vertex χ = 1"
  (chromatic-number (do (def sg (make-graph @{})) (add-node sg "s") sg)) 1)

(cassert-eq "K3 χ = 3"
            (chromatic-number (fx-complete 3 fx-idx->int)) 3)

(cassert-eq "C4 χ = 2"
            (chromatic-number c4) 2)

(cassert-eq "K5 χ = 5"
            (chromatic-number (fx-complete 5 fx-idx->int)) 5)

(def p4 (make-graph))
(add-nodes p4 "a" "b" "c" "d")
(add-edges p4 ["a" "b"] ["b" "c"] ["c" "d"])

(cassert-eq "path χ = 2" (chromatic-number p4) 2)

(print "\n* Turán graphs and numbers")

(cassert-eq "T(3,2) has 2 edges" ((make-turan-graph 3 2) :edge-count) 2)
(cassert-eq "T(5,2) has 6 edges" ((make-turan-graph 5 2) :edge-count) 6)
(cassert-eq "T(5,2) χ = 2" (chromatic-number (make-turan-graph 5 2)) 2)
(cassert-eq "T(7,3) has 16 edges" ((make-turan-graph 7 3) :edge-count) 16)
(cassert-eq "T(7,3) χ = 3" (chromatic-number (make-turan-graph 7 3)) 3)
(cassert-eq "turan-number(5,2) = 6" (turan-number 5 2) 6)
(cassert-eq "turan-number(6,3) = 12" (turan-number 6 3) 12)
(cassert-eq "turan-number(7,3) = 16" (turan-number 7 3) 16)
(cassert-err "T(n,r) requires r ≤ n" (make-turan-graph 3 5))
(cassert-err "T(n,r) requires r ≥ 1" (make-turan-graph 3 0))

(print "\n* edge colouring (Vizing)")

(def k3-edge (make-graph))
(add-nodes k3-edge 1 2 3)
(add-edges k3-edge [1 2] [2 3] [3 1])

(def c4-edge (make-graph))
(add-nodes c4-edge 1 2 3 4)
(add-edges c4-edge [1 2] [2 3] [3 4] [4 1])

(def c5-edge (make-graph))
(add-nodes c5-edge 1 2 3 4 5)
(add-edges c5-edge [1 2] [2 3] [3 4] [4 5] [5 1])

(cassert-eq "K3 χ′ = 3 (class 2)"
            (chromatic-index k3-edge) 3)
(cassert-eq "K3 class 2"
            (get (vizing-edge-colour k3-edge) :class) 2)
(cassert-eq "C4 χ′ = 2 (class 1)"
            (chromatic-index c4-edge) 2)
(cassert-eq "C4 class 1"
            (get (vizing-edge-colour c4-edge) :class) 1)
(cassert-eq "C5 χ′ = 3 (class 2)"
            (chromatic-index c5-edge) 3)
(cassert-eq "K4 χ′ = 3 (class 1)"
            (chromatic-index (fx-complete 4 fx-idx->int)) 3)
(cassert-eq "K5 χ′ = 5 (class 2)"
            (chromatic-index (fx-complete 5 fx-idx->int)) 5)
(cassert-eq "path P3 χ′ = 2"
            (chromatic-index p4) 2)
(cassert-eq "empty graph χ′ = 0"
            (chromatic-index (make-graph)) 0)

(cassert-eq "Vizing C5 colouring is proper"
  (proper-edge-colour? c5-edge (get (vizing-edge-colour c5-edge) :colouring)) true)

(cassert-eq "Vizing C4 colouring uses Δ colours"
  (apply max (values (get (vizing-edge-colour c4-edge) :colouring))) 2)

(cassert-eq "greedy C5 colouring is proper"
  (proper-edge-colour? c5-edge (edge-colour-greedy c5-edge)) true)

(cassert-eq "greedy K5 uses ≤ 2Δ-1 colours"
  (<= (apply max (values (edge-colour-greedy (fx-complete 5 fx-idx->int)))) 7) true)

(print-results)
