# -*- mode: janet; -*-
# graph/test/cores: k-core decomposition, shells, crust, corona, onion layers

(import ./helper :prefix "")
(import ../graphs :prefix "")

# Two triangles sharing node c with a pendant t-u attached via t-a
# core numbers: a,b,c,d,e -> 2; t,u -> 1
(def g (make-graph @{}))
(add-nodes g @["a" "b" "c" "d" "e" "t" "u"])
(add-edges g @[["a" "b"] ["b" "c"] ["a" "c"] ["c" "d"]
               ["d" "e"] ["e" "c"] ["a" "t"] ["t" "u"]])

(cassert-eq "core-number a" (get (core-number g) "a") 2)
(cassert-eq "core-number c" (get (core-number g) "c") 2)
(cassert-eq "core-number t" (get (core-number g) "t") 1)
(cassert-eq "core-number u" (get (core-number g) "u") 1)

# k-core 2 keeps the two-triangle structure (5 nodes)
(def core2 (k-core g 2))
(cassert-eq "k-core 2 node count" (length (nodes core2)) 5)
(cassert-eq "k-core 3 node count" (length (nodes (k-core g 3))) 0)

# k-shell: only nodes with exact core number
(cassert-eq "k-shell 2 count" (length (nodes (k-shell g 2))) 5)
(cassert-eq "k-shell 1 count" (length (nodes (k-shell g 1))) 2)

# k-crust: core number <= k
(cassert-eq "k-crust 1 count" (length (nodes (k-crust g 1))) 2)
(cassert-eq "k-crust 2 count" (length (nodes (k-crust g 2))) 7)

# k-corona: k-core nodes with at most k core-neighbours
(cassert-eq "k-corona 2 count" (length (nodes (k-corona g 2))) 4)

# onion layers (networkx/go onion decomposition)
# round 1 peels u (min degree 1)
# round 2 peels t (min degree 1) -> layer 2
# round 3 peels a,b,d,e -> layer 3
# round 4 peels c -> layer 4
(def layers (onion-layers g))
(cassert-eq "onion u" (get layers "u") 1)
(cassert-eq "onion t" (get layers "t") 2)
(cassert-eq "onion a" (get layers "a") 3)
(cassert-eq "onion b" (get layers "b") 3)
(cassert-eq "onion d" (get layers "d") 3)
(cassert-eq "onion e" (get layers "e") 3)
(cassert-eq "onion c" (get layers "c") 4)

# K4: every node is 3-core
(def k4 (make-graph @{}))
(add-nodes k4 @["p" "q" "r" "s"])
(add-edges k4 @[["p" "q"] ["p" "r"] ["p" "s"]
                ["q" "r"] ["q" "s"] ["r" "s"]])
(cassert-eq "K4 core-number"
            (get (core-number k4) "p") 3)
(cassert-eq "K4 k-core 3 count"
            (length (nodes (k-core k4 3))) 4)
(cassert-eq "K4 onion layer"
            (get (onion-layers k4) "p") 1)

# Randomized equivalence
(defn naive-onion-layers
  "Reference round-based onion decomposition."
  [g]
  (def ns (nodes g))
  (def remaining @{})
  (def deg @{})
  (def layer @{})
  (each u ns
    (put remaining u true)
    (put deg u (length (neighbours g u)))
    (put layer u 0))
  (var core 1)
  (var ln 1)
  (while (> (length (keys remaining)) 0)
    (var mins 9007199254740991)
    (each [u _] (pairs remaining)
      (when (< (get deg u) mins)
        (set mins (get deg u))))
    (when (> mins core) (set core mins))
    (def round @[])
    (each [u _] (pairs remaining)
      (when (<= (get deg u) core)
        (array/push round u)))
    (each v round
      (put layer v ln)
      (each w (neighbours g v)
        (when (get remaining w)
          (put deg w (max 0 (- (get deg w) 1)))))
      (put remaining v nil))
    (++ ln))
  layer)

(defn random-g
  "Random graph on 0..n-1, each edge present with probability p."
  [n p]
  (def g (make-graph @{}))
  (each i (range n) (add-node g i))
  (for i 0 n
    (for j (+ i 1) n
      (when (< (math/random) p) (add-edge g i j))))
  g)

(math/seedrandom 20260921)

(var mismatches 0)
(var trials 0)
(for iter 0 150
  (def n (+ 2 (math/floor (* (math/random) 8))))
  (def gg (random-g n 0.35))
  (def a (onion-layers gg))
  (def b (naive-onion-layers gg))
  (var same true)
  (each [u _] (pairs a)
    (unless (= (get a u) (get b u)) (set same false)))
  (unless same (++ mismatches) (print "  ✗ onion mismatch on graph with " n " nodes"))
  (++ trials))

(cassert-eq "randomized onion equivalence" mismatches 0)

(print-results)
