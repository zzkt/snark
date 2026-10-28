# -*- mode: janet; -*-
# graphs/test/random-graph: Erdős–Rényi & Barabási–Albert generators

(import ./helper :prefix "")
(import ../graphs :prefix "")

(print "\n* Erdos-Renyi G(n,p)")

# G(10, 0.0) has no edges (deterministic with seed)
(def g0 (random-graph 10 0.0 1234))
(cassert-eq "G(10,0) node count" (node-count g0) 10)
(cassert-eq "G(10,0) edge count" (edge-count g0) 0)

# G(10, 1.0) is complete K10
(def g1 (random-graph 10 1.0 1234))
(cassert-eq "G(10,1) node count" (node-count g1) 10)
(cassert-eq "G(10,1) is K10" (edge-count g1) 45)

# G(10, 0.5) has a non-trivial number of edges between 0 and 45
(def g5a (random-graph 10 0.5 99))
(def g5b (random-graph 10 0.5 99))
(cassert-eq "G(10,0.5) reproducible with same seed"
            (edge-count g5a) (edge-count g5b))
(cassert-eq "G(10,0.5) has 0 < edges < 45"
            (and (> (edge-count g5a) 0)
                 (< (edge-count g5a) 45)) true)

(print "\n* Erdos-Renyi G(n,m)")

(def gm (random-graph-edges 8 10 7))
(cassert-eq "G(8,10) node count" (node-count gm) 8)
(cassert-eq "G(8,10) exactly 10 edges" (edge-count gm) 10)

# no duplicates -> verify no multi-edges on a complete graph request
(def gfull (random-graph-edges 5 10 1))
(cassert-eq "G(5,10) is K5"
            (edge-count gfull) 10)
(cassert-eq "G(5,10) max possible edges valid"
            (= (length (nodes gfull)) 5) true)

(print "\n* Barabasi-Albert")

(def ba (barabasi-albert 20 3 2 42))
(cassert-eq "BA(20,3,2) node count" (node-count ba) 20)

# m0 clique of 3
(cassert-eq "BA(20,3,2) edge count" (edge-count ba) (+ 3 (* 17 2)))

(def ba2 (barabasi-albert 10 2 1 42))
(cassert-eq "BA(10,2,1) node count" (node-count ba2) 10)
(cassert-eq "BA(10,2,1) edge count" (edge-count ba2) (+ 1 (* 8 1)))

(print-results)
