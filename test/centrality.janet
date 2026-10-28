# -*- mode: janet; -*-
# graphs/test/centrality: eigenvector, Katz, harmonic, percolation centralities

(import ./helper :prefix "")
(import ../graphs :prefix "")

(defn ≈ [a b] (< (math/abs (- a b)) 1e-6))

# Star K1,3. center c, leaves l1..l3
(def star (make-graph @{}))
(add-nodes star @["c" "l1" "l2" "l3"])
(add-edges star @[["c" "l1"] ["c" "l2"] ["c" "l3"]])

# Eigenvector. center max-normalized to 1, leaves 1/sqrt(3)
(def ev (eigenvector-centrality star))
(cassert-eq "eigenvector center"
            (≈ (get ev "c") 1.0) true)

(cassert-eq "eigenvector leaf"
            (≈ (get ev "l1") 0.577350269) true)

(cassert-eq "eigenvector leaves equal"
            (≈ (get ev "l1") (get ev "l2")) true)

# Katz. alpha = 1/(3+1) = 0.25, beta = 1: center 28/13, leaves 20/13
(def kz (katz-centrality star))

(cassert-eq "katz center"
            (≈ (get kz "c") 2.153846154) true)

(cassert-eq "katz leaf"
            (≈ (get kz "l1") 1.538461538) true)

(def kz2 (katz-centrality star 0.2 1.0 true))
(cassert-eq "katz normalized max 1"
            (≈ (get kz2 "c") 1.0) true)

# Harmonic. center 3 (three leaves at distance 1), leaf 2 (1 + 1/2 + 1/2)
(def hm (harmonic-centrality star))
(cassert-eq "harmonic center"
            (≈ (get hm "c") 3.0) true)

(cassert-eq "harmonic leaf"
            (≈ (get hm "l1") 2.0) true)

# Harmonic normalized divides by H(3) = 1 + 1/2 + 1/3
(def hmn (harmonic-centrality star true))
(cassert-eq "harmonic normalized center"
            (≈ (get hmn "c") (/ 3.0 1.833333333)) true)

# Percolation on a path. center node dominates
(def path (make-graph @{}))
(add-nodes path @["n1" "n2" "n3" "n4" "n5"])
(add-edges path @[["n1" "n2"] ["n2" "n3"] ["n3" "n4"] ["n4" "n5"]])

(def pc (percolation-centrality
         path @{"n1" 0.0 "n2" 0.5 "n3" 1.0 "n4" 0.5 "n5" 0.0}))

(cassert-eq "percolation n3 dominates n2"
            (> (math/abs (get pc "n3")) (math/abs (get pc "n2"))) true)

(cassert-eq "percolation n2 = n4 symmetric"
            (≈ (get pc "n2") (get pc "n4")) true)

(cassert-eq "percolation n1 = 0"
            (≈ (get pc "n1") 0.0) true)

# Eigenvector on empty graph
(cassert-eq "eigenvector empty"
            (length (eigenvector-centrality (make-graph @{}))) 0)


# Degree centralities on a :multidigraph
(print "\n* centrality: :multidigraph degree")

(def md (make-multidigraph @{}))
(add-node md "a")
(add-node md "b")
(add-node md "c")
(add-node md "sink")
(add-edge md "a" "b")
(add-edge md "a" "b")  # parallel arc
(add-edge md "b" "c")
(add-edge md "c" "a")

(def mdd (multidigraph-degree-centrality md))
(cassert-eq "multidigraph-degree: covers every node"
            (length mdd) 4)

(cassert-eq "multidigraph-degree: a parallel arc counts once"
            (≈ (get (get mdd "a") :out) 0.333333333) true)

(cassert-eq "multidigraph-degree: c has one out-neighbour"
            (≈ (get (get mdd "c") :out) 0.333333333) true)

(cassert-eq "multidigraph-degree: b has one in and one out"
            (and (≈ (get (get mdd "b") :in) 0.333333333)
                 (≈ (get (get mdd "b") :out) 0.333333333)) true)

(cassert-eq "multidigraph-degree: total is in + out"
            (≈ (get (get mdd "b") :total) 0.666666667) true)

(cassert-eq "multidigraph-degree: a node with no arcs is zero on all three"
            (and (≈ (get (get mdd "sink") :out) 0.0)
                 (≈ (get (get mdd "sink") :in) 0.0)
                 (≈ (get (get mdd "sink") :total) 0.0)) true)

(def mdi (multidigraph-in-degree-centrality md))
(cassert-eq "multidigraph-in-degree: covers every node"
            (length mdi) 4)

(cassert-eq "multidigraph-in-degree: a is reached only by c"
            (≈ (get mdi "a") 0.333333333) true)

(cassert-eq "multidigraph-in-degree: b has two arcs in but ONE neighbour"
            (≈ (get mdi "b") 0.333333333) true)

(cassert-eq "multidigraph-in-degree: c has one in-neighbour"
            (≈ (get mdi "c") 0.333333333) true)

(cassert-eq "multidigraph-in-degree: a node with no arcs is zero"
            (≈ (get mdi "sink") 0.0) true)

(cassert-eq "multidigraph-in-degree: the :in half agrees with the degree table"
            (all (fn [u] (≈ (get mdi u) (get (get mdd u) :in)))
                 ["a" "b" "c" "sink"]) true)

# A one-node graph has no pair to normalize by
(def md1 (make-multidigraph))
(add-node md1 "only")

(cassert-eq "multidigraph-degree: single node avoids dividing by zero"
            (≈ (get (get (multidigraph-degree-centrality md1) "only") :total) 0.0) true)

(cassert-eq "multidigraph-in-degree: single node avoids dividing by zero"
            (≈ (get (multidigraph-in-degree-centrality md1) "only") 0.0) true)

(print-results)
