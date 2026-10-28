# -*- mode: janet; -*-
# graphs/test/connectivity: articulation points, bridges, biconnected components

(import deft :prefix "")
(import ./helper :prefix "")
(import ../graphs :prefix "")
(import ./data/order :prefix "")

#  tests use normalizers from data/order so edge-pairs/components compare
#  regardless of orientation. sorted-str, edge-set-str, and biconn-str.
(def qstr (fn [q] (joined-str q "/")))

(print "\n* digraph fixtures")

(def h (make-digraph))
(add-nodes h @["0" "1" "2" "3" "4"])
(add-edges h ["0" "3" 1] ["1" "3" 1] ["2" "4" 1] ["3" "4" 1])

(cassert-eq "digraph articulation-points (hanging tree)"
            (sorted-str (articulation-points h)) "3,4")

(cassert-eq "digraph bridges (hanging tree)"
            (edge-set-str (bridges h)) "0/3,1/3,2/4,3/4")

(cassert-eq "digraph biconnected-components (hanging tree)"
            (biconn-str (biconnected-components h)) "0/3;1/3;2/4;3/4")


(def t (make-digraph))
(add-nodes t @["0" "1" "2" "3"])
(add-edges t ["0" "1" 1] ["1" "2" 1] ["2" "0" 1] ["1" "3" 1])

(cassert-eq "digraph articulation-points (triangle w/ tail)"
            (sorted-str (articulation-points t)) "1")
(cassert-eq "digraph bridges (triangle w/ tail)"
            (edge-set-str (bridges t)) "1/3")
(cassert-eq "digraph biconnected (triangle w/ tail)"
            (biconn-str (biconnected-components t)) "0/1|0/2|1/2;1/3")

(def p (make-digraph))
(add-nodes p @["0" "1" "2" "3"])
(add-edges p ["0" "1" 1] ["1" "2" 1] ["2" "3" 1])

(cassert-eq "digraph articulation-points (path)"
            (sorted-str (articulation-points p)) "1,2")
(cassert-eq "digraph bridges (path)"
            (edge-set-str (bridges p)) "0/1,1/2,2/3")
(cassert-eq "digraph biconnected (path)"
            (biconn-str (biconnected-components p)) "0/1;1/2;2/3")

(def cc (make-digraph))
(add-nodes cc @["0" "1" "2" "3"])
(add-edges cc ["0" "1" 1] ["2" "3" 1])

(cassert-eq "connected-components (two components)"
            (sorted-str (map |(string/join (sort (map string $)) ",")
                             (connected-components cc))) "0,1,2,3")

(print "\n* weighted-graph")

(def w (make-weighted-graph))
(add-nodes w @["0" "1" "2" "3" "4"])
(add-edge w "0" "3" 5)
(add-edge w "1" "3" 9)
(add-edge w "2" "4" 2)
(add-edge w "3" "4" 7)

(cassert-eq "weighted articulation-points (hanging tree)"
            (sorted-str (articulation-points w)) "3,4")
(cassert-eq "weighted bridges (hanging tree)"
            (edge-set-str (bridges w)) "0/3,1/3,2/4,3/4")
(cassert-eq "weighted biconnected (hanging tree)"
            (biconn-str (biconnected-components w)) "0/3;1/3;2/4;3/4")
(cassert-eq "weighted connected-components (single)"
            (length (connected-components w)) 1)

(print "\n* tree (backing digraph)")

(def tr-tree (make-digraph))
(add-nodes tr-tree @["0" "1" "2" "3" "4"])
(add-edge tr-tree "0" "1" 1)
(add-edge tr-tree "0" "2" 1)
(add-edge tr-tree "1" "3" 1)
(add-edge tr-tree "1" "4" 1)

(def tr (make-tree tr-tree "0" @{"1" "0" "2" "0" "3" "1" "4" "1"}))
(def td (tree->digraph tr))

(cassert-eq "tree->digraph articulation-points (root at 0)"
            (sorted-str (articulation-points td)) "0,1")
(cassert-eq "tree->digraph bridges (root at 0)"
            (edge-set-str (bridges td)) "0/1,0/2,1/3,1/4")

(print "\n* Tarjan LCA")

(def qs @[@["3" "4"] @["3" "2"] @["0" "4"]])
(def lres (tarjan-lca td "0" qs))
(def lmap @{})
(each q (keys lres) (put lmap (qstr q) (string (get lres q))))

(cassert-eq "LCA 3,4 = 1" (get lmap "3/4") "1")
(cassert-eq "LCA 3,2 = 0" (get lmap "2/3") "0")
(cassert-eq "LCA 0,4 = 0" (get lmap "0/4") "0")
(cassert-eq "LCA result covers 3 pairs" (length (keys lres)) 3)

(print "\n* Tarjan LCA iterative (explicit stack)")

(def ilres (tarjan-lca-iterative td "0" qs))
(def ilmap @{})
(each q (keys ilres) (put ilmap (qstr q) (string (get ilres q))))
(cassert-eq "iterative LCA 3,4 = 1" (get ilmap "3/4") "1")
(cassert-eq "iterative LCA 3,2 = 0" (get ilmap "2/3") "0")
(cassert-eq "iterative LCA 0,4 = 0" (get ilmap "0/4") "0")
(cassert-eq "iterative LCA result covers 3 pairs" (length (keys ilres)) 3)
(cassert-eq "iterative matches recursive on every pair"
            (deep= ilres lres) true)

(print "\n* Tarjan LCA iterative on a deep chain (stack safety)")

(defn make-chain [n]
  (def g (make-digraph (seq [i :range [0 n]] (string i)) @{}))
  (for i 0 (- n 1) (add-edge g (string i) (string (+ i 1)) 1))
  g)

(def deep-chain (make-chain 5000))
(def dq @[["4999" "0"] ["2500" "4000"]])
(def dres (tarjan-lca-iterative deep-chain "0" dq))
(cassert-eq "deep chain lca(4999,0) = 0" (get dres (dq 0)) "0")
(cassert-eq "deep chain lca(2500,4000) = 2500" (get dres (dq 1)) "2500")

(print "\n* Tarjan LCA on weighted-graph")

(def lw (make-weighted-graph))
(add-nodes lw @["0" "1" "2" "3" "4"])
(add-edge lw "0" "1" 1)
(add-edge lw "0" "2" 1)
(add-edge lw "1" "3" 1)
(add-edge lw "1" "4" 1)
(def wqs @[@["4" "3"] @["4" "2"]])
(def wlres (tarjan-lca lw "0" wqs))
(def wlmap @{})
(each q (keys wlres) (put wlmap (qstr q) (string (get wlres q))))
(cassert-eq "weighted LCA 4,3 = 1" (get wlmap "3/4") "1")
(cassert-eq "weighted LCA 4,2 = 0" (get wlmap "2/4") "0")
(cassert-eq "weighted LCA covers 2 pairs" (length (keys wlres)) 2)

(print "\n* generic connectivity analysis")

(print "\n* cycle")

(def cyc (make-digraph))
(add-nodes cyc @["0" "1" "2" "3"])
(add-edge cyc "0" "1" 1)
(add-edge cyc "1" "2" 1)
(add-edge cyc "2" "3" 1)
(add-edge cyc "3" "0" 1)
(cassert-eq "cycle articulation-points (none)"
            (sorted-str (articulation-points cyc)) "")
(cassert-eq "cycle bridges (none)"
            (edge-set-str (bridges cyc)) "")
(cassert-eq "cycle biconnected (single block)"
            (length (biconnected-components cyc)) 1)
(cassert-eq "cycle connected-components (single)"
            (length (connected-components cyc)) 1)

(print "\n* self-loops")

(def sl (make-digraph @["a"] @{}))
(add-edge sl "a" "a" 1)
(cassert-eq "self-loop articulation-points (none)"
            (sorted-str (articulation-points sl)) "")
(cassert-eq "self-loop bridges (none)"
            (edge-set-str (bridges sl)) "")
(cassert-eq "self-loop connected-components"
            (length (connected-components sl)) 1)

(print "\n* empty & isolated")

(def e (make-digraph))
(cassert-eq "empty connected-components"
            (length (connected-components e)) 0)
(cassert-eq "empty articulation-points"
            (length (articulation-points e)) 0)

(def iso (make-digraph @["x"] @{}))
(cassert-eq "isolated node connected-components"
            (length (connected-components iso)) 1)
(cassert-eq "isolated node articulation-points (none)"
            (length (articulation-points iso)) 0)

(print "\n* multicomponent graph (path + isolated)")

(def mc (make-digraph))
(add-nodes mc @["a" "b" "c" "z"])
(add-edge mc "a" "b" 1)
(add-edge mc "b" "c" 1)
(cassert-eq "multicomponent connected-components"
            (length (connected-components mc)) 2)

(print "\n* path finding")

(def pg (make-digraph))
(add-nodes pg @["a" "b" "c" "d"])
(add-edge pg "a" "b" 1)
(add-edge pg "b" "c" 1)
(cassert-eq "graph-path-exists? a->c (undirected reach)"
            (graph-path-exists? pg "a" "c") true)
(cassert-eq "graph-path-exists? c->a (undirected reach)"
            (graph-path-exists? pg "c" "a") true)
(cassert-eq "graph-path-exists? a->d (isolated)"
            (graph-path-exists? pg "a" "d") false)
(cassert-eq "graph-shortest-path a->c"
            (string/join (graph-shortest-path pg "a" "c") "->") "a->b->c")
(cassert-eq "graph-shortest-path a->a"
            (string/join (graph-shortest-path pg "a" "a") "->") "a")
(cassert-eq "graph-shortest-path a->d (nil)"
            (graph-shortest-path pg "a" "d") nil)

(print "\n* multidigraph (directed) variants")

(def mdg (make-multidigraph))
(add-nodes mdg @["a" "b" "c" "d"])
(add-edges mdg @[["a" "b"] ["b" "c"]])
(cassert-eq "multidigraph-path-exists? a->c"
            (multidigraph-path-exists? mdg "a" "c") true)
(cassert-eq "multidigraph-path-exists? c->a (none)"
            (multidigraph-path-exists? mdg "c" "a") false)
(cassert-eq "multidigraph-shortest-path a->c"
            (string/join (multidigraph-shortest-path mdg "a" "c") "->") "a->b->c")
(cassert-eq "multidigraph-shortest-path c->a (nil)"
            (multidigraph-shortest-path mdg "c" "a") nil)
(cassert-eq "multidigraph-connected? (a chain + isolated d)"
            (multidigraph-connected? mdg) false)

(def mdg2 (make-multidigraph))
(add-nodes mdg2 @["0" "1" "2" "3"])
(add-edges mdg2 @[["0" "1"] ["2" "3"]])
(cassert-eq "multidigraph-connected-components (2 weak)"
            (length (multidigraph-connected-components mdg2)) 2)

# weak connectivity: a->b and c->b share node b (one weak component)
# even though no directed path connects a to c.
(def mdg3 (make-multidigraph))
(add-nodes mdg3 @["a" "b" "c"])
(add-edges mdg3 @[["a" "b"] ["c" "b"]])
(cassert-eq "weakly connected (a->b, c->b)"
            (multidigraph-connected? mdg3) true)
(cassert-eq "weakly connected components count"
            (length (multidigraph-connected-components mdg3)) 1)
(cassert-eq "weakly connected components size"
            (length (first (multidigraph-connected-components mdg3))) 3)

(print "\n* edge/vertex connectivity")

(def conn-k3 (make-graph))
(add-nodes conn-k3 @["a" "b" "c"])
(add-edge conn-k3 "a" "b")
(add-edge conn-k3 "b" "c")
(add-edge conn-k3 "a" "c")
(cassert-eq "K3 λ = 2"
            (edge-connectivity conn-k3) 2)
(cassert-eq "K3 κ = 2"
            (vertex-connectivity conn-k3) 2)

(def conn-c4 (make-graph))
(add-nodes conn-c4 @["a" "b" "c" "d"])
(add-edge conn-c4 "a" "b")
(add-edge conn-c4 "b" "c")
(add-edge conn-c4 "c" "d")
(add-edge conn-c4 "d" "a")
(cassert-eq "C4 λ = 2"
            (edge-connectivity conn-c4) 2)
(cassert-eq "C4 κ = 2"
            (vertex-connectivity conn-c4) 2)

(def conn-p3 (make-graph))
(add-nodes conn-p3 @["a" "b" "c"])
(add-edge conn-p3 "a" "b")
(add-edge conn-p3 "b" "c")
(cassert-eq "P3 λ = 1"
            (edge-connectivity conn-p3) 1)
(cassert-eq "P3 κ = 1"
            (vertex-connectivity conn-p3) 1)

(def conn-k2 (make-graph))
(add-nodes conn-k2 @["a" "b"])
(add-edge conn-k2 "a" "b")
(cassert-eq "K2 λ = 1"
            (edge-connectivity conn-k2) 1)
(cassert-eq "K2 κ = 1"
            (vertex-connectivity conn-k2) 1)

(def conn-disc (make-graph))
(add-nodes conn-disc @["a" "b" "c" "d"])
(add-edge conn-disc "a" "b")
(add-edge conn-disc "c" "d")
(cassert-eq "disconnected λ = 0"
            (edge-connectivity conn-disc) 0)
(cassert-eq "disconnected κ = 0"
            (vertex-connectivity conn-disc) 0)

(def conn-single (make-graph))
(add-node conn-single "s")
(cassert-eq "single vertex λ = 0"
            (edge-connectivity conn-single) 0)
(cassert-eq "single vertex κ = 0"
            (vertex-connectivity conn-single) 0)

(def conn-mg (make-multigraph))
(add-node conn-mg "a")
(add-node conn-mg "b")
(add-edge conn-mg "a" "b")
(add-edge conn-mg "a" "b")
(cassert-eq "two parallel edges λ = 2"
            (edge-connectivity conn-mg) 2)

(def conn-cd (make-digraph))
(add-nodes conn-cd @["0" "1" "2"])
(each n @["0" "1" "2"]
      (each m @["0" "1" "2"]
            (when (not= n m) (add-edge conn-cd n m 1))))
(cassert-eq "complete digraph κ = n-1"
            (vertex-connectivity conn-cd) 2)

(def conn-cyc (make-digraph))
(add-nodes conn-cyc @["0" "1" "2"])
(add-edge conn-cyc "0" "1" 1)
(add-edge conn-cyc "1" "2" 1)
(add-edge conn-cyc "2" "0" 1)
(cassert-eq "directed 3-cycle κ = 1"
            (vertex-connectivity conn-cyc) 1)
(cassert-err "edge-connectivity rejects digraph"
             (edge-connectivity conn-cyc))

(print "\n* weak connectivity on directed types")

(def wconn-cyc (make-digraph))
(add-nodes wconn-cyc @["a" "b" "c"])
(add-edge wconn-cyc "a" "b" 1)
(add-edge wconn-cyc "b" "c" 1)
(add-edge wconn-cyc "c" "a" 1)
(cassert-eq "dicycle connected? (weak)"
            (connected? wconn-cyc) true)

(def wconn-fork (make-digraph))
(add-nodes wconn-fork @["a" "b" "c"])
(add-edge wconn-fork "a" "b" 1)
(add-edge wconn-fork "c" "b" 1)
(cassert-eq "weak fork (a→b c→b) connected?"
            (connected? wconn-fork) true)

(def wconn-out (make-digraph))
(add-nodes wconn-out @["a" "b" "c"])
(add-edge wconn-out "a" "b" 1)
(cassert-eq "out-star a→b (c isolated) not connected?"
            (connected? wconn-out) false)

(def wcomp-dg (make-digraph))
(add-nodes wcomp-dg @["a" "b" "c" "d"])
(add-edge wcomp-dg "a" "b" 1)
(add-edge wcomp-dg "c" "d" 1)

(def wcomp (connected-components wcomp-dg))
(cassert-eq "digraph connected-components count (weak)"
            (length wcomp) 2)
(cassert-eq "digraph weak component1 has a,b"
            (deep= (sort (wcomp 0)) @["a" "b"]) true)
(cassert-eq "digraph weak component2 has c,d"
            (deep= (sort (wcomp 1)) @["c" "d"]) true)

(def wconn-wg-isolated (make-weighted-graph))
(add-nodes wconn-wg-isolated @["a" "b"])
(cassert-eq "isolated weighted-graph not connected?"
            (connected? wconn-wg-isolated) false)

(def wconn-wg (make-weighted-graph))
(add-nodes wconn-wg @["a" "b" "c"])
(add-edge wconn-wg "a" "b" 1)
(add-edge wconn-wg "b" "c" 1)
(cassert-eq "connected weighted-graph connected?"
            (connected? wconn-wg) true)

(print "\n* node-count resilience")

# constructor nodes must not be duplicated by add-edge/add-node
(def ncp-dg (make-digraph @["a" "b" "c"] @{}))
(add-edge ncp-dg "a" "b" 1)
(cassert-eq "pre-seeded digraph node-count"
            (node-count ncp-dg) 3)

(def ncp-wg (make-weighted-graph @["x" "y"] @{}))
(add-edge ncp-wg "x" "y" 7)
(cassert-eq "pre-seeded weighted-graph node-count"
            (node-count ncp-wg) 2)
(cassert-eq "pre-seeded weighted-graph edge weight"
            (weighted-graph-edge-weight ncp-wg "x" "y") 7)

(def ncp-g (make-graph @{} @["a" "b" "c"]))
(add-edge ncp-g "a" "b")
(cassert-eq "pre-seeded graph node-count"
            (node-count ncp-g) 3)

(def ncp-mg (make-multigraph @{} @["u" "v"]))
(add-edge ncp-mg "u" "v")
(cassert-eq "pre-seeded multigraph node-count"
            (node-count ncp-mg) 2)


(print "\n* multigraph: parallel edges are not bridges (P2b)")

(def pmg (make-multigraph))
(add-nodes pmg "u" "v" "w")
(add-edges pmg @[["u" "v"] ["u" "v"] ["v" "w"]])
(cassert-eq "parallel-edge pair not a bridge"
            (edge-set-str (bridges pmg)) "v/w")

(print-results)
