# -*- mode: janet; -*-
# graphs/test/multigraph: construction of multigraph & multidigraph, mutators

(import ./helper :prefix "")
(import ../graphs  :prefix "")

# multigraph

(print "\n* multigraph construction")

(def mg1 (make-multigraph))
(cassert-eq "empty multigraph node-count" (node-count mg1) 0)
(cassert-eq "empty multigraph edge-count" (edge-count mg1) 0)

(add-edges mg1 @[["a" "b"] ["a" "b"] ["a" "b"]])
(cassert-eq "parallel edges count"
            (edge-count mg1) 3)
(cassert-eq "edge-count-between"
            (multigraph-edge-count-between mg1 "a" "b") 3)
(cassert-eq "neighbours a"
            (length (neighbours mg1 "a")) 1)

(multigraph-remove-edge mg1 "a" "b")
(cassert-eq "remove-edge count"
            (multigraph-edge-count-between mg1 "a" "b") 2)

(remove-edge mg1 "a" "b")
(cassert-eq "remove-all-edges"
            (multigraph-edge-count-between mg1 "a" "b") 0)


# multidigraph

(print "\n* multidigraph construction")

(def mdg1 (make-multidigraph))

(add-edges mdg1 @[["a" "b"] ["a" "b"] ["b" "a"]])
(cassert-eq "directed parallel edges"
            (multidigraph-edge-count-between mdg1 "a" "b") 2)
(cassert-eq "reverse direction"
            (multidigraph-edge-count-between mdg1 "b" "a") 1)
(cassert-eq "out-degree a"
            (out-degree mdg1 "a") 2)
(cassert-eq "in-degree a"
            (in-degree mdg1 "a") 1)


(print "\n* multigraph node data")

(def mgd (make-multigraph))
(add-edge mgd "x" "y")
(cassert-eq "default multigraph node data"
            (multigraph-node-data mgd "x") "x")

(multigraph-set-node-data mgd "x" 42)
(cassert-eq "set multigraph node data"
            (multigraph-node-data mgd "x") 42)
(cassert-eq "unset remains default"
            (multigraph-node-data mgd "y") "y")
(cassert-eq "multigraph nodes-table keys"
            (arr-eq (keys (nodes-table mgd)) (nodes mgd)) true)

(print "\n* multidigraph node data")

(def mdgd (make-multidigraph))
(add-edge mdgd "p" "q")

(cassert-eq "default multidigraph node data"
            (multidigraph-node-data mdgd "p") "p")

(set-node-data mdgd "p" {:kind "start"})
(cassert-eq "set multidigraph node data"
            (node-data mdgd "p") {:kind "start"})

(multidigraph-set-node-data mdgd "r" "extra")
(cassert-eq "multidigraph add via set-node-data"
            (node-data mdgd "r") "extra")
(cassert-eq "multidigraph nodes-table keys"
            (arr-eq (keys (nodes-table mdgd)) (nodes mdgd)) true)

(remove-node mdgd "p")
(cassert-eq "multidigraph remove clears data"
            (node-data mdgd "p") nil)

(print "\n* positional construction")

(def posg (make-multidigraph @{"a" @{"b" 1}} @{"b" @{"a" 1}} @[]))
(cassert-arr-eq "positional args map out first"
                (out-neighbours posg "a") @[(tuple "b" 1)])
(cassert-arr-eq "positional args map in second"
                (in-neighbours posg "b") @["a"])
(cassert-eq "positional nodes arg third"
            (length (keys (nodes-table posg))) 0)

(print "\n* in-neighbours on an asymmetric multidigraph")

(def hb (make-multidigraph :out @{"a" @{"b" 1} "c" @{}} :in @{"b" @{"a" 1}}))
(cassert-arr-eq "in-neighbours node absent from :in → empty"
                (in-neighbours hb "a") @[])
(cassert-arr-eq "in-neighbours neighbour present → found"
                (in-neighbours hb "b") @["a"])
(cassert-eq "in-degree node absent from :in → 0"
            (in-degree hb "c") 0)



(print "\n* multigraph degree with non-numeric labels")

(def mgdeg (make-multigraph))
(add-edges mgdeg @[["a" "b"] ["b" "c"] ["a" "c"]])
(cassert-eq "degree string label"
            (degree mgdeg "c") 2)
(cassert-eq "degree absent string label"
            (degree mgdeg "z") 0)

(def mgkw (make-multigraph))
(add-edges mgkw @[["p" "q"] ["q" "r"]])
(cassert-eq "degree keyword label"
            (degree mgkw "q") 2)
(cassert-eq "degree absent keyword label"
            (degree mgkw "z") 0)

(def mgloop (make-multigraph))
(add-edges mgloop @[["x" "y"] ["x" "y"] ["x" "x"]])
(cassert-eq "degree parallel + self-loop double"
            (degree mgloop "x") 4)
(cassert-eq "degree other endpoint multiplicity"
            (degree mgloop "y") 2)


(print "\n* multidigraph in/out degree counts multiplicity")

(def mddeg (make-multidigraph))
(add-edges mddeg @[["a" "b"] ["a" "b"] ["b" "a"]])
(cassert-eq "mdg out-degree counts parallel"
            (out-degree mddeg "a") 2)
(cassert-eq "mdg in-degree counts parallel"
            (in-degree mddeg "b") 2)


(print "\n* Kahn topological sort handles parallel edges")

(def mgtopo (make-multidigraph))
(add-edges mgtopo @[["a" "b"] ["a" "b"] ["b" "c"]])
(cassert-arr-eq "leaf topo-sort on parallel DAG"
                (topological-sort mgtopo) @["a" "b" "c"])


(print "\n* in-degree counts multiplicity")

(def mdux (make-multidigraph))
(add-edges mdux @[["a" "b"] ["a" "b"] ["b" "a"]])
(cassert-eq "util in-degree counts parallel arcs"
            (in-degree mdux "b") 2)
(cassert-eq "util in-degree other endpoint"
            (in-degree mdux "a") 1)
(cassert-eq "util in-degree absent node"
            (in-degree mdux "z") 0)


(def mhedge (make-multigraph))
(add-edge mhedge "a" "b")
(cassert-eq "has-edge? absent first node -> false"
            (has-edge? mhedge "zz" "a") false)
(cassert-eq "has-edge? absent second node -> false"
            (has-edge? mhedge "a" "zz") false)
(cassert-eq "has-edge? present -> true"
            (has-edge? mhedge "a" "b") true)

(def mdhedge (make-multidigraph))
(add-edge mdhedge "a" "b")
(cassert-eq "multidigraph-has-edge? absent first node -> false"
            (has-edge? mdhedge "zz" "a") false)
(cassert-eq "multidigraph-has-edge? absent second node -> false"
            (has-edge? mdhedge "a" "zz") false)
(cassert-eq "multidigraph-has-edge? present -> true"
            (has-edge? mdhedge "a" "b") true)
(cassert-eq "multidigraph-has-edge? reverse arc absent -> false"
            (has-edge? mdhedge "b" "a") false)

# removing nodes
(def mhc (make-multidigraph))
(add-edge mhc "a" "b")
(def before (mhc :node-count))
(remove-node mhc "zzz")
(cassert-eq "removing an absent node leaves node-count alone"
            (mhc :node-count) before)

(remove-node mhc "a")
(def after-first (mhc :node-count))
(remove-node mhc "a")
(cassert-eq "removing the same node twice leaves the count alone"
            (mhc :node-count) after-first)

(cassert-eq "count is not negative"
            (>= (mhc :node-count) 0) true)

(print-results)
