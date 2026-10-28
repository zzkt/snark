# -*- mode: janet; -*-
# graph/test/cuts: volume, cut-size, boundary, expansion, conductance

(import ./helper :prefix "")
(import ../graphs :prefix "")

(def square (make-graph))
(add-nodes square @["a" "b" "c" "d"])
(add-edges square @[["a" "b"] ["b" "c"] ["c" "d"] ["d" "a"]])

(def sab @["a" "b"])

(cassert-eq "cut-size square {a,b}" (cut-size square sab) 2)
(cassert-eq "volume square {a,b}" (volume square sab) 4)
(cassert-eq "volume complement" (volume square @["c" "d"]) 4)
(cassert-eq "edge-boundary size" (length (edge-boundary square sab)) 2)
(cassert-eq "node-boundary size" (length (node-boundary square sab)) 2)
(cassert-eq "edge-expansion" (edge-expansion square sab) 1)
(cassert-eq "node-expansion" (node-expansion square sab) 1)
(cassert-eq "mixing-expansion" (mixing-expansion square sab) 0.5)
(cassert-eq "normalized-cut-size" (normalized-cut-size square sab) 1)
(cassert-eq "conductance" (conductance square sab) 0.5)
(cassert-eq "boundary-expansion" (boundary-expansion square sab) 1)

# edges with self-loop and a pendant should not inflate cuts
(def loop-g (make-graph))
(add-nodes loop-g @["a" "b" "c" "d"])
(add-edge loop-g "a" "a")
(add-edge loop-g "a" "b")
(add-edge loop-g "b" "c")
(add-edge loop-g "c" "d")
(cassert-eq "self-loop excluded from cut"
            (cut-size loop-g @["a"]) 1)
(cassert-eq "self-loop counted in volume"
            (volume loop-g @["a"]) 3)

# single-node partition: everything degenerate
(def one-g (make-graph))
(add-node one-g "a")
(cassert-eq "edge-expansion single node"
            (edge-expansion one-g @["a"]) 0)
(cassert-eq "conductance single node"
            (conductance one-g @["a"]) nil)
(cassert-eq "boundary-expansion single node"
            (boundary-expansion one-g @["a"]) nil)

# boundary membership
(def sa @["a"])
(cassert-eq "edge-boundary crossing edges"
            (length (edge-boundary square sa)) 2)
(cassert-eq "node-boundary members"
            (length (node-boundary square sa)) 2)

(print-results)
