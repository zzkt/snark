# -*- mode: janet; -*-
# graphs/test/digraph: directed graphs

(import ./helper :prefix "")
(import ../graphs :prefix "")

(print "\n* digraph construction")

(def g (make-digraph))
(cassert-eq "node count initially 0" (digraph-node-count g) 0)
(add-nodes g "A" "B" "C" "D")
(cassert-eq "node count after 4 add-node" (digraph-node-count g) 4)
(add-node g "D")
(cassert-eq "add-node duplicate is a no-op" (digraph-node-count g) 4)
(cassert-eq "edge count initially 0" (digraph-edge-count g) 0)

(print "\n* adding edges")

(add-edge g "A" "B" 3)
(add-edge g "A" "C" -1)
(add-edge g "B" "C" 2)
(add-edge g "A" "Z" 7)
(cassert-eq "add-edge creates missing target node Z"
            (digraph-node-count g) 5)
(cassert-eq "edge count after 4 edges"
            (digraph-edge-count g) 4)
(cassert-eq "raw edge-count after 4 edges"
            (g :edge-count) 4)
(add-edge g "C" "D" 9)
(cassert-eq "new edge increments raw edge-count"
            (g :edge-count) 5)
(remove-edge g "C" "D")
(cassert-eq "remove edge decrements raw edge-count"
            (g :edge-count) 4)

(print "\n* neighbours and degrees")

(cassert-eq "out-degree A" (digraph-out-degree g "A") 3)
(cassert-eq "out-degree unknown node is 0" (digraph-out-degree g "NO") 0)
(cassert-eq "in-degree C" (digraph-in-degree g "C") 2)
(cassert-eq "in-degree Z" (digraph-in-degree g "Z") 1)
(cassert-arr-eq "out-neighbour names of A (sorted)"
  (sort (map |(first $) (digraph-out-neighbours g "A")))
  @["B" "C" "Z"])

(cassert-eq "edge-weight A->B"
            (digraph-edge-weight g "A" "B") 3)
(cassert-eq "edge-weight A->C negative"
            (digraph-edge-weight g "A" "C") -1)
(cassert-eq "edge-weight missing edge is nil"
            (digraph-edge-weight g "B" "A") nil)
(cassert-eq "has-edge A->B"
            (digraph-has-edge? g "A" "B") true)
(cassert-eq "has-edge B->A absent"
            (digraph-has-edge? g "B" "A") false)


(print "\n* reverse")

(def r (digraph-reverse g))
(cassert-eq "reverse node count" (digraph-node-count r) 5)
(cassert-eq "reverse edge count" (digraph-edge-count r) 4)
(cassert-eq "reverse has B->A" (digraph-has-edge? r "B" "A") true)
(cassert-eq "reverse not A->B" (digraph-has-edge? r "A" "B") false)
(cassert-eq "reverse preserves weight (A->B == B->A)"
            (digraph-edge-weight r "B" "A") 3)
(cassert-eq "original unaffected by reverse"
            (digraph-has-edge? g "A" "B") true)

(print "\n* remove edge & remove node")

(def g2 (make-digraph))
(add-edge g2 "X" "Y" 1)
(add-edge g2 "Y" "Z" 1)
(add-edge g2 "Z" "X" 1)
(cassert-eq "digraph: 3-cycle detected"
            (has-cycle? g2) true)
(remove-edge g2 "X" "Y")
(cassert-eq "digraph remove-edge: edge gone"
            (digraph-has-edge? g2 "X" "Y") false)
(cassert-eq "digraph remove-edge: edge count decremented"
            (digraph-edge-count g2) 2)
(cassert-eq "digraph: path acyclic after remove-edge"
            (has-cycle? g2) false)
(remove-node g2 "Y")
(cassert-eq "digraph remove-node: node count decremented"
            (digraph-node-count g2) 2)
(cassert-eq "digraph remove-node: incident edges gone"
            (digraph-edge-count g2) 1)
(cassert-eq "digraph remove-node: no edge from removed node"
            (digraph-has-edge? g2 "Y" "Z") false)

(def g3 (make-digraph))
(add-node g3 "solo")
(remove-node g3 "solo")
(cassert-eq "digraph remove-node isolated: node gone"
            (digraph-node-count g3) 0)
(remove-edge g3 "solo" "solo")
(cassert-eq "digraph remove-edge missing: no-op"
            (digraph-edge-count g3) 0)
(remove-node g3 "missing")
(cassert-eq "digraph remove-node missing: no-op"
            (digraph-node-count g3) 0)


(print "\n* digraph node-data")

(def dnd (make-digraph))
(add-node dnd "P")
(add-node dnd "Q")
(cassert-eq "digraph default node data is node itself"
            (digraph-node-data dnd "P") "P")
(cassert-eq "digraph default node data Q"
            (digraph-node-data dnd "Q") "Q")

(set-node-data dnd "P" {:kind "start"})
(cassert-eq "digraph set node data"
            (digraph-node-data dnd "P") {:kind "start"})
(cassert-eq "digraph unset node data unchanged"
            (digraph-node-data dnd "Q") "Q")
(set-node-data dnd "R" "extra")
(cassert-eq "digraph add node via set-node-data"
            (digraph-node-data dnd "R") "extra")
(cassert-eq "digraph set-node-data adds node"
            (digraph-node-count dnd) 3)
(cassert-eq "digraph nodes-table has node"
            (in (digraph-nodes-table dnd) "P") {:kind "start"})
(remove-node dnd "P")
(cassert-eq "digraph remove clears data"
            (digraph-node-data dnd "P") nil)
(cassert-eq "digraph node-data for absent node is nil"
            (digraph-node-data dnd "zz") nil)

(def drem (make-digraph))
(add-nodes drem @["a" "b" "c"])
(add-edge drem "a" "b" 5)
(add-edge drem "c" "a" 7)
(add-edge drem "a" "a" 3)
(cassert-eq "digraph counts weighted arcs"
            (drem :edge-count) 3)
(remove-node drem "a")
(cassert-eq "removing a clears its out-arc, in-arc and self-loop"
            (drem :edge-count) 0)
(cassert-eq "removed node leaves the count"
            (digraph-node-count drem) 2)
(remove-node drem "b")
(remove-node drem "c")
(cassert-eq "removing the rest leaves no arcs"
            (drem :edge-count) 0)

# a plain two-node arc, weight 1
(def drem2 (make-digraph))
(add-nodes drem2 "a" "b")
(add-edge drem2 "a" "b" 1)
(remove-node drem2 "a")
(cassert-eq "single arc removed exactly"
            (drem2 :edge-count) 0)

(print-results)
