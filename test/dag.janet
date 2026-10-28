# -*- mode: janet; -*-
# graphs/test/dag: acyclicity, topological generations, ancestors/descendants,
#                  transitive closure/reduction, longest paths, antichains, etc

(import ./helper :prefix "")
(import ../graphs :prefix "")
(import ./data/order :prefix "")

(defn edge-arr [red]
  (map (fn [e] (string (e 0) ">" (e 1))) red))

# Diamond DAG: a->b->c, a->d->c, b->d
(def g (make-digraph @[] @{}))
(add-nodes g @["a" "b" "c" "d"])
(add-edge g "a" "b" 1)
(add-edge g "b" "c" 1)
(add-edge g "a" "d" 1)
(add-edge g "d" "c" 1)
(add-edge g "b" "d" 1)

(print "\n* acyclicity & periodicity")

(cassert-eq "diamond acyclic" (is-acyclic? g) true)
(cassert-eq "diamond aperiodic" (is-aperiodic? g) true)

(def cyc (make-digraph))
(add-nodes cyc @["a" "b" "c"])
(add-edge cyc "a" "b" 1)
(add-edge cyc "b" "c" 1)
(add-edge cyc "c" "a" 1)
(cassert-eq "cycle not acyclic"
            (is-acyclic? cyc) false)
(cassert-eq "cycle not aperiodic"
            (is-aperiodic? cyc) false)

(def selflop (make-digraph))
(digraph-add-node selflop "x")
(add-edge selflop "x" "x" 1)
(cassert-eq "self-loop not acyclic"
            (is-acyclic? selflop) false)
(cassert-eq "self-loop aperiodic"
            (is-aperiodic? selflop) true)

(def pathg (make-digraph))
(add-nodes pathg @["p" "q" "r"])
(add-edge pathg "p" "q" 1)
(add-edge pathg "q" "r" 1)
(cassert-eq "path DAG acyclic"
            (is-acyclic? pathg) true)
(cassert-eq "path DAG not aperiodic"
            (is-aperiodic? pathg) false)

(def empty (make-digraph))
(cassert-eq "empty acyclic"
            (is-acyclic? empty) true)
(cassert-eq "empty aperiodic"
            (is-aperiodic? empty) true)

(print "\n* topological generations")

(def gens (topological-generations g))
(cassert-eq "gen count" (length gens) 4)
(cassert-arr-eq "gen 0 == [a]" (gens 0) @["a"])

(cassert-eq "gen 1 == [b]" (set-eq (gens 1) @["b"]) true)
(cassert-eq "gen 2 == [d]" (set-eq (gens 2) @["d"]) true)
(cassert-arr-eq "gen 3 == [c]" (gens 3) @["c"])
(cassert-eq "cyclic generations nil"
            (topological-generations cyc) nil)

(print "\n* ancestors & descendants")

(cassert-set-eq "ancestors c" (ancestors g "c") @["a" "b" "d"])
(cassert-set-eq "ancestors root" (ancestors g "a") @[])
(cassert-set-eq "descendants a" (descendants g "a") @["b" "c" "d"])
(cassert-set-eq "descendants sink" (descendants g "c") @[])
(cassert-set-eq "descendants b" (descendants g "b") @["c" "d"])

(print "\n* transitive closure & reduction")

(def tc (transitive-closure g))
(cassert-eq "closure a reaches all" (length (keys (get tc "a"))) 4)
(cassert-eq "closure c reaches self only" (length (keys (get tc "c"))) 1)
(cassert-eq "closure b reaches d" (get-in tc ["b" "d"]) true)
(cassert-eq "closure a reaches self" (get-in tc ["a" "a"]) true)

(def red (transitive-reduction g))
(cassert-eq "reduction edge count" (length red) 3)
(cassert-eq "reduction keeps a>b" (arr-contains (edge-arr red) "a>b") true)
(cassert-eq "reduction keeps b>d" (arr-contains (edge-arr red) "b>d") true)
(cassert-eq "reduction keeps d>c" (arr-contains (edge-arr red) "d>c") true)
(cassert-eq "reduction drops a>d" (arr-contains (edge-arr red) "a>d") false)
(cassert-eq "reduction drops b>c" (arr-contains (edge-arr red) "b>c") false)

(print "\n* longest path")

(cassert-arr-eq "diamond longest path" (dag-longest-path g) @["a" "b" "d" "c"])
(cassert-eq "diamond longest length" (dag-longest-path-length g) 3)
(cassert-eq "cyclic longest path nil" (dag-longest-path cyc) nil)
(cassert-eq "cyclic longest length -1" (dag-longest-path-length cyc) -1)
(cassert-arr-eq "empty longest path" (dag-longest-path empty) @[])
(cassert-eq "empty longest length" (dag-longest-path-length empty) 0)

(print "\n* antichains")

# diamond
(cassert-eq "diamond antichain count" (length (antichains g)) 5)

# two independent chains
(def chains (make-digraph))
(add-nodes chains @["a" "b" "c" "d"])
(add-edge chains "a" "b" 1)
(add-edge chains "c" "d" 1)
(cassert-eq "chains antichain count"
            (length (antichains chains)) 9)

# star
(def star (make-digraph))
(add-nodes star @["r" "x" "y"])
(add-edge star "r" "x" 1)
(add-edge star "r" "y" 1)
(cassert-eq "star antichain count"
            (length (antichains star)) 5)

(def star-antis (antichains star))
(cassert-eq "star has {x,y} pair"
            (not= nil (find (fn [a] (set-eq a @["x" "y"])) star-antis)) true)

(print "\n* colliders & v-structures")

(cassert-set-eq "colliders of diamond" (colliders g) @["c" "d"])
(cassert-eq "v-structures of diamond" (length (v-structures g)) 0)

# v-structure: a->b, c->b, no a-c edge
(def v (make-digraph))
(add-nodes v @["a" "b" "c"])
(add-edge v "a" "b" 1)
(add-edge v "c" "b" 1)
(cassert-set-eq "colliders of vstruct"
                (colliders v) @["b"])
(cassert-eq "v-structures found"
            (length (v-structures v)) 1)
(cassert-eq "empty colliders"
            (length (colliders empty)) 0)
(cassert-eq "empty v-structures"
            (length (v-structures empty)) 0)


(print-results)
