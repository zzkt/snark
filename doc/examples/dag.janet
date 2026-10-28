# Examples


(import graphs :prefix "")



# A diamond with a shortcut. =a → b → c = and =a → d → c=


(def g (make-digraph))
(add-nodes g ["a" "b" "c" "d"])
(add-edges g [["a" "b" 1] ["b" "c" 1] ["a" "d" 1] ["d" "c" 1]])

(printf "acyclic? %p" (is-acyclic? g))

(printf "ancestors(c) = %s"
        (string/join (sort (map string (ancestors g "c"))) ","))

(printf "longest path: %s" (string/join (dag-longest-path g) " -> "))

(printf "generations: %p" (length (topological-generations g)))



# #+RESULTS:
# : acyclic? true
# : ancestors(c) = a,b,d
# : longest path: a -> b -> c
# : generations: 3

# Transitive closure for reachability


(def h (make-digraph))
(add-nodes h ["a" "b" "c"])
(add-edges h [["a" "b" 1] ["b" "c" 1]])

(def tc (transitive-closure h))
(printf "node 'a' reaches %p nodes (incl. itself): %s"
        (length (keys (get tc "a")))
        (string/join (sort (map string (keys (get tc "a")))) ", "))



# #+RESULTS:
# : node 'a' reaches 3 nodes (incl. itself): a, b, c

# Transitive reduction removes redundant edges while preserving reachability


(def h2 (make-digraph))
(add-nodes h2 ["a" "b" "c" "d"])
(add-edges h2 [["a" "b" 1] ["b" "c" 1] ["a" "c" 1] ["c" "d" 1]])

(def tr (transitive-reduction h2))
(printf "transitive reduction: %s"
        (string/join (map (fn [e] (string (e 0) "->" (e 1))) tr) ", "))



# #+RESULTS:
# : transitive reduction: a->b, b->c, c->d

# Descendants of a node (all reachable nodes in the forward direction, excluding the source)


(def g2 (make-digraph))
(add-nodes g2 ["a" "b" "c" "d" "e"])
(add-edges g2 [["a" "b" 1] ["a" "c" 1] ["b" "d" 1] ["c" "d" 1] ["d" "e" 1]])

(printf "descendants(a) = %s"
        (string/join (sort (map string (descendants g2 "a"))) ", "))



# #+RESULTS:
# : descendants(a) = b, c, d, e

# DAG longest path length (number of edges on the critical path). Multiple longest paths may exist but only one is returned.


(def g3 (make-digraph))
(add-nodes g3 ["s" "a" "b" "c" "t"])
(add-edges g3 [["s" "a" 1] ["s" "b" 1] ["a" "c" 1] ["b" "c" 1] ["c" "t" 1]])

(printf "longest path length: %p" (dag-longest-path-length g3))
(printf "longest path: %s" (string/join (dag-longest-path g3) "->"))



# #+RESULTS:
# : longest path length: 3
# : longest path: s->b->c->t

# Antichains on a diamond DAG (maximal sets of pairwise incomparable nodes)


(def diamond (make-digraph))
(add-nodes diamond "s" "a" "b" "t")
(add-edges diamond ["s" "a" 1] ["s" "b" 1] ["a" "t" 1] ["b" "t" 1])

(def ac (antichains diamond))
(printf "antichains (%p): %s"
        (length ac)
        (string/join
         (map (fn [a] (string "{" (string/join (map string a) ",") "}")) ac) " "))



# #+RESULTS:
# : antichains (6): {} {s} {a} {b} {t} {a,b}

# Source and sink nodes in a DAG


(def g4 (make-digraph))
(add-nodes g4 "s" "a" "b" "t")
(add-edges g4 ["s" "a" 1] ["s" "b" 1] ["a" "t" 1] ["b" "t" 1])

(printf "sources: %s" (string/join (source-nodes g4) ", "))
(printf "sinks:   %s" (string/join (sink-nodes g4) ", "))



# #+RESULTS:
# : sources: s
# : sinks:   t

# Feedback arc set on a cyclic digraph (edges whose reversal breaks all cycles)


(def cyclic (make-digraph))
(add-nodes cyclic "a" "b" "c")
(add-edges cyclic ["a" "b" 1] ["b" "c" 1] ["c" "a" 1])

(def fas (feedback-arc-set cyclic))
(printf "feedback arc set: %s"
        (string/join (map (fn [e] (string (e 0) "->" (e 1))) fas) " "))



# #+RESULTS:
# : feedback arc set: c->a

# Aperiodicity test on a directed cycle (periodic) vs. a graph with a chord (aperiodic)


(def c3 (make-digraph))
(add-nodes c3 "a" "b" "c")
(add-edges c3 ["a" "b" 1] ["b" "c" 1] ["c" "a" 1])

(def c3chord (make-digraph))
(add-nodes c3chord "a" "b" "c")
(add-edges c3chord ["a" "b" 1] ["b" "c" 1] ["c" "a" 1] ["a" "c" 1])

(printf "C3 is-aperiodic? %p" (is-aperiodic? c3))
(printf "C3+chord is-aperiodic? %p" (is-aperiodic? c3chord))



# #+RESULTS:
# : C3 is-aperiodic? false
# : C3+chord is-aperiodic? true

# Colliders and v-structures in a causal DAG


(def causal (make-digraph))
(add-nodes causal "x" "y" "z" "w")
(add-edges causal ["x" "z" 1] ["y" "z" 1] ["z" "w" 1])

(printf "colliders: %s" (string/join (colliders causal) ", "))

(printf "v-structures: %s"
        (string/join (map (fn [vs] (string (vs 0) "->" (vs 1) "<-" (vs 2)))
                          (v-structures causal)) " "))
