(import graphs :prefix "")

(def g (make-digraph))
(add-nodes g ["a" "b" "c" "d"])
(add-edges g [["a" "b" 1] ["b" "c" 1] ["a" "d" 1] ["d" "c" 1]])

(printf "acyclic? %p" (is-acyclic? g))

(printf "ancestors(c) = %s"
        (string/join (sort (map string (ancestors g "c"))) ","))

(printf "longest path: %s" (string/join (dag-longest-path g) " -> "))

(printf "generations: %p" (length (topological-generations g)))

(def h (make-digraph))
(add-nodes h ["a" "b" "c"])
(add-edges h [["a" "b" 1] ["b" "c" 1]])

(def tc (transitive-closure h))
(printf "node 'a' reaches %p nodes (incl. itself): %s"
        (length (keys (get tc "a")))
        (string/join (sort (map string (keys (get tc "a")))) ", "))

(def h2 (make-digraph))
(add-nodes h2 ["a" "b" "c" "d"])
(add-edges h2 [["a" "b" 1] ["b" "c" 1] ["a" "c" 1] ["c" "d" 1]])

(def tr (transitive-reduction h2))
(printf "transitive reduction: %s"
        (string/join (map (fn [e] (string (e 0) "->" (e 1))) tr) ", "))

(def g2 (make-digraph))
(add-nodes g2 ["a" "b" "c" "d" "e"])
(add-edges g2 [["a" "b" 1] ["a" "c" 1] ["b" "d" 1] ["c" "d" 1] ["d" "e" 1]])

(printf "descendants(a) = %s"
        (string/join (sort (map string (descendants g2 "a"))) ", "))

(def g3 (make-digraph))
(add-nodes g3 ["s" "a" "b" "c" "t"])
(add-edges g3 [["s" "a" 1] ["s" "b" 1] ["a" "c" 1] ["b" "c" 1] ["c" "t" 1]])

(printf "longest path length: %p" (dag-longest-path-length g3))
(printf "longest path: %s" (string/join (dag-longest-path g3) "->"))

(def diamond (make-digraph))
(add-nodes diamond ["s" "a" "b" "t"])
(add-edges diamond [["s" "a" 1] ["s" "b" 1] ["a" "t" 1] ["b" "t" 1]])

(def ac (antichains diamond))
(printf "antichains (%p): %s"
        (length ac)
        (string/join
         (map (fn [a] (string "{" (string/join (map string a) ",") "}")) ac) " "))

(def g4 (make-digraph))
(add-nodes g4 ["s" "a" "b" "t"])
(add-edges g4 [["s" "a" 1] ["s" "b" 1] ["a" "t" 1] ["b" "t" 1]])

(printf "sources: %s" (string/join (source-nodes g4) ", "))
(printf "sinks:   %s" (string/join (sink-nodes g4) ", "))

(def cyclic (make-digraph))
(add-nodes cyclic ["a" "b" "c"])
(add-edges cyclic [["a" "b" 1] ["b" "c" 1] ["c" "a" 1]])

(def fas (feedback-arc-set cyclic))
(printf "feedback arc set: %s"
        (string/join (map (fn [e] (string (e 0) "->" (e 1))) fas) " "))

(def c3 (make-digraph))
(add-nodes c3 ["a" "b" "c"])
(add-edges c3 [["a" "b" 1] ["b" "c" 1] ["c" "a" 1]])

(def c3chord (make-digraph))
(add-nodes c3chord ["a" "b" "c"])
(add-edges c3chord [["a" "b" 1] ["b" "c" 1] ["c" "a" 1] ["a" "c" 1]])

(printf "C3 is-aperiodic? %p" (is-aperiodic? c3))
(printf "C3+chord is-aperiodic? %p" (is-aperiodic? c3chord))

(def causal (make-digraph))
(add-nodes causal ["x" "y" "z" "w"])
(add-edges causal [["x" "z" 1] ["y" "z" 1] ["z" "w" 1]])

(printf "colliders: %s" (string/join (colliders causal) ", "))

(printf "v-structures: %s"
        (string/join (map (fn [vs] (string (vs 0) "->" (vs 1) "<-" (vs 2)))
                          (v-structures causal)) " "))
