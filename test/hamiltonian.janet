# -*- mode: janet; -*-
# graphs/test/hamiltonian: Hamiltonian cycles & paths, conditions

(import ./helper :prefix "")
(import ../graphs :prefix "")

# J_9 and J_7  (flower snarks) cubic, 3n cycles and 3n vertices, non-Hamiltonian
(defn flower [n]
  (def g (make-graph))
  (each i (range n)
    (add-edge g (string "A" i) (string "B" i))
    (add-edge g (string "A" i) (string "C" i))
    (add-edge g (string "A" i) (string "D" i)))
  (each i (range n)
    (add-edge g (string "B" i) (string "B" (mod (+ i 1) n))))
  (each i (range (dec n))
    (add-edge g (string "C" i) (string "C" (+ i 1)))
    (add-edge g (string "D" i) (string "D" (+ i 1))))
  (add-edge g (string "C" (dec n)) (string "D" 0))
  (add-edge g (string "D" (dec n)) (string "C" 0))
  g)

(def flower-j7 (flower 7))
(def flower-j9 (flower 9))

(defn make-cyc [names]
  (def g (make-graph))
  (each n names (add-node g n))
  (for i 0 (length names)
    (add-edge g (names i) (names (mod (+ i 1) (length names)))))
  g)

(print "\n* Hamiltonian cycle")

# C4 has a Hamiltonian cycle
(def c4 (make-cyc @["a" "b" "c" "d"]))
(def cyc (hamiltonian-cycle c4))
(cassert-eq "C4 has Hamiltonian cycle"
            (not (nil? cyc)) true)
(cassert-eq "C4 cycle length = 4"
            (length cyc) 4)

# verify consecutive closure
(def closed?
  (and (has-edge? c4 (cyc 0) (cyc 3))
       (all (fn [k] (has-edge? c4 (cyc k) (cyc (mod (+ k 1) (length cyc)))))
            (range 0 (length cyc)))))
(cassert-eq "C4 cycle edges valid" closed? true)

# Star K_{1,4} has no Hamiltonian cycle
(def star (make-graph))
(add-nodes star @["c" "l1" "l2" "l3" "l4"])
(each l @["l1" "l2" "l3" "l4"] (add-edge star "c" l))
(cassert-eq "star has no Hamiltonian cycle"
            (nil? (hamiltonian-cycle star)) true)

# Triangle has a cycle
(def tri (make-cyc @[1 2 3]))
(def tcyc (hamiltonian-cycle tri))
(cassert-eq "triangle has Hamiltonian cycle" (not (nil? tcyc)) true)
(cassert-eq "triangle cycle length 3" (length tcyc) 3)

# Petersen graph has a Hamiltonian path but not a Hamiltonian cycle
(def dumbbell-h (make-graph))
(add-nodes dumbbell-h "a" "b" "c" "x" "y" "z")
(add-edges dumbbell-h [["a" "b"] ["b" "c"] ["c" "a"]
                       ["x" "y"] ["y" "z"] ["z" "x"]])

# no edge between the two triangles -> no cycle visiting all 6
(cassert-eq "two glued triangles no Hamiltonian cycle"
            (nil? (hamiltonian-cycle dumbbell-h)) true)

(print "\n* Hamiltonian path")

# P4 has a Hamiltonian path
(def p (make-graph))
(each n @[1 2 3] (add-node p n))
(add-edge p 1 2) (add-edge p 2 3)
(def hp (hamiltonian-path p))
(cassert-eq "P3 has Hamiltonian path" (not (nil? hp)) true)
(cassert-eq "P3 path length 3" (length hp) 3)

# disconnected graph has no Hamiltonian path
(def two-k2 (make-graph))
(add-nodes two-k2 1 2 3 4)
(add-edges two-k2 [1 2] [3 4])
(cassert-eq "two disjoint edges no Hamiltonian path"
            (nil? (hamiltonian-path two-k2)) true)

(print "\n* hamiltonian: necessary conditions")

# K4 complete graph
(def k4 (make-graph))
(add-nodes k4 1 2 3 4)
(add-edges k4 [1 2] [1 3] [1 4] [2 3] [2 4] [3 4])

# cycles
(def c6 (make-cyc @[1 2 3 4 5 6]))
(def c5 (make-cyc @[1 2 3 4 5]))

# K5 complete graph
(def k5 (make-graph))
(add-nodes k5 1 2 3 4 5)
(for i 0 5 (for j (+ i 1) 5 (add-edge k5 (+ i 1) (+ j 1))))

# star K_{1,4}: min degree 1 -> no possible cycle
(cassert-eq "star fails cycle necessary (min degree)"
            (hamiltonian-possible? star) false)

# disconnected graph -> no Hamiltonian cycle
(cassert-eq "two glued triangles fail cycle necessary (disconnected)"
            (hamiltonian-possible? dumbbell-h) false)
(cassert-eq "two disjoint edges: cycle necessary false (disconnected)"
            (hamiltonian-possible? two-k2) false)

# C4 passes the necessary gate even though Dirac's bound is too weak for cycles
(cassert-eq "C4 passes cycle necessary" (hamiltonian-possible? c4) true)

# P3 passes; two disjoint edges fail (disconnected); a single vertex is a path
(cassert-eq "P3 passes path necessary"
            (hamiltonian-possible? p :path) true)
(cassert-eq "two disjoint edges path necessary false (disconnected)"
            (hamiltonian-possible? two-k2 :path) false)

(def iso (make-graph))
(add-node iso 1)
(cassert-eq "isolated vertex path necessary true"
            (hamiltonian-possible? iso :path) true)

(print "\n* hamiltonian: sufficient conditions (Dirac)")

# K4 satisfies Dirac (min degree 3 >= 2)
(cassert-eq "K4 satisfies Dirac"
            (hamiltonian-dirac-sufficient? k4) true)
(cassert-eq "K4 passes cycle necessary"
            (hamiltonian-possible? k4) true)

# C6 satisfies dirac. min degree 2, n=6, n/2=3 -> 2 < 3 => false (bound too weak)
(cassert-eq "C6 Dirac bound false (2<3)"
            (hamiltonian-dirac-sufficient? c6) false)

# yet C6 has a Hamiltonian cycle
(cassert-eq "C6 has Hamiltonian cycle despite Dirac false"
            (not (nil? (hamiltonian-cycle c6))) true)

# C5 (n=5, min degree 2 < ceil(5/2)=3) -> false
(cassert-eq "C5 Dirac bound false (2<3)"
            (hamiltonian-dirac-sufficient? c5) false)

# K5 (min degree 4 >= ceil(5/2)=3)
(cassert-eq "K5 satisfies Dirac"
            (hamiltonian-dirac-sufficient? k5) true)


(print "\n* hamiltonian: results are vertices")

(defn visits-every-vertex-once? [g p]
  (def vs (nodes g))
  (var seen @{})
  (var distinct 0)
  (each v p (unless (get seen v)
              (put seen v true) (set distinct (+ distinct 1))))
  (and (= (length p) (length vs))
       (= distinct (length vs))
       (all (fn [v] (get seen v)) vs)))

(defn walks-edges? [g p cycle?]
  (all (fn [k] (has-edge? g (p k) (p (mod (+ k 1) (length p)))))
       (range (if cycle? 0 (max 0 (- (length p) 1))))))

(defn all-vertices-are [g p]
  "True when every element of `p` is one of the graph's own vertices."
  (def vs @{})
  (each v (nodes g) (put vs v true))
  (all (fn [v] (get vs v)) p))

(def k4-cycle (hamiltonian-cycle k4))
(cassert-eq "cycle: visits every vertex exactly once"
            (visits-every-vertex-once? k4 k4-cycle) true)
(cassert-eq "cycle: consecutive pairs are edges"
            (walks-edges? k4 k4-cycle true) true)
(cassert-eq "cycle: every element is one of the graph's vertices"
            (all-vertices-are k4 k4-cycle) true)

(def k4-path (hamiltonian-path k4))
(cassert-eq "path: visits every vertex exactly once"
            (visits-every-vertex-once? k4 k4-path) true)
(cassert-eq "path: consecutive pairs are edges"
            (walks-edges? k4 k4-path false) true)
(cassert-eq "path: every element is one of the graph's vertices"
            (all-vertices-are k4 k4-path) true)

(def c4-cycle (hamiltonian-cycle c4))
(cassert-eq "cycle: the string cycle is a real cycle"
            (and (walks-edges? c4 c4-cycle true)
                 (visits-every-vertex-once? c4 c4-cycle))
            true)

(def c4-path (hamiltonian-path c4))
(cassert-eq "path: the string path is a real path"
            (and (walks-edges? c4 c4-path false)
                 (visits-every-vertex-once? c4 c4-path))
            true)

(def grid5 (make-graph))
(for y 0 5
  (for x 0 5
    (def v (+ x (* 5 y)))
    (when (< (+ x 1) 5) (add-edge grid5 v (+ v 1)))
    (when (< (+ y 1) 5) (add-edge grid5 v (+ v 5)))))

(def grid-path (hamiltonian-path grid5))
(cassert-eq "path: 5x5 grid visits all 25 once"
            (visits-every-vertex-once? grid5 grid-path) true)
(cassert-eq "path: 5x5 grid path walks real edges"
            (walks-edges? grid5 grid-path false) true)
(cassert-eq "cycle: a 5x5 grid has no Hamiltonian cycle (25 vertices)"
            (nil? (hamiltonian-cycle grid5)) true)


(print "\n* hamiltonian: using node-limit")

(def limited
  (try (hamiltonian-cycle flower-j9 :node-limit 1000) ([e] e)))

(cassert-eq "node-limit: a search that gives up raises error rather than returning nil"
            (not= nil limited) true)
(cassert-eq "node-limit: the error shows that the limit was hit"
            (not= nil (string/find ":node-limit" limited)) true)
(cassert-eq "node-limit: the error says how much was expanded"
            (not= nil (string/find "expanding" limited)) true)
(cassert-eq "node-limit: the error says how to get an answer anyway"
            (not= nil (string/find "raise :node-limit" limited)) true)

(cassert-eq "node-limit: unset means no ceiling"
            (try (do (hamiltonian-cycle flower-j7 :node-limit 1000000000) true)
                 ([_] false)) true)

(cassert-eq "node-limit: a generous limit gives the same answer as none"
            (= (hamiltonian-cycle flower-j7 :node-limit 1000000000)
               (hamiltonian-cycle flower-j7)) true)
(cassert-eq "node-limit: and a generous limit still finds a valid answer"
            (walks-edges? k4 (hamiltonian-cycle k4 :node-limit 1000000000) true) true)

(cassert-eq "node-limit: same for the path search"
            (not= nil
                  (try (do (hamiltonian-path flower-j9 :node-limit 5) nil)
                       ([e] e))) true)


(print "\n* hamiltonian: at most two ends")

(def three-ends (make-graph))
(add-edge three-ends "c1" "c2")
(add-edge three-ends "c1" "c2")   # duplicate. keeps the graph simple

(def three-leaf (make-graph))
(add-node three-leaf "hub")
(each l ["a" "b" "c"] (add-edge three-leaf "hub" l))

(cassert-eq "path: three degree-1 vertices fail the necessary check"
            (hamiltonian-possible? three-leaf :path) false)
(cassert-eq "path: and so report no path"
            (nil? (hamiltonian-path three-leaf)) true)

(def two-leaf (make-graph))
(add-edges two-leaf  [["a" "b"] ["b" "c"] ["c" "d"]])

(cassert-eq "path: two degree-1 vertices pass the necessary check"
            (hamiltonian-possible? two-leaf :path) true)
(cassert-eq "path: a path graph is traced end to end"
            (visits-every-vertex-once? two-leaf
                                       (hamiltonian-path two-leaf)) true)

# one degree-1 vertex is possible
(def one-leaf (make-graph))
(add-edges one-leaf ["a" "b"] ["b" "c"] ["c" "a"] ["a" "d"])

(cassert-eq "path: one degree-1 vertex passes the necessary check"
            (hamiltonian-possible? one-leaf :path) true)
(cassert-eq "path: a triangle with a pendant vertex is traced"
            (visits-every-vertex-once? one-leaf
                                       (hamiltonian-path one-leaf)) true)


(print "\n* hamiltonian: self-loops")

(def loop-g (make-graph))
(add-edges loop-g ["a" "b"] ["b" "c"] ["c" "a"] ["a" "a"])

(def loop-cycle (hamiltonian-cycle loop-g))
(cassert-eq "cycle: a self-loop does not become a step onto itself"
            (and loop-cycle (walks-edges? loop-g loop-cycle true)) true)
(cassert-eq "cycle: and the self-loop vertex is still visited once"
            (visits-every-vertex-once? loop-g loop-cycle) true)


(print "\n* hamiltonian: cut vertices")

(def cut-dumbbell (make-graph))
(each n @["a" "b" "c" "x" "y" "z"] (add-node cut-dumbbell n))
(each e [["a" "b"] ["b" "c"] ["c" "a"] ["x" "y"] ["y" "z"] ["z" "x"]]
  (add-edge cut-dumbbell (e 0) (e 1)))
(add-edge cut-dumbbell "c" "x")

(cassert-eq "cycle: a graph with a cut vertex still has none"
            (nil? (hamiltonian-cycle cut-dumbbell)) true)
(cassert-eq "cycle: and it says so at once, with no search"
            (nil? (hamiltonian-cycle cut-dumbbell :node-limit 1)) true)

# a cycle with a triangle attached to one of its vertices
(def cut-pendant (make-graph))
(for i 0 8 (add-node cut-pendant i))
(for i 0 8 (add-edge cut-pendant i (mod (+ i 1) 8)))
(add-nodes cut-pendant "p" "q")
(add-edges cut-pendant [0 "p"] ["p" "q"] ["q" 0])

(cassert-eq "cycle: a pendant triangle rules the cycle out"
            (nil? (hamiltonian-cycle cut-pendant)) true)

# Two triangles sharing an edge. 2-connected, with  0-1-3-2-0 cycle.
(def diamond (make-graph))
(add-nodes diamond 0 1 2 3)
(each e [[0 1] [1 2] [2 0] [1 3] [3 2]] (add-edge diamond (e 0) (e 1)))
(cassert-eq "cycle: two triangles sharing an edge have a cycle"
            (not (nil? (hamiltonian-cycle diamond))) true)
(cassert-eq "cycle: and the cycle is a real one"
            (walks-edges? diamond (hamiltonian-cycle diamond) true) true)

(print-results)
