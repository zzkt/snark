# -*- mode: janet; -*-
# graphs/cycles: cycle detection and cycle finding

(import deft :prefix "")
(import ./types :prefix "")
(import ./simple-graph :prefix "")
(import ./multigraph :prefix "")

(deftfn- undirected-cycle-present?
  ```Shared DFS for undirected cycle detection (a self-loop is counted as a cycle).

Returns true when the adjacency `adj` contains a cycle.
```
  [nodes :array adj :function] :boolean
   (var found false)
  (let [visited @{}
        parent @{}]
    (defn dfs [node]
      (put visited node true)
      (each neighbour (adj node)
        (when (not= neighbour (in parent node))
          (cond
            (in visited neighbour) (set found true)
            :else (do (put parent neighbour node) (dfs neighbour))))
        (when found (break))))
    (each node nodes
      (when (and (not found) (not (in visited node)))
        (put parent node nil)
        (dfs node))))
  found)


(define graph-has-cycle?
  "True when the graph contains a cycle. Uses DFS back-edge detection."
  [g :graph] :boolean
   (undirected-cycle-present?
    (graph-nodes g) (fn [n] (graph-neighbours g n))))


(define graph-cycle-basis
  ```Find a cycle basis (fundamental cycles) using DFS.

A self-loop counts as a length-1 fundamental cycle.

Returns array of cycles, where each cycle is an array of nodes.
```
  [g :graph] :array
   (let [visited @{}
         parent @{}
         cycles @[]]
     (defn dfs [node]
       (put visited node true)
       (each neighbour (graph-neighbours g node)
         (cond
           (= neighbour node)
           (array/push cycles @[node])
           (and (in visited neighbour)
                (not= neighbour (in parent node)))
           (let [cycle @[node]]
             (var current (in parent node))
             (var found (= current neighbour))
             (while (and current (not found))
               (array/push cycle current)
               (if (= current neighbour)
                 (set found true)
                 (set current (in parent current))))
             (when found
               (array/push cycles cycle)))
           (not (in visited neighbour))
           (do (put parent neighbour node) (dfs neighbour)))))
     (each node (graph-nodes g)
       (when (not (in visited node))
         (put parent node nil)
         (dfs node)))
     cycles))


(define graph-k-cycle-count
  ```Count the simple cycles of length `k` in a simple undirected graph `g`.

A simple k-cycle is counted once, independent of orientation or starting node.
Chords do not exclude a cycle, the subgraph may contain edges beyond the cycle
itself (e.g. all three 4-cycles of K4 are counted).

The search fixes the smallest-indexed node of each cycle as its start and
enumerates simple paths of length k rather than all C(n,k) subsets.

Exponential in k (worst case around O(C(n,k) * k!)) use small k and moderate n.

Returns 0 if k is below 3 or exceeds the node count.
```
  [g :graph k :number] :number
   (def verts (sort (graph-nodes g)))
   (def n (length verts))
   (if (or (< k 3) (> k n)) 0
     (let [index @{}]
       (each i (range n) (put index (verts i) i))
       (var cycount 0)
       (defn walk [start path in-path depth]
         (def v (path (- depth 1)))
         (each w (graph-neighbours g v)
           (def wi (index w))
           (cond
             (= wi start)
             (when (and (= depth k)
                        (< (index (path 1)) (index (path (- depth 1)))))
               (set cycount (+ cycount 1)))
             (and (> wi start)
                  (not (in in-path wi))
                  (< depth k))
             (do
               (put in-path wi true)
               (array/push path w)
               (walk start path in-path (inc depth))
               (array/pop path)
               (put in-path wi false)))))
       (for si 0 (inc (- n k))
         (def in-path @{})
         (put in-path si true)
         (walk si @[(verts si)] in-path 1))
       cycount)))


(deftfn- directed-cycle-present?
  ```Returns true when the directed adjacency `out` (node -> {neighbour -> w})
contains a directed cycle. Uses WHITE/GRAY/BLACK DFS colouring.```
  [nodes :array out :table] :boolean
   (var found false)
  (let [color @{}]
    (each node nodes
      (put color node :white))
    (defn dfs [node]
      (put color node :gray)
      (each neighbour (keys (get out node))
        (cond
          (= (in color neighbour) :gray) (set found true)
          (= (in color neighbour) :white) (dfs neighbour))
        (when found (break)))
      (put color node :black))
    (each node nodes
      (when (and (not found) (= (in color node) :white))
        (dfs node))))
  found)


(define directed-has-cycle?
 "True when a directed graph contains a directed cycle."
  [g :directed] :boolean
   (directed-cycle-present? (g :nodes)
     (typecase-strict g
       :multidigraph (g :out)
       :digraph (g :adj))))

(define weighted-graph-has-cycle?
  "True when an undirected weighted graph contains a cycle."
  [g :weighted-graph] :boolean
   (undirected-cycle-present? (g :nodes) (fn [n] (keys (get (g :adj) n)))))

(deftfn- lex-less?
  ```True when array `a` sorts strictly before `b` (element-wise, strings).```
  [a :array b :array] :boolean
   (var out nil)
  (for i 0 (min (length a) (length b))
    (when (nil? out)
      (def c (compare (a i) (b i)))
      (when (not= c 0) (set out (< c 0)))))
  (if (nil? out) (< (length a) (length b)) out))


(deftfn- rotate-min
  "Lexicographically-minimal rotation of a cycle (canonical form)."
  [c :array] :array
   (var best c)
  (def n (length c))
  (for i 1 n
    (def r (array/slice c i n))
    (array/concat r (array/slice c 0 i))
    (when (lex-less? r best) (set best r)))
  best)


(deftfn- path-key
  "Injection-safe canonical key for a node sequence `path`."
  [path :array]
   (var acc true)
  (each n (reverse path) (set acc (struct n acc)))
  acc)


(deftfn- johnson-unblock
  "Unblock `u` during Johnson's enumeration (mutates blocked/b-links)."
  [u :dynamic blocked :table b-links :table]
   (put blocked u false)
  (each v (in b-links u)
    (when (in blocked v)
      (put b-links v (filter (fn [x] (not= x v)) (in b-links v)))
      (johnson-unblock v blocked b-links)))
  (put b-links u @[]))


(deftfn- johnson-circuit
  ```Core circuit search step of Johnson's algorithm (mutates stack/cycles).```
  [g :multidigraph v :dynamic start :dynamic blocked
   :table b-links :table stack :array cycles :array]
  (var found false)
  (array/push stack v)
  (put blocked v true)
  (each w (multidigraph-neighbours g v)
    (if (= w start)
      (do (array/push cycles (array/slice stack))
          (set found true))
      (when (not (in blocked w))
        (def reached (johnson-circuit g w start blocked b-links stack cycles))
        (when reached (set found true)))))
  (when (not found)
    (each w (multidigraph-neighbours g v)
      (array/push (in b-links w) v)))
  (array/pop stack)
  (when found (johnson-unblock v blocked b-links)))


(define multidigraph-simple-cycles
  ```Find all simple cycles using simplified Johnson's algorithm.

Returns an array of cycles (where each cycle is an array of nodes)
```
  [g :multidigraph] :array
   (let [stack @[]
         blocked @{}
         b-links @{}
         cycles @[]]
    (each node (multidigraph-nodes g)
      (put blocked node false)
      (put b-links node @[]))
    (each s (multidigraph-nodes g)
      (each node (multidigraph-nodes g)
        (put blocked node false)
        (put b-links node @[]))
      (johnson-circuit g s s blocked b-links stack cycles))
    (def seen @{})
    (def result @[])
    (each c cycles
      (def ll (rotate-min c))
      (def key (path-key ll))
      (unless (get seen key)
        (put seen key true)
        (array/push result ll)))
    result))
