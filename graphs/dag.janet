# -*- mode: janet; -*-
# graphs/dag: Directed acyclic graph analysis & causal inference

# Ancestors & descendants, transitive closure & reduction, longest path,
# topological generations, antichains, colliders and v-structures.

(import deft :prefix "")
(import ./types :prefix "")
(import ./traversal :prefix "")
(import ./util :prefix "")


(define succ
  "Outgoing neighbour names of `node` in the directed graph `g`."
  [g :directed node :dynamic] :array
  (neighbours g node))

(define pred
  ```Incoming neighbour names of `node` in the directed graph `g`.```
  [g :directed node :dynamic] :array
  (in-neighbours g node))

(defn- directed-neighbour-table
  "Return {node [neighbours]} for outgoing edges without per-visit allocations."
  [g]
  (def result @{})
  (each n (nodes g) (put result n @[]))
  (each [u tbl] (pairs (g :adj))
    (each [v _] (pairs tbl)
      (array/push (get result u) v)))
  result)

(defn- directed-in-neighbour-table
  "Return {node [predecessors]} for incoming edges in one pass."
  [g]
  (def result @{})
  (each n (nodes g) (put result n @[]))
  (each [u tbl] (pairs (g :adj))
    (each [v _] (pairs tbl)
      (array/push (get result v) u)))
  result)


# ==============================================================================
#  Topological generations
# ==============================================================================

(define topological-generations
 ```Partition a graph `g` into topological generations.

Generations are represented as an array of arrays, each inner array containing
nodes with no unprocessed incoming edges at each stage. The first generation
contains only source nodes.

Returns an array of arrays, or nil if the graph contains a cycle.
```
  [g :directed] :array-or-nil
  (let [nodes (nodes g)
        in-deg (in-degree-table g)
        result @[]]
    (var produced 0)
    (var frontier (seq [n :in nodes :when (= 0 (in in-deg n))] n))
    (while (> (length frontier) 0)
      (array/push result frontier)
      (+= produced (length frontier))
      (def next-gen @[])
      (each u frontier
        (each v (succ g u)
          (def nd (- (in in-deg v) (edge-multiplicity g u v)))
          (put in-deg v nd)
          (when (= 0 nd) (array/push next-gen v))))
      (set frontier next-gen))
    (when (= produced (length nodes))
      result)))


# ==============================================================================
#  Acyclicity and periodicity
# ==============================================================================

(define is-acyclic?
  ```True when the graph `g` is a directed acyclic graph. False for cyclic graphs.```
  [g :directed] :boolean
  (not (nil? (topological-sort g))))


(define is-aperiodic?
 ```True when the graph is aperiodic.
A self-loop is aperiodic. The empty graph is treated as aperiodic.```
  [g :directed] :boolean
  (if (= (length (nodes g)) 0)
    true
    (do
      (defn helper
        [remaining]
        (def s (first remaining))
        (def levels @{s 0})
        (var this-level @[s])
        (var cycle-gcd 0)
        (var lev 1)
        (while (> (length this-level) 0)
          (def next-level @[])
          (each u this-level
            (each v (succ g u)
              (if (get levels v)
                (set cycle-gcd
                  (math/gcd cycle-gcd
                    (+ (- (get levels u) (get levels v)) 1)))
                (do
                  (array/push next-level v)
                  (put levels v lev)))))
          (set this-level next-level)
          (++ lev))
        (def reached (length (keys levels)))
        (if (= reached (length remaining))
          (= cycle-gcd 1)
          (and (= cycle-gcd 1)
            (helper (filter (fn [n] (not (get levels n))) remaining)))))
      (helper (nodes g)))))


# ==============================================================================
#  Ancestors / descendants
# ==============================================================================

(defn- reachable-set
  "All nodes reachable from start using a precomputed neighbour table."
  [table start &opt include-self]
  (let [seen (if include-self @{start true} @{})
        stack @[start]]
  (var head 0)
  (while (< head (length stack))
    (def u (stack head))
    (++ head)
    (each v (get table u)
      (when (nil? (in seen v))
        (put seen v true)
        (array/push stack v))))
  seen))

(define ancestors
 ```Return the array of all ancestors of `node` in graph `g`.```
  [g :directed node :dynamic] :array
  (keys (reachable-set (directed-in-neighbour-table g) node)))

(define descendants
 ```Return the array of all descendants of `node` in graph `g`.```
  [g :directed node :dynamic] :array
  (keys (reachable-set (directed-neighbour-table g) node)))


# ==============================================================================
#  Transitive closure / reduction
# ==============================================================================

(define transitive-closure
  ```Transitive closure of a graph `g`.

Every node is reachable from itself. Only pairs joined by a directed path  u->v
are included.

Return reachability matrix {u {v true}}
```
  [g :directed] :table
  (def result @{})
  (def nodes (nodes g))
  (def next-table (directed-neighbour-table g))
  (each n nodes
    (put result n (reachable-set next-table n true)))
  result)


(define transitive-reduction
 ```Transitive reduction of a DAG `g`.

A transitive reduction is a unique minimal subgraph with the same reachability
relation. For cyclic inputs the result may include redundant edges.

Returns an array of [u v] edges.
```
  [g :directed] :array
  (def closure (transitive-closure g))
  (def nodes (nodes g))
  (def result @[])
  (each u nodes
    (each v nodes
      (when (has-edge? g u v)
        (var redundant false)
        (each b nodes
          (when (and (not= b u) (not= b v)
                     (get (in closure u) b)
                     (get (in closure b) v))
            (set redundant true)))
        (unless redundant
          (array/push result [u v])))))
  result)


# ==============================================================================
#  Longest path (DAG)
# ==============================================================================

(define dag-longest-path-length
 ```Number of edges on the longest directed path in the DAG `g`.

Returns path length, or 0 for a single-node or empty graph, -1 if cyclic.
```
  [g :directed] :integer
  (def order (topological-sort g))
  (if (nil? order)
    -1
    (do
      (def dist @{})
      (def parent @{})
      (each n order (put dist n 0))
      (var best 0)
      (each u order
        (each v (succ g u)
          (def nd (+ (in dist u) 1))
          (when (> nd (in dist v))
            (put dist v nd)
            (put parent v u))
          (when (> nd best) (set best nd))))
      best)))

(define dag-longest-path
 ```Return the longest directed path as an array of nodes in DAG `g`, or nil if cyclic.```
  [g :directed] :array-or-nil
  (def order (topological-sort g))
  (if (nil? order)
    nil
    (do
      (def dist @{})
      (def parent @{})
      (each n order (put dist n 0))
      (each u order
        (each v (succ g u)
          (def nd (+ (in dist u) 1))
          (when (> nd (in dist v))
            (put dist v nd)
            (put parent v u))))
      (def nodes (nodes g))
      (if (empty? nodes)
        @[]
        (do
          (var (best start)
               (first (sort (seq [n :in nodes] [(in dist n) n])
                            (fn [a b] (> (a 0) (b 0))))))
          (def path @[start])
          (var cur start)
          (while (get parent cur)
            (def p (get parent cur))
            (array/push path p)
            (set cur p))
          (reverse path))))))



# ==============================================================================
#  Antichains
# ==============================================================================

(defn- comparable?
  ```True when there is a directed path between `a` and `b` in either direction.```
  [reach a b]
  (if (= a b)
    false
    (or (get (in reach a) b) (get (in reach b) a))))


(define antichains
 ```Return an array of all antichains of the DAG `g`.

Antichains are subsets of nodes without a directed path between any two nodes.
Includes the empty set and all singletons.
```
  [g :directed] :array
  (def reach (transitive-closure g))
  (def nodes (nodes g))
  (def n (length nodes))
  (def result @[[]])
  # explicit-stack enumeration in ascending index order
  (def stack @[[0 @[]]])
  (while (> (length stack) 0)
    (def [start chosen] (array/pop stack))
    (for i start n
      (def v (nodes i))
      (var ok true)
      (each j chosen
        (when (comparable? reach v (nodes j))
          (set ok false)))
      (when ok
        (def nextc (array/concat (array/slice chosen) @[i]))
        (array/push result (seq [j :in nextc] (nodes j)))
        (array/push stack [(+ i 1) nextc]))))
  result)


# ==============================================================================
#  Sources and sinks
# ==============================================================================

(define source-nodes
  "Return the array of source nodes."
  [g :directed] :array
  (def in-deg (in-degree-table g))
  (seq [n :in (nodes g)
        :when (= 0 (- (in in-deg n) (edge-multiplicity g n n)))]
       n))

(define sink-nodes
  "Return the array of sink nodes."
  [g :directed] :array
  (seq [n :in (nodes g)
        :when (empty? (seq [v :in (succ g n) :when (not= v n)] v))]
       n))


# ==============================================================================
#  Cycle breaking (Eades feedback arc heuristic)
# ==============================================================================

(defn- alive-count
  ```Number of non-nil values in a table `s`.```
  [s]
  (var n 0)
  (each [_ v] (pairs s) (when v (++ n)))
  n)

(defn- active-nodes
  ```Live `node` names in a fixed canonical `order`.```
  [order nodes]
  (seq [n :in order :when (get nodes n)] n))

(define feedback-arc-indices
  "Eades feedback-arc heuristic over an explicit arc list."
  [arcs] :table
  (let [by-id @{}
        nodes @{}
        incoming @{}
        outgoing @{}]
    (each [i a] (pairs arcs)
      (put by-id i a)
      (put nodes (a 0) true)
      (put nodes (a 1) true)
      (def out-adj (get outgoing (a 0) @{}))
      (put out-adj i true)
      (put outgoing (a 0) out-adj)
      (def in-adj (get incoming (a 1) @{}))
      (put in-adj i true)
      (put incoming (a 1) in-adj))

    (def node-order (canonical-nodes (keys nodes)))
    (def non-cycle @{})

    (defn prune-node
      "Flag a removed node's arcs as non-cycle and drop it."
      [n]
      (each arc-id (keys (get incoming n @{}))
        (put non-cycle arc-id true)
        (def adj-end ((by-id arc-id) 0))
        (def adj-set (get outgoing adj-end))
        (put adj-set arc-id nil)
        (when (and (= 0 (alive-count adj-set))
                   (= 0 (alive-count (get incoming adj-end @{}))))
          (put nodes adj-end nil)
          (put outgoing adj-end nil)
          (put incoming adj-end nil)))
      (each arc-id (keys (get outgoing n @{}))
        (put non-cycle arc-id true)
        (def adj-end ((by-id arc-id) 1))
        (def adj-set (get incoming adj-end))
        (put adj-set arc-id nil)
        (when (and (= 0 (alive-count adj-set))
                   (= 0 (alive-count (get outgoing adj-end @{}))))
          (put nodes adj-end nil)
          (put incoming adj-end nil)
          (put outgoing adj-end nil)))
      (put nodes n nil)
      (put incoming n nil)
      (put outgoing n nil))

    # Repeatedly strip sinks then sources; whatever survives is on a cycle..
    (while (pos? (alive-count nodes))
      (var found true)
      (while found
        (set found false)
        (each n (active-nodes node-order nodes)
          (when (= 0 (alive-count (get outgoing n @{})))
            (set found true)
            (prune-node n))))
      (var found-source true)
      (while found-source
        (set found-source false)
        (each n (active-nodes node-order nodes)
          (when (= 0 (alive-count (get incoming n @{})))
            (set found-source true)
            (prune-node n))))
      (when (pos? (alive-count nodes))
        (var max-node nil)
        (var max-diff nil)
        (var max-edges 0)
        (var max-incoming 0)
        (each n (active-nodes node-order nodes)
          (def out-cnt (alive-count (get outgoing n @{})))
          (def in-cnt (alive-count (get incoming n @{})))
          (def diff (- out-cnt in-cnt))
          (def edge-cnt (+ out-cnt in-cnt))
          (when (or (nil? max-diff)
                    (> diff max-diff)
                    (and (= diff max-diff)
                         (or (> edge-cnt max-edges)
                             (and (= edge-cnt max-edges)
                                  (= in-cnt max-incoming)
                                  (< n max-node)))))
            (set max-diff diff)
            (set max-node n)
            (set max-edges edge-cnt)
            (set max-incoming in-cnt)))
        (each arc-id (keys (get incoming max-node @{}))
          (def a (by-id arc-id))
          (put (get incoming max-node) arc-id nil)
          (put (get outgoing (a 0)) arc-id nil))
        (prune-node max-node)))

    (def result @{})
    (each [i _] (pairs arcs)
      (unless (get non-cycle i)
        (put result i true)))
    result))


(define dagify
  ```Turn any graph into a simple DAG, as an array of arcs.

The result has no self-loops, no parallel arcs and no directed cycles, and is
deterministic for a given graph.

Returns an array of {:id int :from node :to node :weight int} arcs.
```
  [g] :array
  (def directed? (structurally-directed? g))
  (def arcs @[])
  (each [u v] (structural-edges-for g directed?)
    (array/push arcs @{:id (length arcs) :from u :to v :weight 1})
    (unless directed?
      (array/push arcs @{:id (length arcs) :from v :to u :weight 0})))
  # Reverse  cycle arcs in place.
  (def cycle-arcs (feedback-arc-indices (seq [a :in arcs] @[(a :from) (a :to)])))
  (each i (sorted (keys cycle-arcs))
    (def a (arcs i))
    (def tmp (a :from))
    (put a :from (a :to))
    (put a :to tmp))
  # Merge parallel arcs by weight
  (def first-arc @{})
  (def order @[])
  (each a arcs
    (def k (tuple (a :from) (a :to)))
    (def prev (get first-arc k))
    (if prev
      (put prev :weight (+ (prev :weight) (a :weight)))
      (do
        (put first-arc k a)
        (array/push order a))))
  (def result @[])
  (each a order
    (when (> (a :weight) 0)
      (put a :id (length result))
      (array/push result a)))
  result)


(define feedback-arc-set
  ```The feedback arc set of a directed graph (via Eades' heuristic).

Returns an array of [u v] edges, each of which lies on a directed cycle, such
that reversing those edge makes the graph acyclic.
```
  [g :directed] :array
  (def arcs (seq [n :in (canonical-nodes (nodes g))
                  v :in (succ g n)
                  :when (not= v n)]
                 @[n v]))
  (def cycle-arcs (feedback-arc-indices arcs))
  (seq [i :in (sorted (keys cycle-arcs))] (arcs i)))


# ==============================================================================
#  Colliders and v-structures (causal inference)
# ==============================================================================

(define colliders
  "Return the array of collider nodes (i.e. two or more distinct incoming edges)."
  [g :directed] :array
  (let [result @[]
        in-deg (in-degree-table g)]
    (each u (nodes g)
      (when (>= (in in-deg u) 2)
        (array/push result u)))
    result))

(define v-structures
  ```Return the array of v-structures.

v-structures are triples [a b c] with directed edges a->b and c->b (b a collider)
and no edge between a and c.
```
  [g :directed] :array
  (def result @[])
  (each b (colliders g)
    (def p (pred g b))
    (for i 0 (length p)
      (for j (+ i 1) (length p)
        (def a (p i))
        (def c (p j))
        (when (and (not= a c)
                   (not (has-edge? g a c))
                   (not (has-edge? g c a)))
          (array/push result @[a b c])))))
  result)
