# -*- mode: janet; -*-
# graphs/traversal: BFS and DFS traversal

#  Provides both type-specific (graph-bfs, digraph-bfs, etc.) and generic
#  (graph-traverse-bfs, graph-traverse-dfs) implementations.
#  Generic versions accept a neighbour-function for dispatch.

(import deft :prefix "")
(import ./simple-graph :prefix "")
(import ./multigraph :prefix "")
(import ./digraph :prefix "")
(import ./util :prefix "")
(import ./types :prefix "")

# ==============================================================================
#  Neighbour-function dispatch
# ==============================================================================

(defn- out-keys-fn
  "Out-neighbour names of a node for adjacency-table graph types."
  [g node]
  (def adj (get (g :adj) node))
  (if adj (keys adj) @[]))

(define graph-neighbours-fn
  "Return a [g node] -> neighbour-names function for the given graph."
  [g :any-graph] :function
  (typecase-strict g
    :multidigraph multidigraph-neighbours
    :digraph out-keys-fn
    :weighted-graph out-keys-fn
    :graph graph-neighbours
    :multigraph multigraph-neighbours
    :tree neighbours
    :weighted-tree neighbours))


# ==============================================================================
#  Generic BFS / DFS
# ==============================================================================

(define graph-traverse-bfs
  ```Generic BFS from `start`, using a neighbour-fn (fn [g node]) → [neighbours].
Returns nodes in BFS order.
```
  [g :any-graph start :node-label neighbour-fn :function] :array
  (let [visited @{start true}
        result @[start]
        queue @[start]]
    (var head 0)
    (while (< head (length queue))
      (let [node (queue head)]
        (++ head)
        (each neighbour (neighbour-fn g node)
          (when (nil? (in visited neighbour))
            (put visited neighbour true)
            (array/push result neighbour)
            (array/push queue neighbour)))))
    result))


(define graph-traverse-dfs
  "Generic DFS from `start`, using a neighbour-fn (fn [g node]) → [neighbours].
Returns nodes in DFS order."
  [g :any-graph start :node-label neighbour-fn :function] :array
  (let [visited @{start true}
        result @[start]
        stack @[start]]
    (while (> (length stack) 0)
      (let [node (last stack)]
        (array/pop stack)
        (each neighbour (neighbour-fn g node)
          (when (nil? (in visited neighbour))
            (put visited neighbour true)
            (array/push result neighbour)
            (array/push stack neighbour)))))
    result))


# ==============================================================================
#  Type-specific BFS/DFS
# ==============================================================================

(define graph-bfs
  "BFS on any supported graph type from start (follows outgoing/undirected edges).
Returns nodes in BFS order."
  [g :any-graph start :dynamic] :array
  (graph-traverse-bfs g start (graph-neighbours-fn g)))

(define graph-dfs
  "DFS on any supported graph type from start (follows outgoing/undirected edges).
Returns nodes in DFS order."
  [g :any-graph start :dynamic] :array
  (graph-traverse-dfs g start (graph-neighbours-fn g)))

(define multidigraph-bfs
  "BFS on :multidigraph from start (follows outgoing edges)."
  [g :multidigraph start :dynamic] :array
  (graph-traverse-bfs g start multidigraph-neighbours))

(define multidigraph-dfs
  "DFS on :multidigraph from start (follows outgoing edges)."
  [g :multidigraph start :dynamic] :array
  (graph-traverse-dfs g start multidigraph-neighbours))

(define digraph-bfs
  "BFS on :digraph from start (follows outgoing edges)."
  [g :digraph start :dynamic] :array
  (graph-traverse-bfs g start out-keys-fn))

(define digraph-dfs
  "DFS on :digraph from start (follows outgoing edges)."
  [g :digraph start :dynamic] :array
  (graph-traverse-dfs g start out-keys-fn))


# ==============================================================================
#  BFS dispatch by graph type
# ==============================================================================

(define bfs-path
  "BFS on graph `g` from `start`. Dispatches to type-specific BFS."
  [g :any-graph start :dynamic] :array
  (typecase-strict g
    :multidigraph (multidigraph-bfs g start)
    :digraph (digraph-bfs g start)
    :weighted-graph (digraph-bfs g start)
    :graph (graph-bfs g start)
    :multigraph (graph-bfs g start)
    :tree (graph-bfs (g :digraph) start)
    :weighted-tree (digraph-bfs (g :graph) start)))


# ==============================================================================
#  Full DFS traversal
# ==============================================================================

(define dfs-all
  "Full DFS traversal visiting all nodes in graph `g`."
  [g :any-graph] :array
  (def nodes (nodes g))
  (def visited @{})
  (def result @[])
  (def nf (graph-neighbours-fn g))
  (each node nodes
    (when (nil? (in visited node))
      (put visited node true)
      (array/push result node)
      (var stack @[node])
      (while (> (length stack) 0)
        (let [cur (last stack)]
          (array/pop stack)
          (each neighbour (nf g cur)
            (when (nil? (in visited neighbour))
              (put visited neighbour true)
              (array/push result neighbour)
              (array/push stack neighbour)))))))
  result)


# ==============================================================================
#  Topological sort (Kahn's algorithm)
# ==============================================================================

(define topological-sort
 ```Topological sort of a directed acyclic graph.
Uses Kahn's algorithm, works on :digraph, :weighted-graph and :multidigraph.

Returns array of nodes in topological order, or nil if a cycle exists.
```
  [g :any-graph] (:or :array :nil)
  (let [node-set (keys (to-set (nodes g)))
        in-deg (in-degree-table g)
        result @[]
        queue @[]
        nf (graph-neighbours-fn g)]
    (each node node-set
      (when (= 0 (in in-deg node))
        (array/push queue node)))
    (var head 0)
    (while (< head (length queue))
      (let [node (queue head)]
        (++ head)
        (array/push result node)
        (each neighbour (nf g node)
          (def nd (- (in in-deg neighbour)
                     (edge-multiplicity g node neighbour)))
          (put in-deg neighbour nd)
          (when (= 0 nd)
            (array/push queue neighbour)))))
    (when (= (length result) (length node-set))
      result)))


# ==============================================================================
#  Strongly connected components (Kosaraju's algorithm)
# ==============================================================================

(define kosaraju-scc
  ```Find strong connectivity components.
Uses Kosaraju's two-pass DFS. Works on :digraph and :multidigraph.

Returns an array of arrays containing the SCC's node labels.
```
  [g :directed] :array
  (def nodes (nodes g))
  (def nf (graph-neighbours-fn g))
  # Pass 1. DFS on the original graph
  (def visited @{})
  (def finish @[])
  (each start nodes
    (when (nil? (in visited start))
      (put visited start true)
      (var stack @[[start 0 (nf g start)]])
      (while (> (length stack) 0)
        (def top (dec (length stack)))
        (def entry (stack top))
        (def node (entry 0))
        (def i (entry 1))
        (def nbrs (entry 2))
        (if (< i (length nbrs))
          (do
            (put stack top [node (inc i) nbrs])
            (def nxt (nbrs i))
            (when (nil? (in visited nxt))
              (put visited nxt true)
              (array/push stack [nxt 0 (nf g nxt)])))
          (do
            (array/pop stack)
            (array/push finish node))))))
  # Pass 2. DFS on the reversed graph in decreasing order.
  (def comps @[])
  (def seen @{})
  (var i (dec (length finish)))
  (while (>= i 0)
    (def start (finish i))
    (when (nil? (in seen start))
      (put seen start true)
      (def comp @[])
      (var stack @[start])
      (while (> (length stack) 0)
        (def node (last stack))
        (array/pop stack)
        (array/push comp node)
        (each nb (in-neighbours g node)
          (when (nil? (in seen nb))
            (put seen nb true)
            (array/push stack nb))))
      (array/push comps comp))
    (-- i))
  comps)


# ==============================================================================
#  Hamiltonian path in a DAG
# ==============================================================================

(define hamiltonian-path-dag
  ```Find a Hamiltonian path for a DAG.

Uses unique topological order and checks for spanning chain of consecutive edges.

Returns array of node labels, or nil if no path exists.
```
  [g :any-graph] (:or :array :nil)
  (def order (topological-sort g))
  (if (nil? order) nil
    (do
      (def nf (graph-neighbours-fn g))
      (def path @[(order 0)])
      (for i 1 (length order)
        (def u (path (dec (length path))))
        (def v (order i))
        (when (has-edge? g u v)
          (array/push path v)))
      (if (= (length path) (length order)) path nil))))


# ==============================================================================
#  Semi-connected test (on the SCC condensation)
# ==============================================================================

(define semi-connected?
  ```Return true if a directed graph is semi-connected.

A graph is semi-connected if there is a directed path u→v or v→u for every pair
of vertices u, v. Computed using the SCC condensation.
```
  [g :digraph] :boolean
  (let [comps (kosaraju-scc g)
        nc (length comps)]
    (if (<= nc 1)
      true
      (do
        # component id per node
        (def cid @{})
        (for id 0 nc
          (each n (comps id) (put cid n id)))
        # implicit condensation. per-component distinct successors and
        # in-degrees via the adjacency
        (def adj (g :adj))
        (def succ @{})
        (def in-deg @{})
        (for id 0 nc
          (def succ-id @{})
          (each u (comps id)
            (def adj-u (get adj u))
            (when adj-u
              (each v (keys adj-u)
                (def vid (get cid v))
                (when (and vid (not= id vid))
                  (put succ-id vid true)))))
          (put succ id succ-id)
          (each s (keys succ-id)
            (put in-deg s (+ 1 (or (get in-deg s) 0)))))
        # Kahn's algorithm over the implicit condensation DAG
        (def queue @[])
        (var head 0)
        (for id 0 nc
          (unless (get in-deg id) (array/push queue id)))
        (def order @[])
        (while (< head (length queue))
          (def c (queue head))
          (++ head)
          (array/push order c)
          (each s (keys (in succ c))
            (def nd (- (in in-deg s) 1))
            (put in-deg s nd)
            (when (= 0 nd) (array/push queue s))))
        # a DAG is semi-connected iff its topological order is a spanning chain
        (if (not= (length order) nc)
          false
          (do
            (var semi true)
            (for i 1 nc
              (unless (in (succ (order (dec i))) (order i))
                (set semi false)
                (break)))
            semi))))))


# ==============================================================================
#  Eulerian cycle (Hierholzer's algorithm)
# ==============================================================================

(defn- out-bound-edge-list
  "All outgoing edges of u as @[v idx] entries, one per parallel edge."
  [g u]
  (typecase-strict g
    :multidigraph
      (let [out-tbl (get (g :out) u) result @[]]
        (when out-tbl
          (each [v cnt] (pairs out-tbl)
            (for i 0 cnt (array/push result @[v i]))))
        result)
    (fn [untagged]
      (seq [v :in (neighbours g u)] @[v 0]))))

(defn- in-bound-edge-list
  "All incoming edges of u as @[v idx] entries, one per parallel edge."
  [g u]
  (typecase-strict g
    :multidigraph
      (let [in-tbl (get (g :in) u) result @[]]
        (when in-tbl
          (each [v cnt] (pairs in-tbl)
            (for i 0 cnt (array/push result @[v i]))))
        result)
    (fn [untagged]
      (seq [v :in (in-neighbours g u)] @[v 0]))))

(define eulerian-cycle
  ```Find an Eulerian cycle for a directed graph.

Eulerian cycle where every vertex has equal in-degree and out-degree (counting
edge multiplicity). Parallel edges are each traversed exactly once.

Returns an array of nodes, or nil if the graph is not Eulerian.
```
  [g :any-graph] (:or :array :nil)
  (def nodes (nodes g))
  (var balanced true)
  (each u nodes
    (when (not= (length (out-bound-edge-list g u))
                (length (in-bound-edge-list g u)))
      (set balanced false)))
  (if (not balanced)
    nil
    (do
      (var start nil)
      (each u nodes
        (when (> (length (out-bound-edge-list g u)) 0)
          (set start u) (break)))
      (if (nil? start)
        nil
        (do
          # weak connectivity
          (def nf (graph-neighbours-fn g))
          (def reached @{start true})
          (def queue @[start])
          (var head 0)
          (while (< head (length queue))
            (def node (queue head))
            (++ head)
            (each nb (nf g node)
              (unless (in reached nb)
                (put reached nb true)
                (array/push queue nb))))
          (var connected true)
          (each u nodes
            (when (and (> (length (out-bound-edge-list g u)) 0) (nil? (in reached u)))
              (set connected false)))
          (if (not connected)
            nil
            # Hierholzer. each node's outgoing edge list in order
            (do
              (def edge-lists @{})
              (def ptr @{})
              (each u nodes
                (put edge-lists u (out-bound-edge-list g u))
                (put ptr u 0))
              (def stack @[start])
              (def circuit @[])
              (while (> (length stack) 0)
                (def u (last stack))
                (def el (get edge-lists u))
                (def p (get ptr u))
                (if (< p (length el))
                  (do
                    (def v (get (el p) 0))
                    (put ptr u (+ p 1))
                    (array/push stack v))
                  (do
                    (array/pop stack)
                    (array/push circuit u))))
              # cycle is in reverse Eulerian order. reverse it.
              (def result @[])
              (var i (dec (length circuit)))
              (while (>= i 0)
                (array/push result (circuit i))
                (-- i))
              result)))))))


(define eulerian-tour
 ```Find a simple cycle in graph `g` visiting all nodes.

Returns an array of nodes, or nil if there is no cycle.
The starting node is not repeated at the end.
```
  [g :any-graph] (:or :array :nil)
  (def vs (nodes g))
  (if (< (length vs) 3)
    nil
    (do
      (def start (vs 0))
      (def result @[start])
      (var prev nil)
      (var current start)
      (var closed false)
      (while (not closed)
        (def nexts (seq [v :in (neighbours g current)
                         :when (not= v prev)] v))
        (if (empty? nexts)
          (set closed true)
          (do
            (def nxt (nexts 0))
            (if (= nxt start)
              (set closed true)
              (if (some (fn [v] (= v nxt)) result)
                (set closed true)
                (do
                  (array/push result nxt)
                  (set prev current)
                  (set current nxt)))))))
      (if (and (= (length result) (length vs))
               (some (fn [v] (= v start))
                     (neighbours g (last result))))
        result
        nil))))


# ==============================================================================
#  DAG shortest path (topological relaxation)
# ==============================================================================

(defn- dag-topological-relaxation
 ```Topological relaxation of a DAG `g` from node `src`.

Returns [dist parent] where parent is {node predecessor}, or nil if cyclic.```
  [g src]
  (def order (topological-sort g))
  (if (nil? order)
    nil
    (do
      (def dist @{src 0})
      (def parent @{src nil})
      (def nf (graph-neighbours-fn g))
      (each u order
        (when (not (nil? (in dist u)))
          (each v (nf g u)
            (def nd (+ (in dist u) 1))
            (when (or (nil? (in dist v)) (< nd (in dist v)))
              (put dist v nd)
              (put parent v u)))))
      [dist parent])))


(define dag-shortest-path
  ```Shortest distances from node `src` in a DAG (assuming unit weights).

Return a table of {node, distance} for all reachable nodes, or nil
if the graph contains a cycle.
```
  [g :any-graph src :dynamic] (:or :table :nil)
  (def res (dag-topological-relaxation g src))
  (if res (res 0) nil))

(define dag-shortest-path-tree
  ```Shortest-path parent tree from node `src` in a DAG (assuming unit weights).

Return a table of {node, parent} reachable from src (src maps to nil).
```
  [g :any-graph src :dynamic] (:or :table :nil)
  (def res (dag-topological-relaxation g src))
  (if res (res 1) nil))


# ==============================================================================
#  Shortest cycle through a given directed edge
# ==============================================================================

(define shortest-cycle-through-edge
  ```Find a shortest directed cycle containing the edge u→v.

Return an array of nodes beginning and ending at u, or nil if no such cycle exists.
```
  [g :directed u :dynamic v :dynamic] (:or :array :nil)
  (if (and (= u v) (has-edge? g u v))
    @[u u]
    (do
      (def nf (graph-neighbours-fn g))
      (def parent @{v nil})
      (def visited @{v true})
      (def queue @[v])
      (var head 0)
      (var found false)
      (while (and (< head (length queue)) (not found))
        (def node (queue head))
        (++ head)
        (each nb (nf g node)
          (unless (in visited nb)
            (put visited nb true)
            (put parent nb node)
            (array/push queue nb)
            (when (= nb u) (set found true)))))
      (if (not found)
        nil
        (do
          # rebuild the path v → ... → u from the parent chain (u → ... → v)
          # the cycle is [u] + reverse( u → ... → v)
          (def back @[u])
          (var cur u)
          (var stuck false)
          (while (and (not stuck) (not= cur v))
            (set cur (in parent cur))
            (if (nil? cur)
              (set stuck true)
              (array/push back cur)))
          (if stuck
            nil
            (do
              (def rback @[])
              (var i (dec (length back)))
              (while (>= i 0)
                (array/push rback (back i))
                (-- i))
              (array/concat @[u] rback))))))))
