# -*- mode: janet; -*-
# graphs/connectivity: connected components, articulation points, bridges,
# biconnected components, and path finding.

#  Generic functions; connected-components, articulation-points, bridges,
#  biconnected-components, work on any graph with :nodes and :adj fields.
#
#  Type-specific functions are prefixed; graph-*, multidigraph-*, etc.

(import deft :prefix "")
(import ./digraph :prefix "")
(import ./flow :prefix "")
(import ./multigraph :prefix "")
(import ./traversal :prefix "")
(import ./util :prefix "")
(import ./types :prefix "")


(defn- g->weak-bfs
  ```BFS over the undirected graph `g` from `start` node.

`nb` is an undirected-neighbour accessor (see `make-undirected-neighbours-fn`)

Returns array of reachable node names.
```
  [g start nb]
  (graph-traverse-bfs g start nb))

(defn- node-count [g]
  (length (nodes g)))

(defn- put-pair
  "Record the unordered pair a--b in both orientations inside table t."
  [t a b]
  (put t a (put (or (get t a) @{}) b true))
  (put t b (put (or (get t b) @{}) a true)))

(defn- undirected-edge-count
 ```Number of parallel edges between `u` and `v` on undirected mulitigraph `g`.```
  [g u v]
  (typecase-strict g
    :multigraph (or (get-in (g :adj) [u v]) 0)
    (fn [_] (if (or (get (get (g :adj) u) v)
                    (get (get (g :adj) v) u))
              1 0))))


# ==============================================================================
#  Connected components
# ==============================================================================

(define connected-components
 ```Connected components of the underlying undirected graph.

Returns array of arrays, each a connected component.```
  [g :any-graph] :array
  (let [nodes (nodes g)
        nb (make-undirected-neighbours-fn g)
        visited @{}
        result @[]]
    (each node nodes
      (when (nil? (in visited node))
        (let [component @[node]]
          (put visited node true)
          (var stack @[node])
          (while (> (length stack) 0)
            (let [cur (last stack)]
              (array/pop stack)
              (each neighbour (nb g cur)
                (when (nil? (in visited neighbour))
                  (put visited neighbour true)
                  (array/push component neighbour)
                  (array/push stack neighbour)))))
          (array/push result component))))
    result))


(define graph-connected?
  ```True when the graph `g` contains a single connected component.
Weakly-connected for directed types.```
  [g :any-graph] :boolean
  (<= (length (connected-components g)) 1))


(define graph-connected-components
 ```Return array of arrays, each a connected component of graph `g`.```
  [g :any-graph] :array
  (let [nb (make-undirected-neighbours-fn g)
        visited @{}
        result @[]]
    (each node (nodes g)
      (when (nil? (in visited node))
        (let [component (g->weak-bfs g node nb)]
          (array/push result component)
          (each n component (put visited n true)))))
    result))


(define multidigraph-connected?
  ```Return true when a directed graph is weakly connected.
i.e. its underlying undirected graph is connected.```
  [g :multidigraph] :boolean
  (let [nodes (multidigraph-nodes g)]
    (or (empty? nodes)
        (= (length (g->weak-bfs g (first nodes)
                                (make-undirected-neighbours-fn g)))
           (multidigraph-node-count g)))))


(define multidigraph-connected-components
  ```Find weakly connected components of a directed graph.
Groups nodes connected by edges regardless of direction.```
  [g :multidigraph] :array
  (graph-connected-components g))


# ==============================================================================
#  Articulation points, bridges (Tarjan's DFS, undirected)
# ==============================================================================

(defn- tarjan-undirected
  ```Low-link DFS over the undirected view of `g`.

Returns [disc low parent tree-edges] where tree-edges are edges [u v] in DFS order.
```
  [g]
  (let [nodes (nodes g)
        nb (make-undirected-neighbours-fn g)
        visited @{}
        disc @{}
        low @{}
        parent @{}
        tree-edges @[]]
    (var time 0)
    (defn dfs [u]
      (set time (+ time 1))
      (put disc u time)
      (put low u time)
      (put visited u true)
      (each v (nb g u)
        (if (not (get visited v))
          (do
            (put parent v u)
            (dfs v)
            (put low u (min (get low u) (get low v)))
            (array/push tree-edges @[u v]))
          (when (and (not= v (get parent u)) (get disc v))
            (put low u (min (get low u) (get disc v)))))))
    (each u nodes
      (when (not (get visited u))
        (put parent u nil)
        (dfs u)))
    @[disc low parent tree-edges]))


(define articulation-points
  ```Find articulation points, nodes whose removal disconnects the graph.

Return array of node names (sorted using Tarjan DFS)
```
  [g :any-graph] :array
  (def [disc low parent _] (tarjan-undirected g))
  (def children @{})
  (each u (nodes g) (put children u @[]))
  (each [v p] (pairs parent)
    (when p (array/push (get children p) v)))
  (def ap-set @{})
  (each u (nodes g)
    (def p (get parent u))
    (if (nil? p)
      (when (> (length (get children u)) 1)
        (put ap-set u true))
      (each v (get children u)
        (when (>= (get low v) (get disc u))
          (put ap-set u true)
          (break)))))
  (sort (seq [u :in (nodes g) :when (get ap-set u)] u)))


(define bridges
 ```Find bridges, edges whose removal disconnects the graph.

Return array of [u v] edge pairs. (sorted using Tarjan DFS).
```
  [g :any-graph] :array
  (def [disc low _ edges] (tarjan-undirected g))
  (def result @[])
  (each [u v] edges
    (when (and (> (get low v) (get disc u))
               (= 1 (undirected-edge-count g u v)))
      (array/push result [u v])))
  result)


# ==============================================================================
#  Biconnected components
# ==============================================================================

(defn- norm-edge
  ```An edge pair with the lower label first.
An edge is written the same way whichever endpoint the traversal reached first.```
  [e]
  (if (< (e 0) (e 1)) (array ;e) @[(e 1) (e 0)]))


(defn- sort-comp
  "A component's edges ordered by value."
  [edges]
  (sort edges (fn [a b]
                (if (= (a 0) (b 0))
                  (< (a 1) (b 1))
                  (< (a 0) (b 0))))))


(defn- biconnected-undirected
  ```The edge-biconnected components of the undirected view of `g`.```
  [g]
  (let [nodes (nodes g)
        nb (make-undirected-neighbours-fn g)
        disc @{}
        low @{}
        parent @{}
        stack @[]
        out @[]]
    (var time 0)
    (defn dfs [u]
      (set time (+ time 1))
      (put disc u time)
      (put low u time)
      (def p (get parent u))
      # A multigraph can have parallel edges, the first one seen
      # is the tree edge and any later one is a back edge.
      (var skipped-parent false)
      (each v (nb g u)
        (cond
          (= v u) nil
          (not (get disc v))
          (do
            (put parent v u)
            (array/push stack @[u v])
            (dfs v)
            (put low u (min (get low u) (get low v))))
          (and (= v p) (nil? (get parent p)) (not skipped-parent))
          (set skipped-parent true)
          (and (not= v p) (< (get disc v) (get disc u)))
          (do
            (array/push stack @[u v])
            (put low u (min (get low u) (get disc v))))))
      (when (and p (> (get low u) (get disc p)))
        (def popped @[])
        (var bridge nil)
        (var popping true)
        (while popping
          (if (zero? (length stack))
            (set popping false)
            (do
              (def e (array/pop stack))
              (if (and (= (e 0) p) (= (e 1) u))
                (do (set bridge e) (set popping false))
                (array/push popped e)))))
        (when bridge
          (when (pos? (length popped))
            (array/push out (sort-comp (map norm-edge popped))))
          (array/push out (sort-comp @[(norm-edge bridge)])))))
    (each u nodes
      (unless (get disc u)
        (put parent u nil)
        (dfs u)
        (when (pos? (length stack))
          (def comp @[])
          (while (pos? (length stack))
            (array/push comp (norm-edge (array/pop stack))))
          (array/push out (sort-comp comp)))))
    out))


(define biconnected-components
  ```Find biconnected components, maximal 2-edge-connected blocks.

Bridges form singleton components, each 2-edge-connected block is one component.
Works on any graph type (edges are treated as undirected).

Returns array of arrays of [u v] edge pairs.
```
  [g :any-graph] :array
  (biconnected-undirected g))


# ==============================================================================
#  Path finding
# ==============================================================================

(defn- path-exists*
  ```BFS path-exists over `neighbour-fn` (fn [g node] → neighbours).```
  [g start end neighbour-fn]
  (var found false)
  (when (= start end) (set found true))
  (let [visited @{start true}
        queue @[start]]
    (var head 0)
    (while (and (not found) (< head (length queue)))
      (let [node (queue head)]
        (++ head)
        (when (= node end) (set found true))
        (each neighbour (neighbour-fn g node)
          (when (nil? (in visited neighbour))
            (put visited neighbour true)
            (array/push queue neighbour))))))
  found)


(defn- shortest-path*
  ```BFS shortest path over `neighbour-fn` or nil when `end` is unreachable.```
  [g start end neighbour-fn]
  (var result false)
  (when (= start end)
    (set result @[start]))
  (let [visited @{start true}
        parent @{start nil}
        queue @[start]]
    (var head 0)
    (while (and (= result false)
                (< head (length queue)))
      (let [node (queue head)]
        (++ head)
        (each neighbour (neighbour-fn g node)
          (when (and (= result false)
                     (nil? (in visited neighbour)))
            (put visited neighbour true)
            (put parent neighbour node)
            (array/push queue neighbour)
            (when (= neighbour end)
              (let [path @[]]
                (var current end)
                (while current
                  (array/push path current)
                  (set current (in parent current)))
                (set result (reverse path)))))))))
  (if (= result false) nil result))


(define graph-path-exists?
  "True when there is a path from node `start` to `end`."
  [g :any-graph start :dynamic end :dynamic] :boolean
  (path-exists* g start end undirected-neighbours))

(define multidigraph-path-exists?
  "True when there is a directed path from node `start` to `end`."
  [g :multidigraph start :dynamic end :dynamic] :boolean
  (path-exists* g start end multidigraph-neighbours))


(define graph-shortest-path
  ```Shortest unweighted path from node `start` to `end`.
Returns array of nodes, or nil if no path exists.```
  [g :any-graph start :dynamic end :dynamic] (:or :array :nil)
  (shortest-path* g start end undirected-neighbours))

(define multidigraph-shortest-path
  ```Shortest directed path from start to end.
Returns array of nodes, or nil if no path exists.```
  [g :multidigraph start :dynamic end :dynamic] (:or :array :nil)
  (shortest-path* g start end multidigraph-neighbours))


# ==============================================================================
#  Edge connectivity (edge-Menger, via max flow)
# ==============================================================================

(defn- make-edge-capacity-digraph
  ```Capacity digraph with one unit (w for :multigraph) in each direction per edge.```
  [g]
  (let [ns (nodes g)
        idx @{}]
    (for i 0 (length ns) (put idx (ns i) i))
    (def dg (make-digraph @[] @{}))
    (each i (range (length ns)) (digraph-add-node dg (string i)))
    (def seen @{})
    (each u ns
      (each [v w] (out-neighbours g u)
        (when (not= u v)
          (unless (get-in seen [u v])
            (put-pair seen u v)
            (def cap (if (= w true) 1 w))
            (digraph-add-edge dg (string (get idx u)) (string (get idx v)) cap)
            (digraph-add-edge dg (string (get idx v)) (string (get idx u)) cap)))))
    @[dg ns]))


(defn- edge-connectivity-undirected
  "Min over unordered pairs of the max-flow with unit capacities."
  [g]
  (def [dg ns] (make-edge-capacity-digraph g))
  (def n (length ns))
  (var best math/inf)
  (for i 0 n
    (for j (+ i 1) n
      (set best (min best (get (dinic dg (string i) (string j)) :flow)))))
  (if (= best math/inf) 0 (math/floor best)))

(define edge-connectivity
  ```The minimum number of edges whose deletion disconnects the graph.

Edge connectivity λ(g) is computed as the minimum over all unordered vertex
pairs of the maximum number of edge-disjoint paths joining them, by the
edge-Menger theorem. Edge multiplicities are respected for multigraphs.
Directed graphs are not supported.
```
  [g :any-graph] :number
  (typecase-strict g
    :graph      (edge-connectivity-undirected g)
    :multigraph (edge-connectivity-undirected g)
    (fn [_] (error "edge-connectivity requires an undirected graph type (or :multigraph)"))))


# ==============================================================================
#  Vertex connectivity (vertex-Menger, via max flow on a split graph)
# ==============================================================================

(defn- vertex-connectivity-value
  ```Max number of internally vertex-disjoint s→t paths via the in/out split.

Every vertex v gets a capacity-1 arc in(v)→out(v) (unlimited for s and t) and
every arc u→v becomes an unlimited arc out(u)→in(v).```
  [g s t]
  (let [ns (nodes g)
        idx @{}]
    (for i 0 (length ns) (put idx (ns i) i))
    (defn inn [i] (string "i" i))
    (defn out [i] (string "o" i))
    (def dg (make-digraph @[] @{}))
    (each u ns
      (def i (get idx u))
      (digraph-add-node dg (inn i))
      (digraph-add-node dg (out i))
      (digraph-add-edge dg (inn i) (out i) 1))
    (def is (get idx s))
    (def it (get idx t))
    (digraph-add-edge dg (inn is) (out is) math/inf)
    (digraph-add-edge dg (inn it) (out it) math/inf)
    (each u ns
      (each [v w] (out-neighbours g u)
        (when (not= u v)
          (digraph-add-edge dg (out (get idx u)) (inn (get idx v)) math/inf))))
    (get (dinic dg (out is) (inn it)) :flow)))

(defn- vertex-connectivity-min
 ```Min over non-adjacent pairs of graph `g`.```
  [g directed?]
  (let [ns (nodes g)
        n (length ns)]
    (cond
      (< n 2) 0
      (do
        (def pairs
          (seq [i :range [0 n]
                j :range [0 n]
                :when (and (if directed? (not= i j) (< i j))
                           (not (has-edge? g (ns i) (ns j))))]
            @[i j]))
        (if (empty? pairs)
          (- n 1)
          (do
            (var best math/inf)
            (each [i j] pairs
              (set best (min best (vertex-connectivity-value g (ns i) (ns j)))))
            (math/floor best)))))))


(define vertex-connectivity
  ```The minimum number of vertices whose deletion disconnects the graph.

Vertex connectivity κ(g) is omputed by the vertex-Menger theorem as the minimum,
over non-adjacent pairs, of the maximum number of internally vertex-disjoint
paths. Uses max flow on the vertex-splitting network.

For undirected graphs the minimum is taken over unordered non-adjacent pairs,
for directed graphs over ordered pairs (u, v) with no arc u→v (arc-Menger
version). Complete graphs have no non-adjacent pair, so κ = n-1.
```
  [g :any-graph] :number
  (typecase-strict g
    :graph        (vertex-connectivity-min g false)
    :multigraph   (vertex-connectivity-min g false)
    :digraph      (vertex-connectivity-min g true)
    :multidigraph (vertex-connectivity-min g true)
    (fn [_] (error "vertex-connectivity requires a :graph or :digraph type"))))
