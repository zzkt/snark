# -*- mode: janet; -*-
# graphs/init: graph module loader, unified interface, type-based dispatch

(import deft :prefix "")
(import ./types :prefix "")
(import ./util :prefix "_u_")
(import ./simple-graph :prefix "_sg_")
(import ./multigraph :prefix "_mg_")
(import ./digraph :prefix "_dg_")
(import ./weighted-graph :prefix "_w_")
(import ./tree :prefix "_r_")
(import ./traversal :prefix "_t_")
(import ./connectivity :prefix "_c_")
(import ./shortest-paths :prefix "_sp_")
(import ./cycles :prefix "_cy_")
(import ./metrics :prefix "_m_")
(import ./pagerank :prefix "_p_")
(import ./random-graph :prefix "_rg_")
(import ./centrality :prefix "_c2_")
(import ./clustering :prefix "_c3_")
(import ./lca :prefix "_l_")
(import ./dag :prefix "_dag_")
(import ./layout :prefix "_gd_")
(import ./similarity :prefix "_s_")
(import ./heap :prefix "_h_")
(import ./distance-matrix :prefix "_dm_")
(import ./flow :prefix "_f_")
(import ./hamiltonian :prefix "_hm_")
(import ./matching :prefix "_m4_")
(import ./cuts :prefix "_cu_")
(import ./sequence :prefix "_sq_")
(import ./cores :prefix "_co_")
(import ./export :prefix "_e_")
(import ./import :prefix "_i_")
(import ./colouring :prefix "_col_")
(import ./spanning-tree :prefix "_spt_")
(import ./tournament :prefix "_tour_")
(import ./planarity :prefix "_pl_")


# Importers
(def graph-from-adjacency-list _i_graph-from-adjacency-list)
(def graph-from-adjacency-matrix _i_graph-from-adjacency-matrix)
(def graph-from-dot _i_graph-from-dot)
(def graph-from-gexf _i_graph-from-gexf)
(def import-dot _i_import-dot)
(def import-gexf _i_import-gexf)
(def import-file _i_import-file)
(def import-hog _i_import-hog)

# ==============================================================================
#  Nodes & edges
# ==============================================================================

(define add-node
  "Add a node to any graph type. No-op if the node already exists."
  [g :any-graph node :node-label]
  (typecase-strict g
    :multidigraph (_mg_multidigraph-add-node g node)
    :digraph (_dg_digraph-add-node g node)
    :weighted-graph (_w_weighted-graph-add-node g node)
    :graph (_sg_graph-add-node g node)
    :multigraph (_mg_multigraph-add-node g node)
    :tree (_dg_digraph-add-node (g :digraph) node)
    :weighted-tree (_w_weighted-graph-add-node (g :graph) node)))

(define add-edge
 ```Add an edge between two nodes.
Default weight is 1 when omitted for weighted graphs. For :tree the edge is
added and recorded as a parent link (i.e. `a` is parent of `b`).```
  [g :any-graph a :node-label b :node-label &opt weight]
  (typecase-strict g
    :multidigraph (_mg_multidigraph-add-edge g a b)
    :digraph (_dg_digraph-add-edge g a b (or weight 1))
    :weighted-graph (_w_weighted-graph-add-edge g a b (or weight 1))
    :graph (_sg_graph-add-edge g a b)
    :multigraph (_mg_multigraph-add-edge g a b)
    :tree (do (_dg_digraph-add-edge (g :digraph) a b (or weight 1))
              (put (g :parent) b a)
              g)
    :weighted-tree (do (_w_weighted-graph-add-edge (g :graph) a b (or weight 1))
                       (put (g :parent) b a)
                       g)))

(define remove-edge
  "Remove an edge between two nodes."
  [g :any-graph a :node-label b :node-label]
  (typecase-strict g
    :multidigraph (_mg_multidigraph-remove-all-edges g a b)
    :digraph (_dg_digraph-remove-edge g a b)
    :graph (_sg_graph-remove-edge g a b)
    :multigraph (_mg_multigraph-remove-all-edges g a b)
    :weighted-graph (_w_weighted-graph-remove-edge g a b)
    :tree (do (_dg_digraph-remove-edge (g :digraph) a b)
              (when (= (get (g :parent) b) a) (put (g :parent) b nil))
              g)
    :weighted-tree (do (_w_weighted-graph-remove-edge (g :graph) a b)
                       (when (= (get (g :parent) b) a) (put (g :parent) b nil))
                       g)))

(define remove-edges
  "Remove multiple edges from any graph type.
Accepts a single iterable of edges or variadic edge entries, each of the form [a b].
Returns graph, calls can be chained."
  [g :any-graph & edges]
  (if (= (length edges) 0)
    (do g)
    (if (= (length edges) 1)
      (let [es (edges 0)]
        (cond
          (or (nil? es) (and (indexed? es) (= (length es) 0))) g
          (and (indexed? es) (not (keyword? es)) (not (empty? es)) (indexed? (es 0)))
          (each e es (remove-edge g (e 0) (e 1)))
          (and (indexed? es) (> (length es) 1)) (remove-edge g (es 0) (es 1))
          true g))
      (each e edges
        (when (indexed? e) (remove-edge g (e 0) (e 1))))))
  g)

(define remove-node
  "Remove a node and all its incident edges."
  [g :any-graph node :node-label]
  (typecase-strict g
    :multidigraph (_mg_multidigraph-remove-node g node)
    :digraph (_dg_digraph-remove-node g node)
    :graph (_sg_graph-remove-node g node)
    :multigraph (_mg_multigraph-remove-node g node)
    :weighted-graph (_w_weighted-graph-remove-node g node)
    :tree (do (_dg_digraph-remove-node (g :digraph) node)
              (let [orphans (seq [kv :in (pairs (g :parent)) :when (= (kv 1) node)]
                              (kv 0))]
                (each k orphans (put (g :parent) k nil)))
              (put (g :parent) node nil)
              g)
    :weighted-tree (do (_w_weighted-graph-remove-node (g :graph) node)
                       (let [orphans (seq [kv :in (pairs (g :parent)) :when (= (kv 1) node)]
                                       (kv 0))]
                         (each k orphans (put (g :parent) k nil)))
                       (put (g :parent) node nil)
                       g)))

(define remove-nodes
  "Remove multiple nodes from any graph type.
Accepts a single iterable of nodes or variadic node names.
Returns graph, calls can be chained."
  [g :any-graph & nodes]
  (if (= (length nodes) 0)
    (do g)
    (if (= (length nodes) 1)
      (let [ns (nodes 0)]
        (cond
          (or (nil? ns) (and (indexed? ns)
                             (= (length ns) 0))) g
          (and (indexed? ns) (not (keyword? ns))
               (> (length ns) 0)
               (indexed? (ns 0)))
          (each n ns (remove-node g n))
          (and (indexed? ns) (not (keyword? ns)))
          (each n ns (remove-node g n))
          true (remove-node g ns)))
      (each n nodes (remove-node g n))))
  g)

(define add-nodes
  "Add multiple nodes to any graph type.
Accepts a single iterable of nodes or variadic node names.
Returns graph, calls can be chained."
  [g :any-graph & nodes]
  (if (= (length nodes) 0)
    (do g)
    (if (= (length nodes) 1)
      (let [ns (nodes 0)]
        (cond
          (or (not ns)
              (and (indexed? ns)
                   (= (length ns) 0))) g
          (and (indexed? ns)
               (not (keyword? ns))
               (not (empty? ns))
               (indexed? (ns 0))) (each n ns (add-node g n))
          (and (indexed? ns)
               (not (keyword? ns))) (each n ns (add-node g n))
          true (add-node g ns)))
      (each n nodes (add-node g n))))
  g)

(define add-edges
  ```Add multiple edges to any graph type.

Accepts a single iterable of edges, or edges of the form [a b] or [a b weight].
Missing endpoint are created.

Returns graph, calls can be chained.```
  [g :any-graph & edges]
  (if (= (length edges) 0)
    (do g)
    (if (= (length edges) 1)
      (let [es (edges 0)]
        (cond
          (or (nil? es)
              (and (indexed? es)
                   (= (length es) 0))) g
          (and (indexed? es)
               (not (keyword? es))
               (> (length es) 0)
               (indexed? (es 0))) (each e es
                                    (if (> (length e) 2)
                                      (add-edge g (e 0) (e 1) (e 2))
                                      (add-edge g (e 0) (e 1))))
          (and (indexed? es)
               (> (length es) 1)) (if (> (length es) 2)
                                    (add-edge g (es 0) (es 1) (es 2))
                                    (add-edge g (es 0) (es 1)))
          true g))
      (each e edges
        (if (indexed? e)
          (if (> (length e) 2)
            (add-edge g (e 0) (e 1) (e 2))
            (add-edge g (e 0) (e 1)))
          (do nil)))))
  g)


# ==============================================================================
#  Queries
# ==============================================================================

(define has-node?
  "True when the node is in the graph."
  [g :any-graph node :node-label] :boolean
  (_u_has-node? g node))

(define has-edge?
  "True when an edge exists from a to b."
  [g :any-graph a :node-label b :node-label] :boolean
  (_u_has-edge? g a b))

(define neighbours
  "Return the names of neighbours of a node.
For directed graphs, returns outgoing neighbours."
  [g :any-graph node :node-label] :array
  (_u_neighbours g node))

(define nodes
  "Return all node names from any graph type."
  [g :any-graph] :array
  (_u_nodes g))


# ==============================================================================
#  Degree
# ==============================================================================

(define degree
  ```Total degree of a node.

- For undirected graphs this is the neighbour count (self-loops contribute +2
for :graph and double multiplicity for :multigraph)
- For directed graphs, in-degree + out-degree.
```
  [g :any-graph node :node-label] :number
  (typecase-strict g
    :graph (_sg_graph-degree g node)
    :multigraph (_mg_multigraph-degree g node)
    :multidigraph (+ (_mg_multidigraph-in-degree g node)
                     (_mg_multidigraph-out-degree g node))
    :digraph (+ (_dg_digraph-in-degree g node)
                (_dg_digraph-out-degree g node))
    :weighted-graph (_w_weighted-graph-degree g node)
    :tree
    (let [dg (g :digraph)]
      (+ (_dg_digraph-in-degree dg node) (_dg_digraph-out-degree dg node)))
    # backing weighted graph is symmetric. degree is already the total degree
    :weighted-tree (_w_weighted-graph-degree (g :graph) node)))

(define in-degree
  ```In-degree of a node.
- For directed graphs counts incoming edges
- For undirected graphs, the degree.```
  [g :any-graph node :node-label] :number
  (typecase-strict g
    :multidigraph (_mg_multidigraph-in-degree g node)
    :digraph (_dg_digraph-in-degree g node)
    (fn [x] (degree x node))))

(define out-degree
  ```Out-degree of a node.
- For directed graphs counts outgoing edges
- For undirected graphs, the degree.```
  [g :any-graph node :node-label] :number
  (typecase-strict g
    :multidigraph (_mg_multidigraph-out-degree g node)
    :digraph (_dg_digraph-out-degree g node)
    (fn [x] (degree x node))))


# ==============================================================================
#  Counts and Metrics
# ==============================================================================

(define node-count
  "Number of nodes in the graph"
  [g :any-graph] :integer
  (length (_u_nodes g)))

(define edge-count
  "Number of edges in the graph."
  [g :any-graph] :integer
  (typecase-strict g
    :digraph (_dg_digraph-edge-count g)
    :weighted-graph (_w_weighted-graph-edge-count g)
    :tree (_dg_digraph-edge-count (g :digraph))
    :weighted-tree (_w_weighted-graph-edge-count (g :graph))
    (fn [x] (get x :edge-count))))

(define edge-weight
  "Weight of edge a→b. Returns nil for unweighted graphs."
  [g :any-graph a  b] (:or :number :nil)
  (typecase-strict g
    :digraph (_dg_digraph-edge-weight g a b)
    :weighted-graph (_w_weighted-graph-edge-weight g a b)
    :weighted-tree (_w_weighted-graph-edge-weight (g :graph) a b)))

(define self-loops
  "Return nodes that contain a self-loop."
  [g :any-graph] :array
  (filter (fn [n] (has-edge? g n n)) (nodes g)))

(define density
  ```Graph density is the ratio of actual edges to possible edges.
Undirected: 2E / (N*(N-1)). Directed: E / (N*(N-1)).```
  [g :any-graph] :number
  (let [n (node-count g)]
    (if (<= n 1) 0.0
      (let [e (edge-count g)
            denom (* n (- n 1))]
        (typecase-strict g
          :graph (/ (* 2.0 e) denom)
          :multigraph (/ (* 2.0 e) denom)
          :tree (/ (* 2.0 e) denom)
          :weighted-tree (/ (* 2.0 e) denom)
          :weighted-graph (/ (* 2.0 e) denom)
          (fn [_] (/ (* 1.0 e) denom)))))))

(define degree-sequence
  "Return the degree sequence (sorted descending)."
  [g :any-graph] :array
  (sort (map (fn [n] (degree g n)) (_u_nodes g)) >))

(define average-degree
  "Average degree across all nodes."
  [g :any-graph] :number
  (let [n (node-count g)]
    (if (zero? n) 0
      (/ (sum (map (fn [node] (degree g node)) (nodes g))) n))))


# ==============================================================================
#  Node Data
# ==============================================================================

(define nodes-table
  "Return the table mapping node name -> node data."
  [g :any-graph] :table
  (typecase-strict g
    :weighted-graph (_w_weighted-graph-nodes-table g)
    :digraph (_dg_digraph-nodes-table g)
    :tree (_r_tree-nodes-table g)
    :weighted-tree (_r_weighted-tree-nodes-table g)
    :graph (_sg_graph-nodes-table g)
    :multigraph (_mg_multigraph-nodes-table g)
    :multidigraph (_mg_multidigraph-nodes-table g)))

(define node-data
  "Return the data associated with a node, or nil."
  [g :any-graph node :node-label] :dynamic
  (typecase-strict g
    :weighted-graph (_w_weighted-graph-node-data g node)
    :digraph (_dg_digraph-node-data g node)
    :tree (_r_tree-node-data g node)
    :weighted-tree (_r_weighted-tree-node-data g node)
    :graph (_sg_graph-node-data g node)
    :multigraph (_mg_multigraph-node-data g node)
    :multidigraph (_mg_multidigraph-node-data g node)))

(define set-node-data
  "Associate data with a node label. Adds the node if it does not exist."
  [g :any-graph  node :node-label data :dynamic] :dynamic
  (typecase-strict g
    :weighted-graph (_w_weighted-graph-set-node-data g node data)
    :digraph (_dg_digraph-set-node-data g node data)
    :tree (_r_tree-set-node-data g node data)
    :weighted-tree (_r_weighted-tree-set-node-data g node data)
    :graph (_sg_graph-set-node-data g node data)
    :multigraph (_mg_multigraph-set-node-data g node data)
    :multidigraph (_mg_multidigraph-set-node-data g node data)))


# ==============================================================================
#  Edge Data
# ==============================================================================

(define edge-data
  "Data associated with the edge a->b, or nil. Keyed by the pair of endpoints."
  [g :any-graph a :node-label b :node-label] :dynamic
  (typecase-strict g
    :weighted-graph (_w_weighted-graph-edge-data g a b)
    :digraph (_dg_digraph-edge-data g a b)
    :tree (_dg_digraph-edge-data (g :digraph) a b)
    :weighted-tree (_w_weighted-graph-edge-data (g :graph) a b)
    :graph (_sg_graph-edge-data g a b)
    :multigraph (_mg_multigraph-edge-data g a b)
    :multidigraph (_mg_multidigraph-edge-data g a b)))

(define set-edge-data
  ```Associate data with the edge a->b and return the graph.
The edge is not created if it is missing.```
  [g :any-graph a :node-label b :node-label data :dynamic] :dynamic
  (typecase-strict g
    :weighted-graph (_w_weighted-graph-set-edge-data g a b data)
    :digraph (_dg_digraph-set-edge-data g a b data)
    :tree (_dg_digraph-set-edge-data (g :digraph) a b data)
    :weighted-tree (_w_weighted-graph-set-edge-data (g :graph) a b data)
    :graph (_sg_graph-set-edge-data g a b data)
    :multigraph (_mg_multigraph-set-edge-data g a b data)
    :multidigraph (_mg_multidigraph-set-edge-data g a b data)))

(define edge-data-has?
  "True when data has been stored for the edge a->b."
  [g :any-graph a :node-label b :node-label] :boolean
  (typecase-strict g
    :weighted-graph (_w_weighted-graph-edge-data-has? g a b)
    :digraph (_dg_digraph-edge-data-has? g a b)
    :tree (_dg_digraph-edge-data-has? (g :digraph) a b)
    :weighted-tree (_w_weighted-graph-edge-data-has? (g :graph) a b)
    :graph (_sg_graph-edge-data-has? g a b)
    :multigraph (_mg_multigraph-edge-data-has? g a b)
    :multidigraph (_mg_multidigraph-edge-data-has? g a b)))

(define remove-edge-data
  "Drop any data stored for the edge a->b. Returns the graph."
  [g :any-graph a :node-label b :node-label] :dynamic
  (typecase-strict g
    :weighted-graph (_w_weighted-graph-remove-edge-data g a b)
    :digraph (_dg_digraph-remove-edge-data g a b)
    :tree (_dg_digraph-remove-edge-data (g :digraph) a b)
    :weighted-tree (_w_weighted-graph-remove-edge-data (g :graph) a b)
    :graph (_sg_graph-remove-edge-data g a b)
    :multigraph (_mg_multigraph-remove-edge-data g a b)
    :multidigraph (_mg_multidigraph-remove-edge-data g a b)))

(define edge-data-table
  "The whole edge-data store: a table keyed by (tuple source target)."
  [g :any-graph] :table
  (typecase-strict g
    :weighted-graph (_w_weighted-graph-edge-data-table g)
    :digraph (_dg_digraph-edge-data-table g)
    :tree (_dg_digraph-edge-data-table (g :digraph))
    :weighted-tree (_w_weighted-graph-edge-data-table (g :graph))
    :graph (_sg_graph-edge-data-table g)
    :multigraph (_mg_multigraph-edge-data-table g)
    :multidigraph (_mg_multidigraph-edge-data-table g)))


# ==============================================================================
#  In/Out Neighbours
# ==============================================================================

(define in-neighbours
  "Incoming neighbours of a node. For directed graphs, returns nodes with
   edges pointing to the target. For undirected graphs, same as neighbours."
  [g :any-graph node :node-label] :array
  (_u_in-neighbours g node))

(define out-neighbours
  "Outgoing neighbours (with weights) of a node. For directed graphs,
   returns [neighbour weight] pairs for outgoing edges."
  [g :any-graph node :node-label] :array
  (_u_out-neighbours g node))


# ==============================================================================
#  Structural edges
# ==============================================================================

(define structural-edges
 ```Distinct structural edges of `g` as an array of `[u v]` pairs.

Parallel arcs between the same pair are counted as one edge as is the edge count
for :multidigraph.```
  [g :any-graph] :array
  (_u_structural-edges g))

(define structural-degree
  ```Count of distinct edges incident to `node`, self-loops excluded.

Distinct out-edges (plus distinct in-edges for a directed graph).
Differs from `degree` and `in-degree`, a self-loop contributes nothing, and
parallel arcs are counted once each side rather than summed.```
  [g :any-graph node :node-label] :integer
  (_u_structural-degree g node))


# ==============================================================================
#  Traversal
# ==============================================================================

(define bfs
  "BFS traversal from `start`. Returns nodes in BFS order."
  [g :any-graph start (or :string :integer)] :array
  (_t_graph-bfs g start))

(define dfs
  "DFS traversal from `start`. Returns nodes in DFS order."
  [g :any-graph start (or :string :integer)] :array
  (_t_graph-dfs g start))


# ==============================================================================
#  Connectivity
# ==============================================================================

(define connected?
  "True when the graph is connected (single weakly-connected component)."
  [g :any-graph] :boolean
  (_c_graph-connected? g))

(define connected-components
  "Connected components (weak connectivity for directed graphs)."
  [g :any-graph] :array
  (_c_connected-components g))


# ==============================================================================
#  Cycle Detection
# ==============================================================================

(define has-cycle?
  "True when the graph contains a cycle."
  [g :any-graph] :boolean
  (typecase-strict g
    :graph (_cy_graph-has-cycle? g)
    :multigraph (_cy_graph-has-cycle? g)
    :multidigraph (_cy_directed-has-cycle? g)
    :digraph (_cy_directed-has-cycle? g)
    :weighted-graph (_cy_weighted-graph-has-cycle? g)
    (fn [_] false)))


# ==============================================================================
#  Topological Sort
# ==============================================================================

(define topological-sort
  "Topological sort (Kahn's algorithm). Returns array of nodes or nil if cyclic."
  [g :any-graph] [:or :array :nil]
   (typecase-strict g
     :multidigraph (_mg_multidigraph-topo-sort g)
     (fn [g] (_t_topological-sort g))))


# ==============================================================================
#  (Re)exported Algorithm Modules
# ==============================================================================

# Traversal
(def bfs-path _t_bfs-path)
(def dfs-all _t_dfs-all)
(def digraph-bfs _t_digraph-bfs)
(def digraph-dfs _t_digraph-dfs)
(def multidigraph-bfs _t_multidigraph-bfs)
(def multidigraph-dfs _t_multidigraph-dfs)
(def kosaraju-scc _t_kosaraju-scc)
(def semi-connected? _t_semi-connected?)
(def eulerian-cycle _t_eulerian-cycle)
(def eulerian-tour _t_eulerian-tour)
(def dag-shortest-path _t_dag-shortest-path)
(def dag-shortest-path-tree _t_dag-shortest-path-tree)
(def hamiltonian-path-dag _t_hamiltonian-path-dag)
(def shortest-cycle-through-edge _t_shortest-cycle-through-edge)

# Connectivity
(def articulation-points _c_articulation-points)
(def bridges _c_bridges)
(def biconnected-components _c_biconnected-components)
(def graph-path-exists? _c_graph-path-exists?)
(def graph-shortest-path _c_graph-shortest-path)
(def multidigraph-connected? _c_multidigraph-connected?)
(def multidigraph-connected-components _c_multidigraph-connected-components)
(def multidigraph-path-exists? _c_multidigraph-path-exists?)
(def multidigraph-shortest-path _c_multidigraph-shortest-path)
(def edge-connectivity _c_edge-connectivity)
(def vertex-connectivity _c_vertex-connectivity)

# Shortest paths
(def graph-shortest-path-lengths _sp_graph-shortest-path-lengths)
(def graph-diameter _sp_graph-diameter)
(def graph-eccentricity _sp_graph-eccentricity)
(def graph-center _sp_graph-center)
(def graph-periphery _sp_graph-periphery)

# Cycles
(def graph-cycle-basis _cy_graph-cycle-basis)
(def graph-k-cycle-count _cy_graph-k-cycle-count)
(def multidigraph-simple-cycles _cy_multidigraph-simple-cycles)

# Metrics
(def graph-density _m_graph-density)
(def graph-diameter-metrics _m_graph-diameter)
(def graph-radius _m_graph-radius)
(def graph-center-metrics _m_graph-center)
(def clustering-coefficient _m_clustering-coefficient)
(def average-clustering-coefficient _m_average-clustering-coefficient)
(def average-path-length _m_average-path-length)
(def eccentricity _m_eccentricity)
(def global-clustering-coefficient _m_global-clustering-coefficient)
(def graph-transitivity _m_graph-transitivity)
(def graph-average-clustering _m_graph-average-clustering)
# Cycle and independence invariants. All read the undirected simple structure.
(def girth _m_girth)
(def shortest-cycle _m_shortest-cycle)
(def triangle-count _m_triangle-count)
(def bipartite? _m_bipartite?)
(def cubic? _m_cubic?)
(def maximum-independent-set _m_maximum-independent-set)
(def independence-number _m_independence-number)
(def minimum-vertex-cover _m_minimum-vertex-cover)
(def vertex-cover-number _m_vertex-cover-number)
(def spanning-tree-count _m_spanning-tree-count)
(def toughness _m_toughness)
# Export
(def to-dot _e_to-dot)
(def to-gexf _e_to-gexf)
(def to-cytoscape _e_to-cytoscape)
(def petri-net-to-dot _e_petri-net-to-dot)
(def petri-net-to-tikz _e_petri-net-to-tikz)
(def petri-net-state-space-to-dot _e_petri-net-state-space-to-dot)
(def to-tikz _e_to-tikz)
(def to-svg _e_to-svg)
(def multidigraph-average-clustering _m_multidigraph-average-clustering)

# PageRank
(def pagerank _p_pagerank)

# Random graphs
(def random-graph _rg_random-graph)
(def random-graph-edges _rg_random-graph-edges)
(def barabasi-albert _rg_barabasi-albert)

# Centrality
(def degree-centrality _c2_degree-centrality)
(def degree-out-centrality _c2_degree-out-centrality)
(def degree-in-centrality _c2_degree-in-centrality)
(def closeness-centrality _c2_closeness-centrality)
(def closeness-centrality-all _c2_closeness-centrality-all)
(def betweenness-centrality _c2_betweenness-centrality)
(def multidigraph-degree-centrality _c2_multidigraph-degree-centrality)
(def multidigraph-in-degree-centrality _c2_multidigraph-in-degree-centrality)
(def eigenvector-centrality _c2_eigenvector-centrality)
(def katz-centrality _c2_katz-centrality)
(def harmonic-centrality _c2_harmonic-centrality)
(def percolation-centrality _c2_percolation-centrality)

# Clustering / Community detection
(def louvain _c3_louvain)
(def leiden _c3_leiden)
(def aggregate _c3_aggregate)
(def total-weight _c3_total-weight)

# LCA
(def tarjan-lca _l_tarjan-lca)
(def tarjan-lca-iterative _l_tarjan-lca-iterative)

# Similarity
(def jaccard-similarity _s_jaccard-similarity)
(def cosine-similarity _s_cosine-similarity)
(def knn _s_knn)

# Flow
(def edmonds-karp _f_edmonds-karp)
(def dinic _f_dinic)
(def min-cut _f_min-cut)

# Hamiltonian paths & cycles
(def hamiltonian-cycle _hm_hamiltonian-cycle)
(def hamiltonian-path _hm_hamiltonian-path)
(def hamiltonian-possible? _hm_hamiltonian-possible?)
(def hamiltonian-dirac-sufficient? _hm_hamiltonian-dirac-sufficient?)

# DAG & causal inference
(def topological-generations _dag_topological-generations)
(def is-acyclic? _dag_is-acyclic?)
(def is-aperiodic? _dag_is-aperiodic?)
(def source-nodes _dag_source-nodes)
(def sink-nodes _dag_sink-nodes)
(def feedback-arc-set _dag_feedback-arc-set)
(def dagify _dag_dagify)
(def ancestors _dag_ancestors)
(def descendants _dag_descendants)
(def transitive-closure _dag_transitive-closure)
(def transitive-reduction _dag_transitive-reduction)
(def dag-longest-path _dag_dag-longest-path)
(def dag-longest-path-length _dag_dag-longest-path-length)

# TALA hierarchical layout
(def layout-dag _gd_layout-dag)
(def antichains _dag_antichains)
(def colliders _dag_colliders)
(def v-structures _dag_v-structures)

# General graph drawing layouts
(def layout-force _gd_layout-force)
(def layout-circle _gd_layout-circle)
(def layout-tree _gd_layout-tree)
(def layout-radial _gd_layout-radial)
(def layout-orthogonal _gd_layout-orthogonal)
(def layout-spectral _gd_layout-spectral)
(def layout-spring _gd_layout-spring)
(def layout-polyarc _gd_layout-polyarc)
(def layout-lombardi _gd_layout-lombardi)
(def layout-phylogenetic _gd_layout-phylogenetic)
(def layout-phylogram _gd_layout-phylogram)
(def layout-phylogenetic-tree _gd_layout-phylogenetic-tree)
(def layout-unrooted-phylogram _gd_layout-unrooted-phylogram)
(def layout-unrooted-phylogenetic-tree _gd_layout-unrooted-phylogenetic-tree)
(def graph-layout _gd_graph-layout)

# Matching & cliques
(def is-matching? _m4_is-matching?)
(def is-maximal-matching? _m4_is-maximal-matching?)
(def is-perfect-matching? _m4_is-perfect-matching?)
(def maximal-matching _m4_maximal-matching)
(def find-cliques _m4_find-cliques)
(def find-all-cliques _m4_find-all-cliques)
(def node-clique-number _m4_node-clique-number)
(def number-of-cliques _m4_number-of-cliques)

# Cuts & expansion
(def volume _cu_volume)
(def cut-size _cu_cut-size)
(def edge-boundary _cu_edge-boundary)
(def node-boundary _cu_node-boundary)
(def edge-expansion _cu_edge-expansion)
(def node-expansion _cu_node-expansion)
(def mixing-expansion _cu_mixing-expansion)
(def normalized-cut-size _cu_normalized-cut-size)
(def conductance _cu_conductance)
(def boundary-expansion _cu_boundary-expansion)

# Degree sequence realizability
(def is-graphical? _sq_is-graphical?)
(def is-digraphical? _sq_is-digraphical?)
(def is-multigraphical? _sq_is-multigraphical?)
(def is-pseudographical? _sq_is-pseudographical?)
(def is-valid-havel-hakimi? _sq_is-valid-havel-hakimi?)
(def is-valid-erdos-gallai? _sq_is-valid-erdos-gallai?)

# Bipartite matching & Hall's condition
(def bipartite-sets _m4_bipartite-sets)
(def maximum-bipartite-matching _m4_maximum-bipartite-matching)
(def bipartite-perfect-matching? _m4_bipartite-perfect-matching?)
(def satisfies-hall-condition? _m4_satisfies-hall-condition?)
(def minimum-vertex-cover _m4_minimum-vertex-cover)
(def maximum-independent-set _m4_maximum-independent-set)

# General (non-bipartite) maximum matching - Edmonds blossom
(def maximum-matching _m4_maximum-matching)
(def is-perfect-matching-graph? _m4_is-perfect-matching-graph?)

# Planarity (DMP algorithm, Demoucron-Malgrange-Pertuisot)
(def is-planar? _pl_is-planar?)
(def planar-faces _pl_planar-faces)

# Coloring & Turán
(def greedy-colour _col_greedy-colour)
(def proper-colouring? _col_proper-colouring?)
(def proper-edge-colour? _col_proper-edge-colour?)
(def chromatic-number _col_chromatic-number)
(def vertex-colouring _col_vertex-colouring)
(def make-turan-graph _col_make-turan-graph)
(def turan-number _col_turan-number)

# Edge colouring (Vizing)
(def edge-colour-greedy _col_edge-colour-greedy)
(def vizing-edge-colour _col_vizing-edge-colour)
(def chromatic-index _col_chromatic-index)

# Spanning trees & Eulerian circuits (Matrix-Tree, BEST)
(def matrix-determinant _spt_matrix-determinant)
(def graph-laplacian _spt_graph-laplacian)
(def count-spanning-trees _spt_count-spanning-trees)
(def count-eulerian-circuits _spt_count-eulerian-circuits)

# Tournaments
(def is-tournament? _tour_is-tournament?)
(def tournament-matrix _tour_tournament-matrix)
(def tournament-score-sequence _tour_tournament-score-sequence)
(def valid-tournament-score-sequence? _tour_valid-tournament-score-sequence?)
(def tournament-hamiltonian-path _tour_tournament-hamiltonian-path)
(def is-tournament-king? _tour_is-tournament-king?)
(def tournament-kings _tour_tournament-kings)
(def strong-tournament? _tour_strong-tournament?)
(def regular-tournament? _tour_regular-tournament?)
(def transitive-tournament? _tour_transitive-tournament?)

# Cores
(def core-number _co_core-number)
(def k-core _co_k-core)
(def k-shell _co_k-shell)
(def k-crust _co_k-crust)
(def k-corona _co_k-corona)
(def onion-layers _co_onion-layers)


# ==============================================================================
#  Constructors (type-specific)
# ==============================================================================

# Queue

# Heap
(def make-binary-min-heap _h_make-binary-min-heap)
(def heap-push _h_heap-push)
(def heap-pop _h_heap-pop)
(def heap-peek _h_heap-peek)
(def heap-size _h_heap-size)

# Distance matrix
(def make-distance-matrix _dm_make-distance-matrix)
(def distance-matrix-size _dm_distance-matrix-size)
(def distance-matrix-get _dm_distance-matrix-get)
(def distance-matrix-set _dm_distance-matrix-set)
(def distance-matrix-names _dm_distance-matrix-names)
(def distance-matrix-nearest-pair _dm_distance-matrix-nearest-pair)
(def distance-matrix-remove _dm_distance-matrix-remove)
(def distance-matrix-merge _dm_distance-matrix-merge)
(def distance-matrix-from-fn _dm_distance-matrix-from-fn)
(def distance-matrix-map _dm_distance-matrix-map)
(def distance-matrix-to-table _dm_distance-matrix-to-table)

# Simple graph
(def make-graph _sg_make-graph)
(def graph-adj _sg_graph-adj)
(def graph-nodes _sg_graph-nodes)
(def graph-nodes-table _sg_graph-nodes-table)
(def graph-node-data _sg_graph-node-data)
(def graph-set-node-data _sg_graph-set-node-data)
(def graph-degree _sg_graph-degree)

# Multi(di)graph
(def make-multigraph _mg_make-multigraph)
(def make-multidigraph _mg_make-multidigraph)
(def multigraph-nodes-table _mg_multigraph-nodes-table)
(def multigraph-node-data _mg_multigraph-node-data)
(def multigraph-set-node-data _mg_multigraph-set-node-data)
(def multidigraph-nodes-table _mg_multidigraph-nodes-table)
(def multidigraph-node-data _mg_multidigraph-node-data)
(def multidigraph-set-node-data _mg_multidigraph-set-node-data)
(def multigraph-edge-count-between _mg_multigraph-edge-count-between)
(def multidigraph-edge-count-between _mg_multidigraph-edge-count-between)
# generic remove-edge -> *-remove-all-edges
(def multigraph-remove-edge _mg_multigraph-remove-edge)
(def multidigraph-remove-edge _mg_multidigraph-remove-edge)

# Digraph
(def make-digraph _dg_make-digraph)
(def digraph-add-node _dg_digraph-add-node)
(def digraph-add-edge _dg_digraph-add-edge)
(def digraph-remove-edge _dg_digraph-remove-edge)
(def digraph-remove-node _dg_digraph-remove-node)
(def digraph-out-neighbours _dg_digraph-out-neighbours)
(def digraph-in-neighbours _dg_digraph-in-neighbours)
(def digraph-out-degree _dg_digraph-out-degree)
(def digraph-in-degree _dg_digraph-in-degree)
(def digraph-edge-weight _dg_digraph-edge-weight)
(def digraph-has-edge? _dg_digraph-has-edge?)
(def digraph-nodes _dg_digraph-nodes)
(def digraph-nodes-table _dg_digraph-nodes-table)
(def digraph-node-data _dg_digraph-node-data)
(def digraph-set-node-data _dg_digraph-set-node-data)
(def digraph-reverse _dg_digraph-reverse)
(def digraph-node-count _dg_digraph-node-count)
(def digraph-edge-count _dg_digraph-edge-count)

# Weighted graph
(def make-weighted-graph _w_make-weighted-graph)
(def weighted-graph-add-node _w_weighted-graph-add-node)
(def weighted-graph-add-edge _w_weighted-graph-add-edge)
(def weighted-graph-remove-edge _w_weighted-graph-remove-edge)
(def weighted-graph-remove-node _w_weighted-graph-remove-node)
(def weighted-graph-neighbours _w_weighted-graph-neighbours)
(def weighted-graph-neighbour-names _w_weighted-graph-neighbour-names)
(def weighted-graph-degree _w_weighted-graph-degree)
(def weighted-graph-nodes _w_weighted-graph-nodes)
(def weighted-graph-nodes-table _w_weighted-graph-nodes-table)
(def weighted-graph-node-data _w_weighted-graph-node-data)
(def weighted-graph-set-node-data _w_weighted-graph-set-node-data)
(def weighted-graph-edge-weight _w_weighted-graph-edge-weight)
(def weighted-graph-node-count _w_weighted-graph-node-count)
(def weighted-graph-edge-count _w_weighted-graph-edge-count)
(def weighted-graph->distance-matrix _w_weighted-graph->distance-matrix)
(def weighted-graph-mst _w_weighted-graph-mst)
(def weighted-graph-shortest-path _w_weighted-graph-shortest-path)
(def weighted-graph-all-shortest-paths _w_weighted-graph-all-shortest-paths)

# Tree
(def make-tree _r_make-tree)
(def tree-root _r_tree-root)
(def tree-parent _r_tree-parent)
(def tree->digraph _r_tree->digraph)

# Weighted tree
(def make-weighted-tree _r_make-weighted-tree)
(def weighted-tree-root _r_weighted-tree-root)
(def weighted-tree-parent _r_weighted-tree-parent)
(def weighted-tree->graph _r_weighted-tree->graph)
(def weighted-tree-nodes _r_weighted-tree-nodes)
(def weighted-tree-nodes-table _r_weighted-tree-nodes-table)
(def weighted-tree-node-data _r_weighted-tree-node-data)
(def weighted-tree-set-node-data _r_weighted-tree-set-node-data)
(def weighted-tree-edge-weight _r_weighted-tree-edge-weight)
(def weighted-tree-node-count _r_weighted-tree-node-count)
(def weighted-tree-edge-count _r_weighted-tree-edge-count)
