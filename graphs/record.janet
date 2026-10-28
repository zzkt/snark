# -*- mode: janet; -*-
# graphs/record: shared operations for the adjacency-list graph family
#                :graph, :digraph, :weighted-graph, :multigraph and :petri-net

#  Each graph type shares the same field layout {nodes array, adj table,
#  node-data table, edge-data table, node-count, edge-count} where `adj` maps
#  a node to its neighbours; weights for :digraph and :weighted-graph, booleans
#  for :graph, multiplicities for :multigraph.
#
#  `nodes` is the ordered array of node labels and `node-data` is a store keyed
#  by label. `edge-data` is keyed by the pair of endpoints.
#
#  Keep this file as a leaf module (no imports, untyped) to avoid import cycles.

(defn add-node
  ```Add a `node` to a graph `g`. No-op if node is already present.

`seen` is a membership set of `nodes`, node existence test is a hash lookup.
The only functions that should change `nodes` are `add-node` and `remove-node`.
Anything that directly accesses `(g :nodes)` must register in `(g :seen)` for
consistency.

Returns the graph.
```
  [g node]
  (def seen (g :seen))
  (when (and (empty? seen) (not (empty? (g :nodes))))
    (each n (g :nodes) (put seen n true)))
  (unless (in seen node)
    (array/push (g :nodes) node)
    (put seen node true)
    (put (g :node-data) node node)
    (set (g :node-count) (+ (get g :node-count) 1)))
  (unless (get (g :adj) node)
    (put (g :adj) node @{}))
  g)


# ==============================================================================
#  Edge Data
#
#  node-data is keyed by node label. edge-data is keyed by the pair of endpoints.
#
#  Directed edges have intrinsic order, (a b) and (b a) are keyed differently.
#  An undirected edge is not ordered, so the key is canonicalised to (min max)
#  and the accessor should normalise lookup.
#
#  Parallel edges of a :multigraph can't be keyed the same way, the adjacency
#  contains a single count per pair, so all edges share one key.
# ==============================================================================

(defn- tree-children
  "The children list of a :tree or :weighted-tree from its :parent table."
  [g]
  (or (get g :children-cache)
      (let [parent (get g :parent)
            root (get g :root)
            adj @{}
            backing (get g :graph)]
        (each child (keys parent) (put adj child @{}))
        (eachp [_ par] parent (unless (get adj par) (put adj par @{})))
        (eachp [child par] parent
          (put (get adj par) child
               (or (when backing (get (get (backing :adj) par) child)) true)))
        (unless (get adj root) (put adj root @{}))
        (set (g :children-cache) adj)
        adj)))

(defn- tree-parents
  "The parent list of a tree, where ach node's in-neighbour is a parent."
  [g]
  (or (get g :parents-cache)
      (let [parent (get g :parent)
            backing (get g :graph)
            rev @{}]
        (each child (keys parent) (put rev child @{}))
        (eachp [child par] parent
          (put (get rev child) par
               (or (when backing (get (get (backing :adj) par) child)) true)))
        (set (g :parents-cache) rev)
        rev)))

(defn adjacency
  ```The out-adjacency table of a graph `g`.

Uses :adj for most types, :out for :multidigraph, or the derived children map
for trees.
```
  [g]
  (or (get g :adj)
      (get g :out)
      (when (get g :parent) (tree-children g))))

(defn in-adjacency
  ```The in-adjacency table of a graph `g`.

Uses :adj for most types, :out for :multidigraph, or the derived parent map
for trees.
```
  [g]
  (or (get g :in)
      (when (get g :parent) (tree-parents g))
      (adjacency g)))

(defn edge-key
  "The key with which the data for edge a-b is stored.

Undirected keys are canonical (min max), so one edge is one entry.
A self-loop can be keyed as (a a)."
  [directed? a b]
  (cond
    directed? (tuple a b)
    (< a b) (tuple a b)
    (< b a) (tuple b a)
    (tuple a a)))

(defn has-edge?
  "True when the edge a-b is present in graph `g`."
  [g directed? a b]
  (not= nil (get (get (adjacency g) a) b)))

(defn edge-data
  "Data associated with the edge a-b, or nil."
  [g directed? a b]
  (get (g :edge-data) (edge-key directed? a b)))

(defn set-edge-data
  ```Associate `data` with the edge a-b in graph `g`.

The edge must exist before any data is added, it's not created if it doesn't.
```
  [g directed? a b data]
  (when (has-edge? g directed? a b)
    (put (g :edge-data) (edge-key directed? a b) data))
  g)

(defn remove-edge-data
  "Drop any data stored for the edge a-b in graph `g`."
  [g directed? a b]
  (put (g :edge-data) (edge-key directed? a b) nil)
  g)

(defn edge-data-has?
  "True when data has been stored for the edge a-b."
  [g directed? a b]
  (not= nil (get (g :edge-data) (edge-key directed? a b))))

(defn edge-data-table
  "The whole edge-data store: a table keyed by (tuple source target).
   Undirected keys are canonical (min max)."
  [g] (g :edge-data))


(defn remove-node
  ```Remove `node` and all its incident edges from a graph `g`.

Fields adj, nodes, node-data, node-count, and edge-count are updated.

`edges-per-entry` maps an adjacency value to the number of edges it stands for
in a multigraph (not uniform across graph types.

`directed?` counts in-arcs held in related node tables, self-loops counted once.
It should remain unset for symmetric undirected types.

Returns the graph.```
  [g node &opt edges-per-entry directed?]
  (def per-entry (or edges-per-entry (fn [v] (if (number? v) v 1))))
  (def adj (g :adj))
  (def neighbours (get adj node))
  (when neighbours
    (each [_ v] (pairs neighbours)
      (set (g :edge-count) (- (get g :edge-count) (per-entry v))))
    (put adj node nil))
  (loop [[other tbl] :pairs adj :when (not= nil (get tbl node))]
    (when (and directed? (not= other node))
      (set (g :edge-count) (dec (g :edge-count))))
    (put tbl node nil))
  (def nodes (g :nodes))
  (def idx (find-index (fn [n] (= n node)) nodes))
  (when idx
    (array/remove nodes idx)
    (set (g :node-count) (- (get g :node-count) 1)))
  (put (g :seen) node nil)
  (put (g :node-data) node nil)
  # drop the data of every incident edge
  (def dir? (if directed? true false))
  (loop [[other _] :pairs adj]
    (when (not= other node)
      (remove-edge-data g dir? other node)))
  (when (and dir? neighbours)
    (loop [[other _] :pairs neighbours]
      (remove-edge-data g true node other)))
  g)

(defn has-node?
  "True when `node` is in the adjacency table of graph `g`."
  [g node] (not= nil (get (adjacency g) node)))

(defn neighbours
  "Return the neighbours of `node` as an array of its adj keys."
  [g node]
  (let [adj-node (get (adjacency g) node)]
    (if adj-node (keys adj-node) @[])))

(defn nodes
  "Return all nodes of graph `g` as array of labels."
  [g]
  (or (get g :nodes)
      (when (get g :parent)
        (let [dg (or (get g :digraph) (get g :graph))]
          (when dg (get dg :nodes))))))

(defn nodes-table
  "Return the table mapping node name -> node data."
  [g] (g :node-data))

(defn node-count
  ```Number of nodes in a graph.
Computed from the label array, not the :node-count field.```
  [g] (length (nodes g)))

(defn node-data
  "Return the data associated with a node, or nil when absent."
  [g node] (get (g :node-data) node))

(defn set-node-data
  ```Store `data` in graph `g` and associate it with `node`.
Adds the node if it does not exist. Returns the graph.```
  [g node data]
  (add-node g node)
  (put (g :node-data) node data)
  g)

(defn self-loops
  "Return nodes that have a self-loop."
  [g]
  (let [result @[]]
    (loop [[node tbl] :pairs (adjacency g)]
      (when (get tbl node)
        (array/push result node)))
    result))

(defn edge-count-field
  "The stored :edge-count, or computed from the adjacency when the record has none."
  [g]
  (or (get g :edge-count)
      (sum (seq [[_ adj] :pairs (adjacency g)] (length adj)))))

(defn edge-weight
  "Weight of the edge a→b, or nil if absent."
  [g a b] (get (get (adjacency g) a) b))

(defn degree
  "Number of edges incident to node `name`."
  [g name]
  (def adj (get (adjacency g) name))
  (if adj (length adj) 0))

(defn edge-count-sum
  "Raw sum of adjacency sizes.
Directed count for :digraph, twice the undirected count for :weighted-graph."
  [g] (sum (seq [[_ adj] :pairs (adjacency g)] (length adj))))

(defn undirected-edge-count-underlying
  "Number of undirected edges implied by a symmetric boolean adjacency table.

Each non-loop edge appears in two adjacency lists, each self-loop in one.

Trees are an exception, the derived map is directed (parent -> child)
rather than symmetric, so every edge is listed exactly once."
  [g]
  (if (get g :parent)
    (sum (seq [[_ adj] :pairs (adjacency g)] (length adj)))
    (do
      (var total 0)
      (var self 0)
      (loop [[node tbl] :pairs (adjacency g)]
        (set total (+ total (length tbl)))
        (when (get tbl node) (set self (+ self 1))))
      (math/trunc (/ (+ total self) 2)))))

# move -> metrics
(defn density
  "Undirected density. 2E/(N*(N-1)).
Node count is taken from the label array, edge count from the :edge-count
field, falling back to the adjacency for the tree types without a counter."
  [g]
  (let [n (length (nodes g))]
    (if (<= n 1) 0
      (/ (* 2.0 (edge-count-field g))
         (* n (- n 1))))))
