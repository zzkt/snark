# -*- mode: janet; -*-
# graphs/simple-graph: undirected simple graph with self loops

# The :graph type based on adjacency-list representation.
# Self-loops are supported, and contribute +2 to degree.

(import deft :prefix "")
(import ./record :prefix "pr-")


# ==============================================================================
#  Graph — Undirected graph (with, or without self loops)
# ==============================================================================

(deftrecord :graph
  ```Undirected simple graph (with or without self loops).

The adjacency list `adj` maps each node to a table of its neighbours.
Node labels are kept in the ordered array `nodes`, and `node-data` maps each
label to any data (keyed by node label).
```
  (field adj :table @{})
  (field nodes :array @[])
  (field node-data :table @{})
  (field seen :table @{})
  (field edge-data :table @{})
  (field node-count :number 0)
  (field edge-count :number 0))

(def _make-graph-raw make-graph)

(defn make-graph
  "Construct an undirected graph."
  [& args]
  (def g (apply _make-graph-raw args))
  (when (and (empty? (pr-nodes g)) (not (empty? (g :adj))))
    (set (g :nodes) (array ;(sort (keys (g :adj))))))
  (set (g :node-count) (length (pr-nodes g)))
  (set (g :edge-count) (pr-undirected-edge-count-underlying g))
  g)

(define graph-add-node
  "Add a node to the graph."
  [g :graph node :node-label] :graph
  (pr-add-node g node))

(define graph-add-edge
  ```Add an undirected edge between a and b. Self-loops are permitted.

Symmetry is maintained by adding both adj[a][b] and adj[b][a].```
  [g :graph a :node-label b :node-label] :graph
  (graph-add-node g a)
  (graph-add-node g b)
  (when (nil? (in (in (g :adj) a) b))
    (put (in (g :adj) a) b true)
    (put (in (g :adj) b) a true)
    (set (g :edge-count) (+ (pr-edge-count-field g) 1)))
  g)

(define graph-remove-edge
  "Remove an edge between a and b. No-op if the edge does not exist."
  [g :graph a :node-label b :node-label] :graph
  (let [adj-a (in (g :adj) a)]
    (when (and adj-a (not= nil (in adj-a b)))
      (put adj-a b nil)
      (def adj-b (in (g :adj) b))
      (when adj-b (put adj-b a nil))
      (pr-remove-edge-data g false a b)
      (set (g :edge-count) (- (pr-edge-count-field g) 1))))
  g)

(define graph-remove-node
  "Remove a node and all its incident edges."
  [g :graph node :node-label] :graph
  (pr-remove-node g node))

(define graph-has-node?
  "True when node is in the graph."
  [g :graph node :node-label] :boolean
  (pr-has-node? g node))

(define graph-has-edge?
  "True when edge a-b exists."
  [g :graph a :node-label b :node-label] :boolean
  (let [adj-a (in (g :adj) a)]
    (if adj-a (not= nil (in adj-a b)) false)))

(define graph-neighbours
  "Return the neighbours of a node."
  [g :graph node :node-label] :array
  (pr-neighbours g node))

(define graph-nodes-table
  "Return the table mapping node name -> node data."
  [g :graph] :table
  (pr-nodes-table g))

(define graph-node-data
  "Return the data associated with a node, or nil if the node is absent."
  [g :graph node :node-label] :dynamic
  (pr-node-data g node))

(define graph-set-node-data
  "Associate a node with data. Adds the node if it does not yet exist."
  [g :graph node :node-label data :dynamic] :graph
  (pr-set-node-data g node data))

(define graph-degree
  "Return the degree of a node. Self-loops contribute +2."
  [g :graph node :node-label] :number
  (let [adj-node (in (g :adj) node)]
    (if (nil? adj-node) 0
      (let [base (length adj-node)]
        (if (not= nil (in adj-node node))
          (+ base 1) base)))))

(define graph-in-degree
  "In-degree of `node` in `g` (equal to degree for undirected graphs)."
  [g :graph node :node-label] :number
  (graph-degree g node))

(define graph-out-degree
  "Out-degree of `node` in `g` (equal to degree for undirected graphs)."
  [g :graph node :node-label] :number
  (graph-degree g node))

(define graph-self-loops
  "Return nodes in graph `g` that have a self-loop."
  [g :graph] :array
  (pr-self-loops g))

(define graph-density
  "Graph density. Undirected, 2E/(N*(N-1))."
  [g :graph] :number
  (pr-density g))

(define graph-edge-data
  "Returns data associated with the edge a->b, or nil.
Undirected lookups are order insensitive."
  [g :graph a :node-label b :node-label] :dynamic
  (pr-edge-data g false a b))

(define graph-set-edge-data
  "Associate data with the edge a->b. The edge is not created if absent.

  Returns the graph."
  [g :graph a :node-label b :node-label data :dynamic] :graph
  (pr-set-edge-data g false a b data))

(define graph-edge-data-has?
  "True when data is available for the edge a->b."
  [g :graph a :node-label b :node-label] :boolean
  (pr-edge-data-has? g false a b))

(define graph-remove-edge-data
  "Remove any data for the edge a->b."
  [g :graph a :node-label b :node-label] :graph
  (pr-remove-edge-data g false a b))

(define graph-edge-data-table
  "The whole edge-data store, keyed by (tuple source target)."
  [g :graph] :table
  (pr-edge-data-table g))
