# -*- mode: janet; -*-
# graphs/multigraph: directed and undirected multigraphs

# :multigraph — undirected multigraph with parallel edges and self loops.
# :multidigraph — directed multigraph with parallel edges and self loops.
#
# Performance: O(1) neighbour lookup via table keys, and node/edge counters.

(import deft :prefix "")
(import ./record :prefix "pr-")

# ==============================================================================
#  Multigraph — Undirected multigraph with self loops
# ==============================================================================

(deftrecord :multigraph
 ``` Undirected multigraph.
- adjacency list maps node → {neighbour → count}
- `nodes` is the ordered array of node labels
- `node-data` maps each node label to that node's data.
```
  (field adj :table @{})
  (field nodes :array @[])
  (field node-data :table @{})
  (field seen :table @{})
  (field edge-data :table @{})
  (field node-count :number 0)
  (field edge-count :number 0))

(define multigraph-add-node
  "Add a node to the multigraph."
  [g :multigraph node :node-label] :multigraph
  (pr-add-node g node))

(define multigraph-add-edge
  "Add an edge. Increments multiplicity for parallel edges."
  [g :multigraph a :node-label b :node-label] :multigraph
  (multigraph-add-node g a)
  (multigraph-add-node g b)
  (let [adj-a (in (multigraph-adj g) a)
        adj-b (in (multigraph-adj g) b)
        current (or (in adj-a b) 0)]
    (put adj-a b (+ current 1))
    (put adj-b a (+ current 1))
    (set-multigraph-edge-count g (+ (multigraph-edge-count g) 1)))
  g)

(define multigraph-remove-edge
  "Remove one edge between a and b. Decrements multiplicity."
  [g :multigraph a :node-label b :node-label] :multigraph
  (let [adj-a (in (multigraph-adj g) a)
        adj-b (in (multigraph-adj g) b)]
    (when (and adj-a (not= nil (in adj-a b)) (> (in adj-a b) 0))
      (let [new-val (- (in adj-a b) 1)]
        (if (= new-val 0)
          (do (put adj-a b nil) (put adj-b a nil)
              (pr-remove-edge-data g false a b))
          (do (put adj-a b new-val) (put adj-b a new-val)))
        (set-multigraph-edge-count g (- (multigraph-edge-count g) 1))))
    g))

(define multigraph-remove-all-edges
  "Remove all edges between a and b."
  [g :multigraph a :node-label b :node-label] :multigraph
  (let [adj-a (in (multigraph-adj g) a)
        adj-b (in (multigraph-adj g) b)]
    (when (and adj-a (not= nil (in adj-a b)))
      (let [count (in adj-a b)]
        (put adj-a b nil)
        (put adj-b a nil)
        (set-multigraph-edge-count g (- (multigraph-edge-count g) count))))
    g))

(define multigraph-remove-node
  "Remove a node and all its incident edges."
  [g :multigraph node :node-label] :multigraph
  (pr-remove-node g node))

(define multigraph-has-node?
  "True when node is in the multigraph."
  [g :multigraph node :node-label] :boolean
  (pr-has-node? g node))

(define multigraph-has-edge?
  "True when at least one edge exists between a and b."
  [g :multigraph a :node-label b :node-label] :boolean
  (let [adj-a (in (multigraph-adj g) a)]
    (if adj-a (and (not= nil (in adj-a b)) (> (in adj-a b) 0)) false)))

(define multigraph-edge-count-between
  "Number of edges between a and b."
  [g :multigraph a :node-label b :node-label] :number
  (let [adj-a (in (multigraph-adj g) a)]
    (if (and adj-a (not= nil (in adj-a b))) (in adj-a b) 0)))

(define multigraph-neighbours
  "Return unique neighbours of a node."
  [g :multigraph node :node-label] :array
  (pr-neighbours g node))

(define multigraph-nodes
  "Return all nodes."
  [g :multigraph] :array
  (pr-nodes g))

(define multigraph-node-data
  "Return node data for `node`, or nil when absent."
  [g :multigraph node :node-label] :dynamic
  (pr-node-data g node))
(define multigraph-nodes-table
  "Return the table mapping node name -> node data."
  [g :multigraph] :table
  (pr-nodes-table g))

(define multigraph-set-node-data
  "Associate a node with data. Adds the node if it does not yet exist."
  [g :multigraph node :node-label data :dynamic] :multigraph
  (pr-set-node-data g node data))

(define multigraph-degree
  "Degree of a node (sum of edge multiplicities). Self-loops count double."
  [g :multigraph node :node-label] :number
  (def adj (in (multigraph-adj g) node))
  (if (nil? adj) 0
    (sum (seq [[n c] :pairs adj] (if (= n node) (* c 2) c)))))

(define multigraph-self-loops
  "Return nodes that have self-loops."
  [g :multigraph] :array
  (pr-self-loops g))

(define multigraph-density
  "Multigraph density. 2E / (N*(N-1))."
  [g :multigraph] :number
  (pr-density g))


# ==============================================================================
#  Multidigraph — Directed multigraph with self loops
# ==============================================================================

(deftrecord :multidigraph
  "Directed multigraph. out/in edges map node → {target/source → count}.
   `nodes` is the ordered array of node labels; `node-data` maps each
   node label to that node's data (linked by label, not by index)."
  (field out :table @{})
  (field in :table @{})
  (field nodes :array @[])
  (field node-data :table @{})
  (field edge-data :table @{})
  (field node-count :number 0)
  (field edge-count :number 0))


(define multidigraph-add-node
  "Add a node to a directed multigraph."
  [g :multidigraph node :node-label] :multidigraph
  (when (nil? (in (multidigraph-out g) node))
    (put (multidigraph-out g) node @{})
    (put (multidigraph-in g) node @{})
    (array/push (g :nodes) node)
    (put (g :node-data) node node)
    (set-multidigraph-node-count g (+ (multidigraph-node-count g) 1)))
  g)

(define multidigraph-add-edge
  "Add a directed edge a→b. Increments multiplicity."
  [g :multidigraph a :node-label b :node-label] :multidigraph
  (multidigraph-add-node g a)
  (multidigraph-add-node g b)
  (let [out-a (in (multidigraph-out g) a)
        in-b (in (multidigraph-in g) b)
        current (or (in out-a b) 0)]
    (put out-a b (+ current 1))
    (put in-b a (+ current 1))
    (set-multidigraph-edge-count g (+ (multidigraph-edge-count g) 1)))
  g)

(define multidigraph-remove-edge
  "Remove one edge a→b. Decrements multiplicity."
  [g :multidigraph a :node-label b :node-label] :multidigraph
  (let [out-a (in (multidigraph-out g) a)
        in-b (in (multidigraph-in g) b)]
    (when (and out-a (not= nil (in out-a b)) (> (in out-a b) 0))
      (let [new-val (- (in out-a b) 1)]
        (if (= new-val 0)
          (do (put out-a b nil) (put in-b a nil)
              (pr-remove-edge-data g true a b))
          (do (put out-a b new-val) (put in-b a new-val)))
        (set-multidigraph-edge-count g (- (multidigraph-edge-count g) 1))))
    g))

(define multidigraph-remove-all-edges
  "Remove all edges a→b."
  [g :multidigraph a :node-label b :node-label] :multidigraph
  (let [out-a (in (multidigraph-out g) a)
        in-b (in (multidigraph-in g) b)]
    (when (and out-a (not= nil (in out-a b)))
      (let [count (in out-a b)]
        (put out-a b nil)
        (put in-b a nil)
        (pr-remove-edge-data g true a b)
        (set-multidigraph-edge-count g (- (multidigraph-edge-count g) count))))
    g))

(define multidigraph-remove-node
  "Remove a node and all incident edges."
  [g :multidigraph node :node-label] :multidigraph
  (let [out (multidigraph-out g)
        in-e (multidigraph-in g)
        out-neighbours (in out node)
        in-neighbours (in in-e node)]
    (when out-neighbours
      (each [t count] (pairs out-neighbours)
        (put (in in-e t) node nil)
        (set-multidigraph-edge-count g (- (multidigraph-edge-count g) count)))
      (put out node nil))
    (when in-neighbours
      (each [s count] (pairs in-neighbours)
        (put (in out s) node nil)
        (set-multidigraph-edge-count g (- (multidigraph-edge-count g) count)))
      (put in-e node nil))
    (when out-neighbours
      (loop [[t _] :pairs out-neighbours]
        (pr-remove-edge-data g true node t)))
    (when in-neighbours
      (loop [[s _] :pairs in-neighbours]
        (pr-remove-edge-data g true s node)))
    (put (g :node-data) node nil)
    (def nodes (g :nodes))
    (def idx (find-index (fn [n] (= n node)) nodes))
    (when idx
      (array/remove nodes idx)
      (set-multidigraph-node-count g (- (multidigraph-node-count g) 1)))
    g))

(define multidigraph-has-node?
  "True when node is in the graph."
  [g :multidigraph node :node-label] :boolean
  (not= nil (in (multidigraph-out g) node)))

(define multidigraph-has-edge?
  "True when at least one edge exists a→b."
  [g :multidigraph a :node-label b :node-label] :boolean
  (let [out-a (in (multidigraph-out g) a)]
    (if out-a (and (not= nil (in out-a b)) (> (in out-a b) 0)) false)))

(define multidigraph-edge-count-between
  "Number of edges a→b."
  [g :multidigraph a :node-label b :node-label] :number
  (let [out-a (in (multidigraph-out g) a)]
    (if (and out-a (not= nil (in out-a b))) (in out-a b) 0)))

(define multidigraph-neighbours
  "Return outgoing neighbours of a node."
  [g :multidigraph node :node-label] :array
  (let [out-node (in (multidigraph-out g) node)]
    (if out-node (keys out-node) @[])))

(define multidigraph-in-neighbours
  "Return incoming neighbours of a node."
  [g :multidigraph node :node-label] :array
  (let [in-node (in (multidigraph-in g) node)]
    (if in-node (keys in-node) @[])))

(define multidigraph-nodes
  "Return all nodes."
  [g :multidigraph] :array
  (pr-nodes g))

(define multidigraph-node-data
  "Return node data for `node`, or nil when absent."
  [g :multidigraph node :node-label] :dynamic
  (pr-node-data g node))
(define multidigraph-nodes-table
  "Return the table mapping node name -> node data."
  [g :multidigraph] :table
  (g :node-data))

(define multidigraph-set-node-data
  "Associate a node with data. Adds the node if it does not yet exist."
  [g :multidigraph node :node-label data :dynamic] :multidigraph
  (multidigraph-add-node g node)
  (put (g :node-data) node data)
  g)

(define multidigraph-in-degree
  "In-degree (sum of incoming edge multiplicities). Self-loops count once."
  [g :multidigraph node :node-label] :number
  (let [in-node (in (multidigraph-in g) node)]
    (if (nil? in-node) 0
      (sum (values in-node)))))

(define multidigraph-out-degree
  "Out-degree (sum of outgoing edge multiplicities). Self-loops count once."
  [g :multidigraph node :node-label] :number
  (let [out-node (in (multidigraph-out g) node)]
    (if (nil? out-node) 0
      (sum (values out-node)))))

(define multidigraph-degree
  "Total degree (in + out). Self-loops count double per direction."
  [g :multidigraph node :node-label] :number
  (+ (multidigraph-in-degree g node) (multidigraph-out-degree g node)))

(define multidigraph-self-loops
  "Return nodes with self-loops."
  [g :multidigraph] :array
  (filter (fn [n] (multidigraph-has-edge? g n n)) (multidigraph-nodes g)))

(define multidigraph-density
  "Directed density. E / (N*(N-1))."
  [g :multidigraph] :number
  (let [n (multidigraph-node-count g)]
    (if (<= n 1) 0
      (/ (* 1.0 (multidigraph-edge-count g))
         (* n (- n 1))))))

# ==============================================================================
#  Type-specific algorithms
# ==============================================================================

(define multidigraph-topo-sort
  "Topological sort using Kahn's algorithm. Only works for DAGs.
   Returns nil if a cycle exists."
  [g :multidigraph] [:or :array :nil]
  (let [in-deg @{}
        result @[]
        queue @[]]
    (each node (multidigraph-nodes g)
      (put in-deg node (multidigraph-in-degree g node))
      (when (= 0 (in in-deg node))
        (array/push queue node)))
    (var head 0)
    (while (< head (length queue))
      (let [node (queue head)]
        (++ head)
        (array/push result node)
        (each neighbour (multidigraph-neighbours g node)
          (put in-deg neighbour
               (- (in in-deg neighbour)
                  (get (get (multidigraph-out g) node) neighbour)))
          (when (= 0 (in in-deg neighbour))
            (array/push queue neighbour)))))
    (when (= (length result) (multidigraph-node-count g))
      result)))

(define multigraph-edge-data
  "Data associated with the edge a->b, or nil. Undirected lookups are
  order-insensitive."
  [g :multigraph a :node-label b :node-label] :dynamic
  (pr-edge-data g false a b))

(define multigraph-set-edge-data
  "Associate data with the edge a->b. The edge is not created if absent.
  Returns the graph."
  [g :multigraph a :node-label b :node-label data :dynamic] :multigraph
  (pr-set-edge-data g false a b data))

(define multigraph-edge-data-has?
  "True when data has been stored for the edge a->b."
  [g :multigraph a :node-label b :node-label] :boolean
  (pr-edge-data-has? g false a b))

(define multigraph-remove-edge-data
  "Drop any data stored for the edge a->b."
  [g :multigraph a :node-label b :node-label] :multigraph
  (pr-remove-edge-data g false a b))

(define multigraph-edge-data-table
  "The whole edge-data store, keyed by (tuple source target)."
  [g :multigraph] :table
  (pr-edge-data-table g))

(define multidigraph-edge-data
  "Data associated with the edge u->v, or nil. Undirected lookups are
  order-insensitive."
  [g :multidigraph a :node-label b :node-label] :dynamic
  (pr-edge-data g true a b))

(define multidigraph-set-edge-data
  "Associate data with the edge u->v. The edge is not created if absent.
  Returns the graph."
  [g :multidigraph a :node-label b :node-label data :dynamic] :multidigraph
  (pr-set-edge-data g true a b data))

(define multidigraph-edge-data-has?
  "True when data has been stored for the edge u->v."
  [g :multidigraph a :node-label b :node-label] :boolean
  (pr-edge-data-has? g true a b))

(define multidigraph-remove-edge-data
  "Drop any data stored for the edge u->v."
  [g :multidigraph a :node-label b :node-label] :multidigraph
  (pr-remove-edge-data g true a b))

(define multidigraph-edge-data-table
  "The whole edge-data store, keyed by (tuple source target)."
  [g :multidigraph] :table
  (pr-edge-data-table g))
