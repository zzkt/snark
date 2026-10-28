# -*- mode: janet; -*-
# graphs/digraph: Directed graph data structure

(import deft :prefix "")
(import ./record :prefix "adj-")

(deftrecord :digraph
  (field nodes :array @[])
  (field adj :table @{})
  (field node-data :table @{})
  (field seen :table @{})
  (field edge-data :table @{})
  (field node-count :number 0)
  (field edge-count :number 0))

(define digraph-add-node
  ```Add a `node` to graph `g.`.

Returns the digraph, or no-op if the node already exists.
```
  [g :digraph name :node-label] :digraph
  (adj-add-node g name))

(define digraph-add-edge
 ```Add a directed edge `u` → `v` with weight `w` to digraph `g`.

Creates nodes u and v if they don't exist.
```
  [g :digraph u :node-label v :node-label w :number]
  (unless (get (g :adj) u) (digraph-add-node g u))
  (unless (get (g :adj) v) (digraph-add-node g v))
  (def adj-u (get (g :adj) u))
  (when (nil? (get adj-u v))
    (set (g :edge-count) (inc (g :edge-count))))
  (put adj-u v w))

(define digraph-remove-edge
  ```Remove the directed edge `u` → `v`from digraph `g`.

Returns the graph. no-op if edge does not exist.
```
  [g :digraph u :node-label v :node-label] :digraph
  (let [adj-u (get (g :adj) u)]
    (when (and adj-u (not= nil (get adj-u v)))
      (put adj-u v nil)
      (adj-remove-edge-data g true u v)
      (set (g :edge-count) (dec (g :edge-count)))))
  g)

(define digraph-remove-node
  ```Remove `node` from graph `g` and all its incident arcs.```
  [g :digraph node :node-label] :digraph
  (adj-remove-node g node (fn [v] 1) true))

(define digraph-out-neighbours
  "Return array of [neighbour weight] pairs for outgoing edges of u."
  [g :digraph u :node-label] :array
  (def adj (get (g :adj) u))
  (if adj (seq [[n w] :pairs adj] @[n w]) @[]))

(define digraph-in-neighbours
  "Return array of [neighbour weight] pairs for incoming edges of u."
  [g :digraph u :node-label] :array
  (seq [[v adj] :pairs (g :adj) :when (get adj u)]
    @[v (get adj u)]))

(define digraph-out-degree
  "Number of outgoing edges from u."
  [g :digraph u :node-label] :integer
  (adj-degree g u))

(define digraph-in-degree
  "Number of incoming edges to u."
  [g :digraph u :node-label] :integer
  (sum (seq [[_ adj] :pairs (g :adj) :when (get adj u)] 1)))

(define digraph-edge-weight
  "Weight of edge `u` → `v`, or nil if the edge doesn't exist."
  [g :digraph u :node-label v :node-label] [:or :number :nil]
  (adj-edge-weight g u v))

(define digraph-has-edge?
  "Check if edge `u` → `v` exists."
  [g :digraph u :node-label v :node-label] :boolean
  (not (nil? (get-in (g :adj) [u v]))))

(define digraph-node-data
  "Return node data for `node`, or nil when absent."
  [g :digraph node :node-label] :dynamic
  (adj-node-data g node))

(define digraph-nodes
  "Return all node names."
  [g :digraph] :array
  (adj-nodes g))

(define digraph-nodes-table
  "Return the table mapping node name -> node data."
  [g :digraph] :table
  (adj-nodes-table g))

(define digraph-set-node-data
  "Associate a node with data. Adds the node if it does not yet exist."
  [g :digraph node :node-label data :dynamic] :digraph
  (adj-set-node-data g node data))

(define digraph-reverse
  "Return a new digraph with all edges reversed."
  [g :digraph] :digraph
  (def rev (make-digraph @[] @{}))
  (each v (g :nodes) (digraph-add-node rev v))
  (loop [[u adj] :pairs (g :adj)]
    (loop [[v w] :pairs adj]
      (digraph-add-edge rev v u w)
      (when-let [d (adj-edge-data g true u v)]
        (adj-set-edge-data rev true v u d))))
  (each v (g :nodes)
    (adj-set-node-data rev v (adj-node-data g v)))
  rev)

(define digraph-node-count
  "Number of nodes in the graph `g`."
  [g :digraph] :integer
  (adj-node-count g))

(define digraph-edge-count
  "Total number of edges in the graph `g`."
  [g :digraph] :integer
  (adj-edge-count-sum g))

(define digraph-edge-data
  "Data associated with the edge u->v, or nil."
  [g :digraph a :node-label b :node-label] :dynamic
  (adj-edge-data g true a b))

(define digraph-set-edge-data
  "Associate data with the edge `u` → `v`. The edge is not created if absent."
  [g :digraph a :node-label b :node-label data :dynamic] :digraph
  (adj-set-edge-data g true a b data))

(define digraph-edge-data-has?
  "True when data has been stored for the edge `u` → `v`."
  [g :digraph a :node-label b :node-label] :boolean
  (adj-edge-data-has? g true a b))

(define digraph-remove-edge-data
  "Drop any data stored for the edge `u` → `v`."
  [g :digraph a :node-label b :node-label] :digraph
  (adj-remove-edge-data g true a b))

(define digraph-edge-data-table
  "The whole edge-data store, keyed by (tuple source target)."
  [g :digraph] :table
  (adj-edge-data-table g))
