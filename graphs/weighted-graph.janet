# -*- mode: janet; -*-
# graphs/weighted-graph: undirected graph with weights, MST and Dijkstra

(import deft :prefix "")
(import ./record :prefix "adj-")
(import ./heap :prefix "_h_")
(import ./distance-matrix :prefix "_dm_")

(deftrecord :weighted-graph
  (field nodes :array @[])
  (field adj :table @{})
  (field node-data :table @{})
  (field seen :table @{})
  (field edge-data :table @{})
  (field node-count :number 0)
  (field edge-count :number 0))

(define weighted-graph-add-node
  "Add a node to a graph. No-op if node already exists. Returns the graph."
  [g :weighted-graph name :node-label] :weighted-graph
  (adj-add-node g name))

(define weighted-graph-add-edge
  "Add or re-weight the edge between `a` and `b`."
  [g :weighted-graph a :node-label b :node-label weight :number] nil
  (weighted-graph-add-node g a)
  (weighted-graph-add-node g b)
  (def adj-a (get (g :adj) a))
  (def adj-b (get (g :adj) b))
  (unless (get adj-a b) (set (g :edge-count) (inc (g :edge-count))))
  (put adj-a b weight)
  (put adj-b a weight))

(define weighted-graph-remove-edge
  "Remove the edge between `a` and `b` from grapn `g`."
  [g :weighted-graph a :node-label b :node-label] nil
  (def adj-a (get (g :adj) a))
  (def adj-b (get (g :adj) b))
  (when (and adj-a (get adj-a b))
    (set (g :edge-count) (dec (g :edge-count))))
  (when adj-a (put adj-a b nil))
  (when adj-b (put adj-b a nil))
  (adj-remove-edge-data g false a b))

(define weighted-graph-remove-node
  "Remove `node` and all its incident edges."
  [g :weighted-graph node :node-label] :weighted-graph
  (adj-remove-node g node (fn [v] 1)))

(define weighted-graph-neighbours
  "Neighbours of `node` in graph `g`."
  [g :weighted-graph node :node-label] :array
  (def adj (get (g :adj) node))
  (if adj
    (seq [[k v] :pairs adj] [k v])
    @[]))

(define weighted-graph-neighbour-names
  "Array of names of neighbours of `node` in the graph `g`."
  [g :weighted-graph name :node-label] :array
  (map (fn [[k _]] k) (weighted-graph-neighbours g name)))

(define weighted-graph-degree
  "Find degree of a node `name` in the graph `g`.
Degree is the number of incident edges, counting a self-loop twice."
  [g :weighted-graph name :node-label] :integer
  (def row (get (g :adj) name))
  (if row (if (get row name) (+ (length row) 1) (length row)) 0))

(define weighted-graph-nodes
  "Return all node labels in graph `g`."
  [g :weighted-graph] :array
  (adj-nodes g))

(define weighted-graph-node-data
  "Return node data for `node`, or nil when absent."
  [g :weighted-graph node :node-label] :dynamic
  (adj-node-data g node))

(define weighted-graph-node-count
  "Number of nodes in the graph `g`."
  [g :weighted-graph] :integer
  (adj-node-count g))

(define weighted-graph-nodes-table
  "Return the table mapping node-name -> node-data in the graph `g`."
  [g :weighted-graph] :table
  (adj-nodes-table g))

(define weighted-graph-set-node-data
  "Associate a node with data. Adds the node if it doesn't exist."
  [g :weighted-graph node :node-label data :dynamic] :weighted-graph
  (adj-set-node-data g node data))

(define weighted-graph-edge-weight
  [g :weighted-graph a :node-label b :node-label] :number-or-nil
  (adj-edge-weight g a b))

(define weighted-graph-edge-count
  ```Number of undirected edges of graph `g`, counting self-loops once.```
  [g :weighted-graph] :integer
  (adj-undirected-edge-count-underlying g))

(defn- negative-weight?
  "True when any edge of the weighted graph carries a negative weight."
  [g]
  (var found false)
  (loop [[_ bs] :pairs (g :adj)]
    (loop [[_ w] :pairs bs]
      (when (< w 0) (set found true))))
  found)

(defn- weighted-graph-dijkstra
  ```Single-source shortest paths in graph `g` from node `start`.
Returns [dist prev] where dist is {node distance}, prev is {node predecessor}.
```
  [g start &opt end]
  # Dijkstra assumes postive edge weights
  (when (negative-weight? g)
    (error
     (string
      "weighted-graph shortest paths: graph contains a negative edge weight")))
  (def dist @{start 0.0})
  (def prev @{})
  (def pq (_h_make-binary-min-heap
            :items @[[0.0 start]]
            :compare (fn [[d1 _] [d2 _]] (< d1 d2))))
  (var done false)
  (while (and (> (_h_heap-size pq) 0) (not done))
    (def [d u] (_h_heap-pop pq))
    (when (and (get dist u) (= d (get dist u)))
      (when (and end (= u end)) (set done true))
      (unless done
        (loop [[v w] :pairs (get (g :adj) u)]
          (def alt (+ d w))
          (def cur (get dist v))
          (when (or (nil? cur) (< alt cur))
            (put dist v alt)
            (put prev v u)
            (_h_heap-push pq [alt v]))))))
  [dist prev])


(define weighted-graph->distance-matrix
  ```All-pairs weighted shortest paths in `g` as a :distance-matrix.
The node set is sorted by name. Unreachable pairs are math/inf.
```
  [g :weighted-graph] :distance-matrix
  (let [names (sorted (g :nodes))
        ln (length names)
        idx @{}]
    (each [i nm] (pairs names) (put idx nm i))
    (def inf math/inf)
    (def dist (seq [i :range [0 ln]]
                (seq [j :range [0 ln]]
                  (if (= i j) 0.0 inf))))
    (when (> ln 1)
      (loop [[a bs] :pairs (g :adj)]
        (loop [[b w] :pairs bs]
          (when (not= a b)
            (put (get dist (get idx a)) (get idx b) w))))
      (for k 0 ln
        (def dk (dist k))
        (for i 0 ln
          (def dik (get (dist i) k))
          (when (< dik inf)
            (def row-i (dist i))
            (for j 0 ln
              (def alt (+ dik (get dk j)))
              (when (< alt (row-i j))
                (put row-i j alt)))))))
    (_dm_make-distance-matrix names dist)))


(define weighted-graph-mst
  ```Minimum spanning tree of a connected weighted graph `g`.```
  [g :weighted-graph] :weighted-graph
  (if (empty? (g :nodes))
    (make-weighted-graph @[] @{})
    (do
      (def start ((g :nodes) 0))
      (def in-mst @{start true})
      (def mst (make-weighted-graph @[] @{}))
      (def cmp3 (fn [[w1 _ _] [w2 _ _]] (< w1 w2)))
      (def heap (_h_make-binary-min-heap :items
                  (seq [[n w] :pairs (get (g :adj) start)]
                    [w start n])
                  :compare cmp3))
      (while (< (length (mst :nodes)) (length (g :nodes)))
        (def item (_h_heap-pop heap))
        (when (nil? item) (break))
        (def [w a b] item)
        (unless (get in-mst b)
          (weighted-graph-add-edge mst a b w)
          (put in-mst b true)
          (loop [[n nw] :pairs (get (g :adj) b)]
            (unless (get in-mst n)
              (_h_heap-push heap [nw b n])))))
      (unless (= (length (mst :nodes)) (length (g :nodes)))
        (error (string "weighted-graph-mst: graph is disconnected, so it has "
                       "no spanning tree (reached "
                       (length (mst :nodes)) " of " (length (g :nodes))
                       " nodes)")))
      mst)))


(define weighted-graph-shortest-path
  ```Shortest path from `start` to `end` via Dijkstra.
Returns [distance path], or nil if end is unreachable.
  ```
  [g :weighted-graph start :node-label end :node-label] (or :array :nil)
  (def [dist prev] (weighted-graph-dijkstra g start end))
  (def end-d (get dist end))
  (when end-d
    (def path @[end])
    (var cur end)
    (while (get prev cur)
      (set cur (get prev cur))
      (array/push path cur))
    @[end-d (reverse path)]))

(define weighted-graph-all-shortest-paths
  "Shortest path distances from `start` to every node via Dijkstra.
Returns {node distance}."
  [g :weighted-graph start :node-label] :table
  (first (weighted-graph-dijkstra g start)))

(define weighted-graph-edge-data
  "Returns data associated with the edge a->b, or nil.
Undirected lookups are order-insensitive."
  [g :weighted-graph a :node-label b :node-label] :dynamic
  (adj-edge-data g false a b))

(define weighted-graph-set-edge-data
  "Associate data with edge from `a` to `b`. The edge is not created if absent.
  Returns the graph."
  [g :weighted-graph a :node-label b :node-label data :dynamic] :weighted-graph
  (adj-set-edge-data g false a b data))

(define weighted-graph-edge-data-has?
  "True when data has been stored for the edge `a` to `b`."
  [g :weighted-graph a :node-label b :node-label] :boolean
  (adj-edge-data-has? g false a b))

(define weighted-graph-remove-edge-data
  "Drop any data stored for the edge `a` to `b`."
  [g :weighted-graph a :node-label b :node-label] :weighted-graph
  (adj-remove-edge-data g false a b))

(define weighted-graph-edge-data-table
  "The whole edge-data store, keyed by (tuple source target)."
  [g :weighted-graph] :table
  (adj-edge-data-table g))
