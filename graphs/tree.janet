# -*- mode: janet; -*-
# graphs/tree: Rooted tree wrappers over digraph or weighted graph

(import deft :prefix "")
(import ./digraph :prefix "")
(import ./weighted-graph :prefix "")

(deftrecord :tree
  (field digraph :digraph)
  (field root :dynamic)
  (field parent :table))

(define tree->digraph
  "Return the backing :digraph of the tree."
  [t :tree] :digraph
  (t :digraph))

(define tree-nodes
  "Return all node names of the tree (i.e. backing digraph's ordered labels)."
  [t :tree] :array
  (digraph-nodes (t :digraph)))

(define tree-node-data
  "Return the data associated with a node."
  [t :tree node :node-label] :dynamic
  (digraph-node-data (t :digraph) node))

(define tree-set-node-data
  "Associate a node with data. Adds the node if it does not yet exist."
  [t :tree node :node-label data :dynamic] :tree
  (digraph-set-node-data (t :digraph) node data)
  t)

(define tree-nodes-table
  "Return the table mapping node name -> node data."
  [t :tree] :table
  (digraph-nodes-table (t :digraph)))


# ==============================================================================
#  Weighted tree
# ==============================================================================

(deftrecord :weighted-tree
  (field graph :weighted-graph)
  (field root :dynamic)
  (field parent :table))

(def- _weighted-tree-raw make-weighted-tree)

(defn- _weighted-tree-parents
  "Breadth-first parent map from `root` over the weighted adjacency."
  [g root]
  (def visited @{root true})
  (def parent @{})
  (var frontier @[root])
  (while (> (length frontier) 0)
    (def _next @[])
    (each u frontier
      (def entry (get (g :adj) u))
      (when entry
        (each v (sort (keys entry))
          (unless (get visited v)
            (put visited v true)
            (put parent v u)
            (array/push _next v)))))
    (set frontier _next))
  parent)

(defn make-weighted-tree :shadow
  "Rooted weighted tree from a weighted graph, root node, and parent map."
  [& args]
  (if (keyword? (first args))
    (let [opts (apply table args)
          graph (get opts :graph)
          root (get opts :root)
          parent (get opts :parent)]
      (if parent
        (_weighted-tree-raw graph root parent)
        (_weighted-tree-raw graph root
                            (_weighted-tree-parents graph root))))
    (let [graph (args 0)
          root (args 1)
          parent (if (> (length args) 2) (args 2) nil)]
      (if parent
        (_weighted-tree-raw graph root parent)
        (_weighted-tree-raw graph root
                            (_weighted-tree-parents graph root))))))


(define weighted-tree->graph
  "Return the backing :weighted-graph of the weighted tree."
  [t :weighted-tree] :weighted-graph
  (t :graph))

(define weighted-tree-nodes
  "All node names of the weighted tree (backing graph's ordered labels)."
  [t :weighted-tree] :array
  (weighted-graph-nodes (t :graph)))

(define weighted-tree-nodes-table
  "Return the backing graph's table mapping node name -> node data."
  [t :weighted-tree] :table
  (weighted-graph-nodes-table (t :graph)))

(define weighted-tree-node-data
  "Return the data associated with a node in the backing graph."
  [t :weighted-tree node :node-label] :dynamic
  (weighted-graph-node-data (t :graph) node))

(define weighted-tree-set-node-data
  "Associate a node with data. Adds the node if it does not exist."
  [t :weighted-tree node :node-label data :dynamic] :weighted-tree
  (weighted-graph-set-node-data (t :graph) node data)
  t)

(define weighted-tree-edge-weight
  "Weight of the edge between a and b, or nil if the edge doesn't exist."
  [t :weighted-tree a :node-label b :node-label] [:or :number :nil]
  (weighted-graph-edge-weight (t :graph) a b))

(define weighted-tree-node-count
  "Number of nodes in the weighted tree."
  [t :weighted-tree] :integer
  (weighted-graph-node-count (t :graph)))

(define weighted-tree-edge-count
  "Number of undirected edges in the weighted tree."
  [t :weighted-tree] :integer
  (weighted-graph-edge-count (t :graph)))
