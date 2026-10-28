# -*- mode: janet; -*-
# graphs/import/adjacency: Convert adjacency representations to graph records

(import deft :prefix "")
(import ../simple-graph :prefix "_sg_")
(import ../digraph :prefix "_dg_")
(import ../weighted-graph :prefix "wxyz_")
(import ../multigraph :prefix "_mg_")

(defn- opts-table [args]
  (table ;args))

(defn- node-labels [n labels]
  (if labels
    (do
      (when (not= (length labels) n)
        (error (string "adjacency labels: expected " n " labels")))
      labels)
    (map string (range n))))

# The :multigraph and :multidigraph types are unweighted
(defn- check-types [weighted? multi?]
  (when (and weighted? multi?)
    (error
     (string "adjacency import: :multigraph with :weighted is not supported."))))

(defn- new-graph [directed? weighted? multi?]
  (check-types weighted? multi?)
  (cond
    multi? (if directed?
             (_mg_make-multidigraph @{})
             (_mg_make-multigraph @{}))
    weighted? (if directed?
                (_dg_make-digraph @[] @{})
                (wxyz_make-weighted-graph))
    directed? (_dg_make-digraph @[] @{})
    (_sg_make-graph @{})))

(defn- add-node! [g u directed? weighted? multi?]
  (cond
    multi? (if directed?
             (_mg_multidigraph-add-node g u)
             (_mg_multigraph-add-node g u))
    weighted? (if directed?
                (_dg_digraph-add-node g u)
                (wxyz_weighted-graph-add-node g u))
    directed? (_dg_digraph-add-node g u)
    (_sg_graph-add-node g u)))

(defn- add-edge! [g u v weight directed? weighted? multi?]
  (cond
    multi? (if directed?
             (_mg_multidigraph-add-edge g u v)
             (_mg_multigraph-add-edge g u v))
    weighted? (if directed?
                (_dg_digraph-add-edge g u v weight)
                (wxyz_weighted-graph-add-edge g u v weight))
    directed? (_dg_digraph-add-edge g u v 1)
    (_sg_graph-add-edge g u v)))


(define graph-from-adjacency-list
  ```Build a graph from an array of neighbour-index arrays.

Each neighbour is an index. When `:weighted`, each neighbour is `[index weight]`

Options  :directed (default false)
         :weighted (default false)
         :multigraph (default false)
         :labels (default nil)

`:multigraph` builds a :multigraph, or a :multidigraph with `:directed` with
parallel entries in `neighbours`.
```
  [neighbours & args]
  (let [opts (opts-table args)
        directed? (get opts :directed false)
        weighted? (get opts :weighted false)
        n (length neighbours)
        labels (node-labels n (get opts :labels))
        multi? (get opts :multigraph false)
        g (new-graph directed? weighted? multi?)]
    (each u labels (add-node! g u directed? weighted? multi?))
    (loop [i :range [0 n]]
      (each item (get neighbours i)
        (def [v weight]
          (if (and weighted? (or (array? item) (tuple? item)))
            item
            [item 1]))
        (unless (and (and (number? v)
                          (= v (math/floor v)))
                     (>= v 0) (< v n))
          (error (string "adjacency list neighbour index out of range: " v)))
        (unless (and multi? (not directed?) (> i v))
          (add-edge! g (labels i) (labels v) weight directed? weighted? multi?))))
    g))


(define graph-from-adjacency-matrix
  ```Build a graph from a square array-of-arrays.

Nonzero entries create edges, numeric nonzero values become weights if `:weighted`.

Options  :directed (default false)
         :weighted (default false)
         :multigraph (default false)
         :labels (default nil)

`:multigraph` builds a :multigraph, or a :multidigraph with `:directed` with
parallel entries in `neighbours`.
```
  [matrix & args] :any-graph
  (let [opts (opts-table args)
        directed? (get opts :directed false)
        weighted? (get opts :weighted false)
        n (length matrix)
        labels (node-labels n (get opts :labels))
        multi? (get opts :multigraph false)
        g (new-graph directed? weighted? multi?)]
    (each row matrix
      (when (not= (length row) n)
        (error "adjacency matrix must be square")))
    (each u labels (add-node! g u directed? weighted? multi?))
    (loop [i :range [0 n]]
      (loop [j :range [0 n]]
        (when (or directed? (<= i j) (= (get (get matrix j) i) 0))
        (def value (get (get matrix i) j))
        (def present? (and (not= value false)
                           (not= value 0)
                           (not= value nil)))
        (when present?
          (add-edge! g (labels i) (labels j)
                     (if weighted? value 1)
                     directed? weighted? multi?)))))
    g))
