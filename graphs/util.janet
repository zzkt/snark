# -*- mode: janet; -*-
# graphs/util: shared helpers for graph types

#  Generic accessors that work across :graph, :digraph, :weighted-graph,
#  :multigraph, :multidigraph and :tree types.
#
#  Reduces duplication of *-nodes, *-neighbours, *-out-neighbours fuctions
#
#  Field layout per type. Every record type stores its node labels in the
#  ordered array `:nodes` which indexes node data in the table `:node-data`
#
#    :graph / :multigraph          — field :adj  (node → {neighbour → count})
#    :digraph / :weighted-graph    — field :adj  (node → {neighbour → weight})
#    :multidigraph                 — fields :out + :in (per-direction counts)
#    :tree                         — field :digraph (backing digraph)
#    :weighted-tree                — field :graph (backing weighted graph)

(import deft :prefix "")
(import ./record :prefix "rec-")
(import ./types :prefix "")


# ==============================================================================
#  Node accessors
# ==============================================================================

(define nodes
  "Return all node names from any graph type."
  [g :any-graph] :array
  (typecase-strict g
    :tree (get (g :digraph) :nodes)
    :weighted-tree (get (g :graph) :nodes)
    :multidigraph (rec-nodes g)
    :graph (rec-nodes g)
    :multigraph (rec-nodes g)
    :weighted-graph (rec-nodes g)
    :digraph (rec-nodes g)
    (fn [untagged]
      (cond
        (table? (get untagged :digraph)) (keys (get (get untagged :digraph) :adj))
        (table? (get untagged :out)) (keys (untagged :out))
        (array? (get untagged :nodes)) (untagged :nodes)
        (table? (get untagged :adj)) (keys (untagged :adj))
        @[]))))


# ==============================================================================
#  Collection helpers
# ==============================================================================

(defn to-set
  "Normalise any collection (array/tuple/table) to a distinct-node table."
  [nodes]
  (def s @{})
  (if (table? nodes)
    (each k (keys nodes) (put s k true))
    (each n nodes (put s n true)))
  s)

(defn arr-contains?
  "True when `arr` contains the value `v` (linear scan)."
  [arr v]
  (truthy? (some (fn [a] (= a v)) arr)))

(defn duplicate-pairs?
  ```True when a pair appears more than once in an array `pairs`.

The search is ordered, not normalised ([a b] and [b a] are different pairs).
```
  [pairs]
  (def seen @{})
  (var dup false)
  (each p pairs
    (def k (tuple (p 0) (p 1)))
    (if (get seen k)
      (set dup true)
      (put seen k true)))
  dup)

(defn node-set
  "All nodes of a graph, as a set."
  [g]
  (to-set (nodes g)))

(defn set-minus
  "Set difference. All keys of `a` that are not keys of `b`."
  [a b]
  (to-set (seq [k :keys a :unless (get b k)] k)))

(defn- adj-table
  "Return the neighbour-count table for node `u`, or nil."
  [g u]
  (typecase-strict g
    :tree (get (get (g :digraph) :adj) u)
    :weighted-tree (get (get (g :graph) :adj) u)
    :multidigraph (get (g :out) u)
    :graph (get (rec-adjacency g) u)
    :multigraph (get (rec-adjacency g) u)
    :weighted-graph (get (rec-adjacency g) u)
    :digraph (get (rec-adjacency g) u)
    (fn [untagged]
      (cond
        (table? (get untagged :digraph)) (get (get (get untagged :digraph) :adj) u)
        (table? (get untagged :graph)) (get (get (get untagged :graph) :adj) u)
        (table? (get untagged :out)) (get (untagged :out) u)
        (get (untagged :adj) u)))))

(defn has-node?
  "True when `node` is in the graph `g`."
  [g node]
  (typecase-strict g
    :tree (not= nil (get (get (g :digraph) :adj) node))
    :weighted-tree (not= nil (get (get (g :graph) :adj) node))
    :multidigraph (not= nil (get (g :out) node))
    :graph (not= nil (get (rec-adjacency g) node))
    :multigraph (not= nil (get (rec-adjacency g) node))
    :weighted-graph (not= nil (get (rec-adjacency g) node))
    :digraph (not= nil (get (rec-adjacency g) node))
    (fn [untagged]
      (cond
        (table? (get untagged :digraph))
        (not= nil (get (get (get untagged :digraph) :adj) node))
        (table? (get untagged :out))
        (not= nil (get (untagged :out) node))
        (not= nil (get (untagged :adj) node))))))


(defn neighbours
  "Return node-name keys for u's outgoing/undirected neighbours."
  [g u]
  (def adj (adj-table g u))
  (if adj (keys adj) @[]))

(defn out-neighbours
  "Return [neighbour weight] pairs for u's outgoing/undirected edges."
  [g u]
  (def adj (adj-table g u))
  (if adj
    (seq [[v w] :pairs adj] [v w])
    @[]))

(defn edge-weights
  "Return a {neighbour weight} table for u's outgoing/undirected edges."
  [g u]
  (def adj (adj-table g u))
  (if adj
    (let [out @{}]
      (each [v w] (pairs adj)
        (put out v (if (boolean? w) 1.0 w)))
      out)
    @{}))

(defn- adj-in-neighbours
  "Incoming neighbours of u using the adjacency table."
  [g u]
  (seq [[s tbl] :pairs (rec-adjacency g) :when (get tbl u)] s))

(defn- adj-in-degree
  "Number of distinct in-neighbours of u using the adjacency table."
  [g u]
  (sum (seq [[s tbl] :pairs (rec-adjacency g) :when (get tbl u)] 1)))

(defn in-degree
  "Number of incoming edges to u counting edge multiplicity."
  [g u]
  (typecase-strict g
    :multidigraph
      (let [in-tbl (get (g :in) u)]
        (if in-tbl (sum (values in-tbl)) 0))
    :digraph
      (adj-in-degree g u)
    :weighted-graph
      (adj-in-degree g u)
    (fn [untagged]
      (cond
        (table? (get untagged :in))
          (let [in-tbl (get (untagged :in) u)]
            (if in-tbl (sum (values in-tbl)) 0))
        (array? (get untagged :nodes))
          (adj-in-degree untagged u)
        (length (neighbours untagged u))))))

(defn edge-multiplicity
  "Number of parallel arcs from u to v.
Edge count for multigraphs, 1 for other graphs, 0 if edge doesn't exist."
  [g u v]
  (typecase-strict g
    :multidigraph (or (get-in (g :out) [u v]) 0)
    :multigraph (or (get-in (rec-adjacency g) [u v]) 0)
    (fn [x] (if (get-in (or (get x :adj)
                            (get (get x :digraph) :adj)) [u v]) 1 0))))

(defn has-edge?
  "Return true if there is an edge from u to v."
  [g u v]
  (def adj (adj-table g u))
  (not (nil? (get adj v))))

(defn in-neighbours
  "Return node labels of u's incoming neighbours (directed).
For undirected types returns the same as neighbours (edges are symmetric)."
  [g u]
  (typecase-strict g
    :multidigraph
      (let [in-tbl (get (g :in) u)]
        (if in-tbl (keys in-tbl) @[]))
    :digraph
      (adj-in-neighbours g u)
    :weighted-graph
      (adj-in-neighbours g u)
    :tree
      (let [parent (get (g :parent) u)]
        (if parent @[parent] @[]))
    (fn [untagged]
      (cond
        (table? (get untagged :in))
          (let [in-tbl (get (untagged :in) u)]
            (if in-tbl (keys in-tbl) @[]))
        (array? (get untagged :nodes))
          (adj-in-neighbours untagged u)
        (neighbours untagged u)))))


# ==============================================================================
#  Direction detection
# ==============================================================================

(define structurally-directed?
 "Structural direction auto-detection.
 True when some edge u->v has no reverse  v->u in the adjacency."
  [g] :boolean
  (def adj (get g :adj))
  (if (not (nil? adj))
    (do
      (var directed false)
      (var any-edge false)
      (loop [[u tbl] :pairs adj]
        (loop [[v _] :pairs tbl]
          (set any-edge true)
          (when (nil? (get-in adj [v u]))
            (set directed true))))
      (if any-edge directed true))
    (cond
      (or (= (type g) :tree) (= (type g) :weighted-tree)) false
      (= (type g) :multidigraph) true
      (table? (get g :out)) true
      (array? (get g :nodes)) true
      true)))

(define directed?
  "True for graph types that are directed by construction."
  [g :any-graph] :boolean
  (let [dcl (type g)]
    (cond
      (or (= :digraph dcl) (= :multidigraph dcl)) true
      (or (= :graph dcl) (= :multigraph dcl) (= :tree dcl)
          (= :weighted-tree dcl) (= :weighted-graph dcl)) false
      (table? (get g :out)) true
      (array? (get g :nodes)) true
      false)))


# ==============================================================================
#  Structural edges
# ==============================================================================

(defn structural-edges-for
  "Distinct structural edges of `g` as an array of `[u v]` pairs."
  [g directed?]
  (def edges @[])
  (loop [[u tbl] :pairs (rec-adjacency g)]
    (loop [[v _] :pairs tbl]
      (when (and (not= u v)
                 (or directed? (< u v)))
        (array/push edges @[u v]))))
  edges)

(defn structural-edges
  "Distinct structural edges of `g`, self-loops excluded."
  [g]
  (structural-edges-for g (structurally-directed? g)))

(defn structural-degree
  ```Number of distinct edges incident to `n`.
Distinct out-edges plus in-edges for a directed graph. Self-loops excluded.
  ```
  [g n]
  (def out-inc (length (seq [v :in (neighbours g n) :when (not= v n)] v)))
  (def in-inc (if (structurally-directed? g)
                 (length (seq [v :in (in-neighbours g n) :when (not= v n)] v))
                 0))
  (+ out-inc in-inc))


# ==============================================================================
#  Deterministic ordering helpers
# ==============================================================================

(defn node-cmp
  "Total deterministic 3-way order over node names (strings)."
  [a b]
  (if (< a b) -1 (if (> a b) 1 0)))

(defn sort-with
  "Sort array `arr` with a comparator `cmp` in place."
  [arr cmp]
  (sort arr (fn [a b] (< (cmp a b) 0))))

(defn canonical-nodes
  "Canonical (sorted, deterministic) copy of a node array."
  [nodes]
  (sort-with (array/slice nodes) node-cmp))


# ==============================================================================
#  Canonical unordered-edge key
# ==============================================================================

(defn node-lt?
  "String ordering used to canonicalise unordered edge pairs."
  [u v] (< (string u) (string v)))

(defn canonical-edge-key
  "Canonical unordered edge key (tuple u v with u preceding v in `node-lt?`)."
  [u v] (if (node-lt? u v) (tuple u v) (tuple v u)))


# ==============================================================================
#  In-degree table
# ==============================================================================

(define in-degree-table
  "In-degrees of every node of g in one pass, {node: count}."
  [g :any-graph] :table
  (def nodes (keys (to-set (nodes g))))
  (def result @{})
  (each n nodes (put result n 0))
  (cond
    (table? (get g :parent))
      (do
        (each n nodes
          (when (get (g :parent) n) (put result n 1)))
        result)
    (table? (get g :in))
      (do
        (each u nodes
          (def in-tbl (get (g :in) u))
          (when in-tbl (put result u (sum (values in-tbl)))))
        result)
    (not (nil? (get g :adj)))
      (do
        (each node nodes
          (def tbl (get (rec-adjacency g) node))
          (when tbl
            (each v (keys tbl)
              (put result v (+ 1 (or (get result v) 0))))))
        result)
    (array? (get g :nodes))
      result
    (each n nodes (put result n (length (neighbours g n))))
    result))


# ==============================================================================
#  Undirected-neighbour accessor (for structural analysis)
# ==============================================================================

(defn- adj-undirected-neighbours
  "Node names reachable from u treating every :adj edge as undirected."
  [g u]
  (let [result @{}
        out (get (rec-adjacency g) u)]
    (when out (each v (keys out) (when (not= v u) (put result v true))))
    (loop [[s tbl] :pairs (rec-adjacency g) :when (get tbl u)]
      (when (not= s u) (put result s true)))
    (keys result)))

(defn- symmetric-adj-neighbours
  "Neighbours of `u` for a graph `g` whose :adj is symmetric."
  [g u]
  (def result @[])
  (each v (neighbours g u) (when (not= v u) (array/push result v)))
  result)

(defn undirected-neighbours
  "Return u's neighbours treating every edge as undirected."
  [g u]
  (typecase-strict g
    :tree
      (let [result @{}
            parent (get (g :parent) u)]
        (when parent (put result parent true))
        (each v (keys (get (get (g :digraph) :adj) u))
          (when (and (not= v u) (nil? (get result v)))
            (put result v true)))
        (keys result))
    :multidigraph
      (let [out (get (g :out) u)
            in (get (g :in) u)
            result @{}]
        (when out (each v (keys out)
                    (when (not= v u) (put result v true))))
        (when in (each v (keys in)
                   (when (not= v u) (put result v true))))
        (keys result))
    :digraph
      (adj-undirected-neighbours g u)
    :weighted-graph
      (adj-undirected-neighbours g u)
    (fn [untagged]
      (cond
        (table? (get untagged :in))
          (let [result @{}
                out (get (untagged :out) u)
                in (get (untagged :in) u)]
            (when out (each v (keys out)
                        (when (not= v u) (put result v true))))
            (when in (each v (keys in)
                       (when (not= v u) (put result v true))))
            (keys result))
        (array? (get untagged :nodes))
          (adj-undirected-neighbours untagged u)
        (symmetric-adj-neighbours untagged u)))))

(defn- in-edge-index
  "Build {node {incoming-source ...}} from an adjacency table."
  [adj]
  (def in-tbl @{})
  (each [s tbl] (pairs adj)
    (each v (keys tbl)
      (put in-tbl v (put (or (get in-tbl v) @{}) s true))))
  in-tbl)

(defn- make-adj-in-index-neighbours-fn
  "Build {node {incoming-source ...}} over g's :adj and return a closure."
  [g]
  (def adj (get g :adj))
  (def in-tbl (in-edge-index adj))
  (fn [_ u]
    (def result @{})
    (def out (get adj u))
    (when out (each v (keys out)
                (when (not= v u) (put result v true))))
    (def ins (get in-tbl u))
    (when ins (each s (keys ins)
                (when (not= s u) (put result s true))))
    (keys result)))

(defn make-undirected-neighbours-fn
  "Return a neighbour accessor with same shape as `undirected-neighbours`

 Use this in preference to `undirected-neighbours` when the caller sweeps
 every node (DFS, components, planarity, etc) and the graph doesn't change."
  [g]
  (typecase-strict g
    # already O(deg) per node for these, nothing to precompute
    :tree undirected-neighbours
    :weighted-tree undirected-neighbours
    :multidigraph undirected-neighbours
    :graph (fn [_ u] (symmetric-adj-neighbours g u))
    :multigraph (fn [_ u] (symmetric-adj-neighbours g u))
    :digraph (make-adj-in-index-neighbours-fn g)
    :weighted-graph (make-adj-in-index-neighbours-fn g)
    (fn [untagged]
      (cond
        (table? (get untagged :in)) undirected-neighbours
        (array? (get untagged :nodes)) (make-adj-in-index-neighbours-fn untagged)
        (fn [_ u] (symmetric-adj-neighbours untagged u))))))


# ==============================================================================
#  In-neighbour accessor (directed sweeps)
# ==============================================================================

(defn- make-adj-in-index-in-fn
  "Build {node {incoming-source ...}} over g's :adj once, and return a closure."
  [g]
  (def in-tbl (in-edge-index (rec-adjacency g)))
  (fn [_ u]
    (def ins (get in-tbl u))
    (if ins (keys ins) @[])))

(defn make-in-neighbours-fn
  "Return an in-neighbour accessor with the same shape as `in-neighbours`.

 Use this in preference to `in-neighbours` when the caller sweeps
 every node and the graph doesn't change."
  [g]
  (typecase-strict g
    # already O(1)/O(deg) per node for these, nothing to precompute
    :multidigraph in-neighbours
    :tree in-neighbours
    :weighted-tree in-neighbours
    :digraph (make-adj-in-index-in-fn g)
    :weighted-graph (make-adj-in-index-in-fn g)
    (fn [untagged]
      (cond
        (table? (get untagged :in)) in-neighbours
        (array? (get untagged :nodes)) (make-adj-in-index-in-fn untagged)
        in-neighbours))))


# ==============================================================================
#  Raw-adjacency accessor (for traversals that walk pairs directly)
# ==============================================================================

(defn make-raw-adj-fn
  "Return `(fn [g u])` giving raw neighbour table."
  [g]
  (typecase-strict g
    :digraph (fn [_ u] (get (rec-adjacency g) u))
    :weighted-graph (fn [_ u] (get (rec-adjacency g) u))
    (fn [untagged]
      (cond
        (table? (get untagged :digraph)) nil
        (table? (get untagged :graph)) nil
        (table? (get untagged :out)) nil
        (fn [_ u] (get (rec-adjacency untagged) u))))))


# ==============================================================================
#  XML text escaping
# ==============================================================================

(defn xml-escape
  "Escape the five XML predefined entities.."
  [s]
  (var t (string s))
  (set t (string/replace-all "&" "&amp;" t))
  (set t (string/replace-all "<" "&lt;" t))
  (set t (string/replace-all ">" "&gt;" t))
  (set t (string/replace-all "\"" "&quot;" t))
  (set t (string/replace-all "'" "&apos;" t))
  t)

(defn xml-unescape
  "Inverse of xml-escape, accepts numeric references (e.g. &#45;)"
  [s]
  (let [t (string s)
        out (buffer/new (length t))]
    (var i 0)
    (def n (length t))
    (while (< i n)
      (def c (get t i))
      (if (= c 38)  # ampersand
        (do
          (def semi (string/find ";" t (+ i 1)))
          (if (nil? semi)
            (do (buffer/push-byte out c) (++ i))
            (do
              (def body (string/slice t (+ i 1) semi))
              (var rep nil)
              (cond
                (= body "amp") (set rep "&")
                (= body "lt") (set rep "<")
                (= body "gt") (set rep ">")
                (= body "quot") (set rep "\"")
                (= body "apos") (set rep "'")
                (and (> (length body) 1) (= (get body 0) 35))
                (do (def num (scan-number (string/slice body 1)))
                    (if (and num (< num 128)) (set rep num)))
                true nil)
              (if (nil? rep)
                (buffer/push-string out (string/slice t i (inc semi)))
                (if (number? rep)
                  (buffer/push-byte out rep)
                  (buffer/push-string out rep)))
              (set i (inc semi)))))
        (do (buffer/push-byte out c) (++ i))))
    (string out)))
