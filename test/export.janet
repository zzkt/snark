# -*- mode: janet; -*-
# graphs/test/export: shared each-edge and multigraph invariants

(import ./helper :prefix "")
(import ../graphs :prefix "")
(import ../graphs/export/common :prefix "ec-")
(import ../graphs/util :prefix "u-")

(print "\n* export: shared invariants")

# :multidigraph keeps :out and :in separately and has no :adj.
(def md (make-multidigraph @{} @{}))
(add-nodes md "a" "b")
(add-edge md "a" "b")
(cassert-eq "multidigraph has no :adj" (nil? (get md :adj)) true)

# each-edge must keep a tree edge whose parent sorts after the child.
(def ed-parent @{})
(put ed-parent "a" "b")
(put ed-parent "b" "c")
(put ed-parent "c" "d")

(def ed-dg (make-digraph @[] @{}))
(add-nodes ed-dg "a" "b" "c" "d")
(each [parent child] [["b" "a"] ["c" "b"] ["d" "c"]]
  (digraph-add-edge ed-dg parent child 1))

(def ed-tree (make-tree ed-dg "d" ed-parent))
(def ed-seen @[])
(ec-each-edge ed-tree (u-structurally-directed? ed-tree)
              (fn [u v w] (array/push ed-seen [u v])))

(cassert-eq "each-edge: a reversed-label tree keeps every parent edge"
            (length ed-seen) (length (get ed-tree :parent)))
(cassert-eq "each-edge: the reported edges are the parent edges"
            (all (fn [[u v]] (= (get (get ed-tree :parent) v) u)) ed-seen) true)
(cassert-eq "to-gexf: a reversed-label tree exports its edges"
            (length (string/find-all "source=" (to-gexf ed-tree))) 3)

# each-edge and multigraph?, asserted directly at the seam.
(defn ee-edges [g directed?]
  (def out @[])
  (ec-each-edge g directed? (fn [u v w] (array/push out [u v w])))
  out)

(each [name g] [["graph" (make-graph @{})]
                ["digraph" (make-digraph @[] @{})]
                ["multigraph" (make-multigraph @{})]
                ["multidigraph" (make-multidigraph @{} @{})]
                ["weighted-graph" (make-weighted-graph @[] @{})]]
  (add-nodes g "a" "b")
  (add-edge g "a" "b")
  (cassert-eq (string "multigraph?: a " name " is one")
              (ec-multigraph? g)
              (or (= name "multigraph") (= name "multidigraph")))
  (cassert-eq (string "each-edge: a " name " reports its one edge once")
              (length (ee-edges g (u-structurally-directed? g))) 1))

# Weight is not multiplicity.
(def ee-w (make-weighted-graph @[] @{}))
(add-nodes ee-w "a" "b")
(add-edge ee-w "a" "b" 7)
(cassert-eq "each-edge: weight 7 is one edge, not seven"
            (length (ee-edges ee-w (u-structurally-directed? ee-w))) 1)
(cassert-eq "each-edge: the weight is preserved"
            (((ee-edges ee-w (u-structurally-directed? ee-w)) 0) 2) 7)

# Multiplicity is expanded into repeated unit edges.
(def ee-m (make-multigraph @{}))
(add-nodes ee-m "a" "b")
(add-edges ee-m ["a" "b"] ["a" "b"] ["a" "b"])
(cassert-eq "each-edge: three parallel edges report three times"
            (length (ee-edges ee-m (u-structurally-directed? ee-m))) 3)
(cassert-eq "each-edge: each is weight 1, not the count"
            (all (fn [e] (= (e 2) 1))
                 (ee-edges ee-m (u-structurally-directed? ee-m))) true)

# Direction: a symmetric digraph is two arcs when asked directed,
# one pair otherwise.
(def ee-sym (make-digraph @[] @{}))
(add-nodes ee-sym "a" "b")
(add-edge ee-sym "a" "b" 1)
(add-edge ee-sym "b" "a" 1)
(cassert-eq "each-edge: both arcs of a symmetric digraph"
            (length (ee-edges ee-sym true)) 2)
(cassert-eq "each-edge: one pair when asked undirected"
            (length (ee-edges ee-sym false)) 1)

(print-results)
