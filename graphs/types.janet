# -*- mode: janet; coding: utf-8; -*-
# graphs/types: type aliases for combinations of graph types.

(import deft :prefix "")
(import ./simple-graph :prefix "")
(import ./digraph :prefix "")
(import ./multigraph :prefix "")
(import ./weighted-graph :prefix "")
(import ./tree :prefix "")

# Unions of the concrete graph record types.

(deftype :any-graph
  (or :graph :multigraph :digraph :multidigraph :weighted-graph
      :tree :weighted-tree))

(deftype :directed
  (or :digraph :multidigraph))

(deftype :undirected
  (or :graph :multigraph))

(deftype :weighted
  (or :digraph :weighted-graph))

(deftype :node-label
   (or :string :integer))

# Common return types

(deftype :number-or-nil
  (or :number :nil))

(deftype :integer-or-nil
  (or :integer :nil))

(deftype :array-or-nil
  (or :array :nil))

(deftype :table-or-nil
  (or :table :nil))

# Not yet
# - bipartite
# - planar
