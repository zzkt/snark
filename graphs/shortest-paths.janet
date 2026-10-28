# -*- mode: janet; -*-
# graphs/shortest-paths: shortest paths via BFS for all graph types

(import deft :prefix "")
(import ./types :prefix "")
(import ./simple-graph :prefix "")
(import ./util :prefix "")
(import ./metrics :prefix "_mt_")

(define graph-shortest-path-lengths
  ```All-pairs shortest path lengths.
Returns table of tables {source {target length}}.```
  [g :any-graph] :table
  (_mt_all-pairs-distances g))

(define graph-diameter
  "Diameter of the graph (longest shortest path).
Returns 0 for the empty graph, or nil when a non-empty graph is disconnected."
  [g :any-graph] :integer-or-nil
  (_mt_graph-diameter g))

(define graph-eccentricity
  "Eccentricity of each node (max shortest path from that node).
   Returns table {node eccentricity}."
  [g :any-graph] :table
  (_mt_eccentricities-scan g))

(defn- extremal-eccentricity-nodes
  "Nodes whose eccentricity is `better` than every other's, seeded with `init`."
  [g init better]
  (let [ecc (graph-eccentricity g)
        nodes (keys ecc)]
    (when (empty? nodes) (break @[]))
    (var best init)
    (each n nodes
      (when (better (in ecc n) best) (set best (in ecc n))))
    (filter (fn [n] (= (in ecc n) best)) nodes)))

(define graph-center
  ```Nodes with minimum eccentricity.

Minimises over every vertex, including isolated nodes (i.e. eccentricity 0)
which means it does not always agree with `graph-radius`.
```
  [g :any-graph] :array
  (_mt_graph-center g))

(define graph-periphery
  "Nodes with maximum eccentricity."
  [g :any-graph] :array
  (extremal-eccentricity-nodes g 0 >))
