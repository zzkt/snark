# -*- mode: janet; -*-
# graphs/clustering: clusters and community detection

(import deft :prefix "")
(import ./digraph :prefix "")
(import ./util :prefix "")


(define total-weight
  "Total weight of all arcs (including self-arcs of aggregated super-nodes)."
  [g :digraph] :number
  (sum (seq [[_ adj] :pairs (g :adj) [_ w] :pairs adj] w)))

(define node-weight
  ```Total arc weight leaving node `u` (modularity volume).

Includes any self-arcs. Aggregated super-nodes represent internal arc weight
as a self-loop.```
  [g :digraph u :dynamic] :number
  (def adj (get (g :adj) u))
  (if adj (sum (values adj)) 0.0))

(define community-improve
```One pass of local moving.

Every node moves to the neighbouring community giving the highest positive
modularity gain. The arc weight is ku from u into a community C, Vu for u's
total volume, volC for community volume and tw for the total weight, the net
gain of moving u from C0 to C1 is:

       dQ = 2*(k1 - k0)/tw + 2*Vu*(vol0 - vol1 - Vu)/tw^2

Only positive gains are accepted, modularity rises monotonically.

Returns true when at least one node moved.
```
  [g :digraph nodes :array comm :table comm-weight :table tw :number] :boolean
  (var improved false)
  (each u nodes
    (def cur-c (get comm u))
    (def vu (node-weight g u))
    (var k0 0.0)
    (def neighbour-comms @{})
    (each e (out-neighbours g u)
      (def v (first e))
      (when (not= v u)
        (def c (get comm v))
        (if (= c cur-c)
          (set k0 (+ k0 (get e 1)))
          (put neighbour-comms c (+ (or (get neighbour-comms c) 0.0)
                                    (get e 1))))))
    (def vol0 (get comm-weight cur-c))
    (var best-c cur-c)
    (var best-gain 0.0)
    (loop [[c k1] :pairs neighbour-comms]
      (def vol1 (get comm-weight c))
      (def net (+ (/ (* 2.0 (- k1 k0)) tw)
                  (/ (* 2.0 vu (- vol0 vol1 vu)) (* tw tw))))
      (when (and (> net best-gain) (> net 0.0))
        (set best-gain net)
        (set best-c c)))
    (when (not= best-c cur-c)
      (set improved true)
      (put comm u best-c)
      (put comm-weight best-c (+ (get comm-weight best-c) vu))
      (put comm-weight cur-c (- (get comm-weight cur-c) vu))))
  improved)


(define aggregate
  ```Collapse each community of `comm` into a single super-node.

Intercommunity arc weights are kept, the internal arc weight of each community
is stored as a self-loop on its super-node.

Returns [g2 comm->name]
```
  [g :digraph comm :table] :tuple
  (let [comms (distinct (values comm))
        name-of @{}
        g2 (make-digraph @[] @{})]
    (each c comms
      (def nm (string c))
      (put name-of c nm)
      (digraph-add-node g2 nm))
    (def w @{})
    (def self @{})
    (each u (nodes g)
      (def nmu (get name-of (get comm u)))
      (each e (out-neighbours g u)
        (def v (first e))
        (def nmv (get name-of (get comm v)))
        (if (= nmu nmv)
          (put self nmu (+ (or (get self nmu) 0.0) (get e 1)))
          (do
            (def row (or (get w nmu) @{}))
            (put row nmv (+ (or (get row nmv) 0.0) (get e 1)))
            (put w nmu row)))))
    (each [nm s] (pairs self)
      (digraph-add-edge g2 nm nm s))
    (each [nmi row] (pairs w)
      (each [nmj wt] (pairs row)
        (digraph-add-edge g2 nmi nmj wt)))
    [g2 name-of]))


(define init-communities
  "Initializes singleton communities and their volume sums."
  [g :digraph nodes :array] :tuple
  (def comm
    (merge @{} ;(seq [[i node] :pairs nodes] {node i})))
  (def comm-weight
    (merge @{} ;(seq [[i node] :pairs nodes]
                     {i (node-weight g node)})))
  [comm comm-weight])


(define louvain
  ```Multi-level Louvain community detection on a symmetric digraph.

Each undirected edge is supplied in both directions. Every accepted move
increases modularity, so the returned partition is a local optimum.

Each level runs local moving recursively until no node can improve modularity
and aggregates the resulting communities into self-weighted super-nodes.

Returns {node: community-id}
```
  [g :digraph] :table
  (def nodes (nodes g))
  (def tw (total-weight g))
  (def [comm comm-weight] (init-communities g nodes))
  (if (> tw 0.0)
    (do
      (var moved true)
      (while moved
        (set moved (community-improve g nodes comm comm-weight tw)))
      (if (< (length (distinct (values comm))) (length nodes))
        (let [[g2 name-of] (aggregate g comm)]
          (def inner (louvain g2))
          (merge @{} ;(seq [[u c] :pairs comm]
                           {u (get inner (get name-of c))})))
        comm))
    comm))


(define leiden
  ```Multi-level Leiden community detection.

Each Louvain community detection pass is followed by a refinement pass of local
moving on the final partition, repairing nodes locked into suboptimal communities
during aggregation.

Returns {node: community-id}
```
  [g :digraph] :table
  (let [community (louvain g)
        nodes (nodes g)
        tw (total-weight g)]
    (def community-weight @{})
    (loop [[c us] :pairs (group-by (fn [u] (get community u)) nodes)]
      (put community-weight c (sum (map (fn [u] (node-weight g u)) us))))
    (community-improve g nodes community community-weight tw)
    community))
