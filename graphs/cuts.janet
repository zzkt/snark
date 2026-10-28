# -*- mode: janet; -*-
# graphs/cuts: cuts, volumes and graph/boundary expansion measures

(import deft :prefix "")
(import ./util :prefix "")
(import ./types :prefix "")


# ==============================================================================
#  Volume and cut size
#
#  A cut C=(S,T) is a partition of V of a graph G=(V,E) into two subsets S and T.
#  The cut-set of a cut C is the set {(u, v) ∈ E | u ∈ S, v ∈ T} of edges that
#  have one endpoint in S and the other endpoint in T.
# ==============================================================================

(define volume
  ```Sum of degrees (or weighted degree) of vertices of graph `g` in `S`.```
  [g :graph s :dynamic] :number
  (var total 0)
  (each v (keys (to-set s))
    (var row 0)
    (each w (neighbours g v)
      (if (= w v) (+= row 2) (++ row)))
    (+= total row))
  total)

(define cut-size
  ```Number of edges with one endpoint in `S` and the other in V \\ S.
Self-loops don't count toward a cut.```
  [g :graph s :dynamic] :number
  (def in-set (to-set s))
  (var count 0)
  (each u (nodes g)
    (each v (neighbours g u)
      (when (and (get in-set u) (not (get in-set v)))
        (++ count))))
  count)


# ==============================================================================
#  Boundaries
# ==============================================================================

(define edge-boundary
  ```List of edges with one endpoint in `S` and the other in V \\ S.```
  [g :graph s :dynamic] :array
  (let [in-set (to-set s)
        out @[]]
    (each u (keys in-set)
      (each v (neighbours g u)
        (when (not (get in-set v))
          (array/push out [u v]))))
    out))

(define node-boundary
  ```Set of nodes outside `S` with at least one neighbour inside `S`.```
  [g :graph s :dynamic] :array
  (let [in-set (to-set s)
        out @[]]
    (each u (keys in-set)
      (each v (neighbours g u)
        (when (and (not (get in-set v)) (not (arr-contains? out v)))
          (array/push out v))))
    out))


# ==============================================================================
#  Expansion measures
# ==============================================================================

(define edge-expansion
  "edge_boundary(S) / |S|, or nil if none exist."
  [g :graph s :dynamic] :number-or-nil
  (def size (length (keys (to-set s))))
  (if (= size 0)
    nil
    (/ (length (edge-boundary g s)) size)))


(define node-expansion
  "node_boundary(S) / |S|, or nil if none exist."
  [g :graph s :dynamic] :number-or-nil
  (def size (length (keys (to-set s))))
  (if (= size 0)
    nil
    (/ (length (node-boundary g s)) size)))


(define mixing-expansion
  ```cut_size(S) / min(volume(S), volume(V\\S)).
Returns nil if either volume is zero.```
  [g :graph s :dynamic] :number-or-nil
  (let [total (volume g s)
        comp (volume g (set-minus (node-set g) (to-set s)))]
    (if (or (= total 0) (= comp 0))
      nil
      (/ (cut-size g s) (min total comp)))))

(define normalized-cut-size
  ```cut_size(S) * (1/volume(S) + 1/volume(V\\S)).
Returns nil on zero volume.```
  [g :graph s :dynamic] :number-or-nil
  (let [total (volume g s)
        comp (volume g (set-minus (node-set g) (to-set s)))]
    (if (or (= total 0) (= comp 0))
      nil
      (* (cut-size g s) (+ (/ 1 total) (/ 1 comp))))))

(define conductance
  ```cut_size(S) / min(volume(S), volume(V\\S)), with padding of min(1, ...).
A graph with only one non-empty partition has conductance 0.```
  [g :graph s :dynamic] :number-or-nil
  (let [total (volume g s)
        comp (volume g (set-minus (node-set g) (to-set s)))]
    (if (or (= total 0) (= comp 0))
      nil
      (min 1 (/ (cut-size g s) (min total comp))))))

(define boundary-expansion
 ```node_boundary(S) / min(|S|, |V\\S|).
Returns nil if either side is empty.```
  [g :graph s :dynamic] :number-or-nil
  (let [in-set (to-set s)
        out-set (set-minus (node-set g) in-set)
        sz-in (length (keys in-set))
        sz-out (length (keys out-set))]
    (if (or (= sz-in 0) (= sz-out 0))
      nil
      (/ (length (node-boundary g s)) (min sz-in sz-out)))))
