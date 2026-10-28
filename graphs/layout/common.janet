# -*- mode: janet; -*-
# graphs/layout/common: constants and helpers shared by layout algorithms

(import deft :prefix "")
(import ../util :prefix "")
(import ../record :prefix "r-")

(def _pi math/pi)

(defn _unique-edges
  "Distinct structural edges from adjacency, loops excluded."
  [g]
  (structural-edges-for g (structurally-directed? g)))

(defn _orientation
  "Signed orientation of the ordered triple (p q r)."
  [p q r]
  (let [px (p 0) py (p 1) qx (q 0) qy (q 1) rx (r 0) ry (r 1)]
    (- (* (- qx px) (- ry py)) (* (- qy py) (- rx px)))))

(defn _on-segment?
  "True when r lies on the closed segment pq."
  [p q r]
  (and (<= (min (p 0) (q 0)) (r 0) (max (p 0) (q 0)))
       (<= (min (p 1) (q 1)) (r 1) (max (p 1) (q 1)))))

(defn _segments-cross?
  "Segment intersection predicate, including endpoint contacts."
  [a b c d]
  (let [o1 (_orientation a b c)
        o2 (_orientation a b d)
        o3 (_orientation c d a)
        o4 (_orientation c d b)
        d1 (* o1 o2)
        d2 (* o3 o4)]
    (cond
      (and (< d1 0) (< d2 0)) true
      (and (= o1 0) (_on-segment? a b c)) true
      (and (= o2 0) (_on-segment? a b d)) true
      (and (= o3 0) (_on-segment? c d a)) true
      (and (= o4 0) (_on-segment? c d b)) true
      true false)))

(defn- _argmax
  "Node of `nodes` maximizing (key node). Tie broken by node-cmp (name order)."
  [nodes key]
  (reduce (fn [best u]
            (if (or (> (key u) (key best))
                    (and (= (key u) (key best))
                         (< (node-cmp u best) 0)))
              u best))
          (nodes 0)
          nodes))

(defn _tree-roots
  "Heuristic root selection by max degree node (tie break by name).
For directed graphs, prefer nodes with in-degree 0 (sources)."
  [g]
  (let [nodes (canonical-nodes (r-nodes g))
        n (length nodes)]
    (cond
      (= n 0) nil
      (= n 1) (nodes 0)
      (let [directed (structurally-directed? g)
            in-deg @{}
            out-deg @{}
            total-deg @{}]
        (each u nodes (do (put in-deg u 0)
                          (put out-deg u 0)
                          (put total-deg u 0)))
        (loop [[u tbl] :pairs (r-adjacency g)]
          (loop [[v _] :pairs tbl]
            (when (not= u v)
              (put out-deg u (+ (get out-deg u) 1))
              (put in-deg v (+ (get in-deg v) 1))
              (put total-deg u (+ (get total-deg u) 1))
              (put total-deg v (+ (get total-deg v) 1)))))
        # for directed: prefer a source (in-degree 0) with max out-degree
        (let [sources (seq [u :in nodes
                            :when (= (get in-deg u) 0)]
                           u)]
          (if (and directed (pos? (length sources)))
            (_argmax sources |(get out-deg $))
            (_argmax nodes |(get total-deg $))))))))

(defn _bfs-layers
  "BFS from root, return array of layers (each layer = array of nodes).
For directed graphs, follow outgoing edges only."
  [g root]
  (let [directed (structurally-directed? g)
        visited @{root true}
        layers @[@[root]]]
    (var frontier @[root])
    (while (> (length frontier) 0)
      (def next-frontier @[])
      (each u frontier
        (loop [[v _] :pairs (or (get (r-adjacency g) u) @{})]
          (when (and (not (get visited v))
                     (or (not directed) (get-in (r-adjacency g) [u v])))
            (put visited v true)
            (array/push next-frontier v))))
      (when (> (length next-frontier) 0)
        (array/push layers next-frontier))
      (set frontier next-frontier))
    # add any unvisited nodes (disconnected components) as final layer
    (let [remaining (seq [u :in (canonical-nodes (r-nodes g))
                             :when (not (get visited u))]
                     u)]
      (when (> (length remaining) 0)
        (array/push layers remaining)))
    layers))
