# -*- mode: janet; -*-
# graphs/layout/radial: concentric ring layout around a root

(import deft :prefix "")
(import ../types :prefix "")
(import ../util :prefix "")
(import ./common :prefix "")
(import ../record :prefix "r-")

(define layout-radial
  ```Deterministic radial layout with subtree sectors on concentric circles.

 A root is placed at the center and each BFS layer forms a concentric ring.
 Children are allocated angular sectors proportional to subtree leaf counts.

 Good for trees, phylogenetic diagrams, and hub-and-spoke networks.

   &keys
     :root      — center node (default: heuristic)
     :width     — canvas width  (default 10.0)
     :height    — canvas height (default 10.0)
     :spacing   — radial distance between layers (default 2.0)
     :arc       — minimum arc length per node (default 1.2)

   Returns {:pos {node [x y]} :eligible true :root node}
```
  [g :weighted & keys] :table
  (let [keys (table ;keys)
        nodes (canonical-nodes (r-nodes g))
        n (length nodes)
        width (get keys :width 10.0)
        height (get keys :height 10.0)
        spacing (get keys :spacing 2.0)
        arc (get keys :arc 1.2)
        directed (structurally-directed? g)]
    (cond
      (<= n 0) @{:pos @{} :eligible true}
      (= n 1) @{:pos @{(nodes 0) [(/ width 2) (/ height 2)]}
                :eligible true :root (nodes 0)}
      (let [root (get keys :root (_tree-roots g))
            layers (_bfs-layers g root)
            n-layers (length layers)
            depth @{}
            min-radius @{}
            order @[root]
            parent @{root nil}
            children @{}
            seen @{root true}
            weights @{}
            cx (/ width 2)
            cy (/ height 2)
            pos @{}
            span @{root [0 (* 2 _pi)]}
            unplaced @[]]
        # BFS depth of every node
        (loop [li :range [0 n-layers]]
          (each name (layers li) (put depth name li)))
        # minimum ring radius per layer. nodes keep :arc angular spacing
        (loop [li :range [0 n-layers]]
          (put min-radius li
               (max (* spacing li) (/ (* (length (layers li)) arc) (* 2 _pi)))))
        (loop [li :range [1 n-layers]]
          (put min-radius li
               (max (get min-radius li)
                    (+ (get min-radius (- li 1)) spacing))))
        # BFS spanning tree rooted at root. traversal order, parents, children
        (var qi 0)
        (while (< qi (length order))
          (def u (order qi))
          (def ch @[])
          (loop [[v _] :pairs (or (get (r-adjacency g) u) @{})]
            (when (and (not (get seen v))
                       (or (not directed) (get-in (r-adjacency g) [u v])))
              (put seen v true)
              (put parent v u)
              (array/push ch v)))
          (put children u ch)
          (each v ch (array/push order v))
          (++ qi))
        # subtree leaf counts (post-order over BFS tree)
        (loop [i :down [(- (length order) 1) 0]]
          (def u (order i))
          (def ch (get children u))
          (if (zero? (length ch))
            (put weights u 1)
            (put weights u
                 (reduce (fn [acc c] (+ acc (get weights c))) 0 ch))))
        # place the root at the centre, then each node at centre of its wedge
        (put pos root [cx cy])
        (each u order
          (def [a0 a1] (get span u))
          (unless (= u root)
            (def r (get min-radius (get depth u)))
            (def mid (/ (+ a0 a1) 2))
            (put pos u [(+ cx (* r (math/cos mid)))
                        (+ cy (* r (math/sin mid)))]))
          (def ch (get children u))
          (when (> (length ch) 0)
            (def total-w (reduce (fn [acc c] (+ acc (get weights c))) 0 ch))
            (var acc a0)
            (each ci ch
              (def slice (/ (* (- a1 a0) (get weights ci)) total-w))
              (put span ci [acc (+ acc slice)])
              (set acc (+ acc slice)))))
        # disconnected nodes: evenly spaced on a ring past the last layer
        (each u nodes (unless (get seen u) (array/push unplaced u)))
        (when (> (length unplaced) 0)
          (def nn (length unplaced))
          (def r-rem (max (* spacing n-layers) (/ (* nn arc) (* 2 _pi))))
          (loop [i :range [0 nn]]
            (def a (* (/ (* 2 _pi) nn) i))
            (put pos (unplaced i) [(+ cx (* r-rem (math/cos a)))
                                   (+ cy (* r-rem (math/sin a)))])))
        @{:pos pos :eligible true :root root}))))
