# -*- mode: janet; -*-
# graphs/layout/tree: rooted BFS-tree layout for trees and forests

(import deft :prefix "")
(import ../types :prefix "")
(import ../util :prefix "")
(import ./common :prefix "")
(import ../record :prefix "r-")

(define layout-tree
  ```Deterministic rooted tree layout using BFS layering.

Finds a root heuristically (or given by :root), layers nodes by BFS distance,
and positions them in rows. Works on any graph, produces best results for trees.

   &keys
     :root     — node name to use as root (default: heuristic)
     :width    — canvas width  (default 10.0)
     :height   — canvas height (default 8.0)
     :hspacing — horizontal spacing between siblings (default auto)
     :vspacing — vertical spacing between layers (default auto)

   Returns {:pos {node [x y]} :eligible true :root node}
```
  [g :weighted & keys] :table
  (let [keys (table ;keys)
        nodes (canonical-nodes (r-nodes g))
        n (length nodes)
        width (get keys :width 10.0)
        height (get keys :height 8.0)]
    (cond
      (<= n 0) @{:pos @{} :eligible true}
      (= n 1) @{:pos @{(nodes 0) [(/ width 2) (/ height 2)]}
                :eligible true :root (nodes 0)}
      (let [root (get keys :root (_tree-roots g))
            layers (_bfs-layers g root)
            n-layers (length layers)
            vspacing (get keys :vspacing (/ (- height 1.0) (max 1 (- n-layers 1))))
            top-y 0.5
            pos @{}]
        (loop [li :range [0 n-layers]]
          (let [layer (layers li)
                nn (length layer)
                layer-y (+ top-y (* li vspacing))
                hspacing (get keys :hspacing (/ (- width 1.0) (max 1 (- nn 1))))]
            (loop [i :range [0 nn]]
              (def node-name (layer i))
              (def x (if (= nn 1)
                       (/ width 2)
                       (+ 0.5 (* i hspacing))))
              (put pos node-name [x layer-y]))))
        @{:pos pos :eligible true :root root}))))
