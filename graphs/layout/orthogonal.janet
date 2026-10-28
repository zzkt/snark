# -*- mode: janet; -*-
# graphs/layout/orthogonal: grid placement with orthogonal edge routing

(import deft :prefix "")
(import ../types :prefix "")
(import ../util :prefix "")
(import ./common :prefix "")
(import ../record :prefix "r-")

(define layout-orthogonal
  ```Deterministic orthogonal layout: nodes on a grid.

Inspired by the orthogonal layouts in Handbook of Graph Drawing and
Visualization. Nodes are placed on a grid so edges can be drawn with
horizontal and vertical segments. Good for sparse graphs, flow networks,
and entity-relationship diagrams.

   &keys
     :root     — root node for BFS ordering (default: heuristic)
     :width    — canvas width  (default 10.0)
     :height   — canvas height (default 8.0)
     :hspacing — horizontal spacing between columns (default 2.0)
     :vspacing — vertical spacing between rows (default 1.5)

Returns {:pos {node [x y]} :eligible true :root node :orthogonal true}
```
  [g :weighted & keys] :table
  (let [keys (table ;keys)
        nodes (canonical-nodes (r-nodes g))
        n (length nodes)
        width (get keys :width 10.0)
        height (get keys :height 8.0)
        hspacing-opt (get keys :hspacing)
        vspacing-opt (get keys :vspacing)]
    (cond
      (<= n 0) @{:pos @{} :eligible true}
      (= n 1) @{:pos @{(nodes 0) [(/ width 2) (/ height 2)]}
                :eligible true}
      (let [root (get keys :root (_tree-roots g))

            layers (_bfs-layers g root)]
        (let [n-layers (length layers)
                max-width (reduce max 1 (map |(length $) layers))
                hspacing (or hspacing-opt (/ (- width 2.0) (max 1 (- max-width 1))))
                vspacing (or vspacing-opt (/ (- height 2.0) (max 1 (- n-layers 1))))
                cx (/ width 2)
                top-y 1.0
                pos @{}]
            # place root at top center
            (put pos root [cx top-y])
            (loop [li :range [1 n-layers]]
              (let [layer (layers li)
                    nn (length layer)
                    layer-y (+ top-y (* li vspacing))
                    # spread horizontally centered
                    start-x (- cx (/ (* (- nn 1) hspacing) 2))]
                (loop [i :range [0 nn]]
                  (put pos (layer i) [(+ start-x (* i hspacing)) layer-y]))))
            @{:pos pos :eligible true :root root :orthogonal true})))))
