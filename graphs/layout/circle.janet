# -*- mode: janet; -*-
# graphs/layout/circle: nodes placed on a circle (cycles, de Bruijn graphs)

(import deft :prefix "")
(import ../types :prefix "")
(import ../util :prefix "")
(import ./common :prefix "")
(import ../record :prefix "r-")

(define layout-circle
```Deterministic circular layout.

Good for cyclic graphs (de Bruijn, Eulerian cycles, etc.) and small FSMs.
Nodes are sorted canonically and placed on a circle at equal angles.

 &keys
  :width  — canvas width  (default 10.0)
  :height — canvas height (default 10.0)
  :radius — circle radius (default: min(w,h)*0.4)

Returns {:pos {node [x y]} :eligible true}
```
  [g :weighted & keys] :table
  (let [keys (table ;keys)
        nodes (canonical-nodes (r-nodes g))
        n (length nodes)
        width (get keys :width 10.0)
        height (get keys :height 10.0)
        radius (get keys :radius (* (min width height) 0.4))]
    (cond
      (<= n 0) @{:pos @{} :eligible true}
      (= n 1) @{:pos @{(nodes 0) [(/ width 2) (/ height 2)]} :eligible true}
      (let [cx (/ width 2)
            cy (/ height 2)
            step (/ (* 2 _pi) n)
            pos @{}]
        (loop [i :range [0 n]]
          (def angle (- (* step i) (/ _pi 2)))
          (put pos (nodes i) [(+ cx (* radius (math/cos angle)))
                              (+ cy (* radius (math/sin angle)))]))
        @{:pos pos :eligible true}))))
