# -*- mode: janet; -*-
# graphs/layout/force: Fruchterman-Reingold force-directed layout

(import deft :prefix "")
(import ../types :prefix "")
(import ../util :prefix "")
(import ./common :prefix "_cm_")
(import ../record :prefix "r-")
(def _pi math/pi)

(defn- _norm
  "Euclidean norm of a 2-vector."
  [dx dy]
  (math/sqrt (+ (* dx dx) (* dy dy))))

# ==============================================================================
#  Fruchterman-Reingold force-directed layout
# ==============================================================================

(define layout-force
  ```Deterministic force-directed layout (Fruchterman-Reingold).

   Positions nodes so connected nodes are close and non-connected nodes
   are far apart, using repulsive and attractive forces.

   &keys
     :width       — canvas width  (default 10.0)
     :height      — canvas height (default 10.0)
     :iterations  — cooling iterations (default 300)
     :gravity     — pull toward center strength (default 0.05)
     :edge-weight — weight for attractive forces (default 1.0)
     :cooling     — cooling schedule is "linear" or "exponential" (default)
     :seed        — random seed for deterministic jitter (default 0)

   Returns {:pos {node [x y]} :eligible true :iterations n :k float}
```
  [g :weighted & keys] :table
  (let [keys (table ;keys)
        nodes (canonical-nodes (r-nodes g))
        n (length nodes)
        width (get keys :width 10.0)
        height (get keys :height 10.0)
        iterations (get keys :iterations 300)
        gravity (get keys :gravity 0.05)
        edge-weight (get keys :edge-weight 1.0)
        cooling (get keys :cooling "exponential")
        seed (get keys :seed 0)]
    (cond
      (<= n 0) @{:pos @{} :eligible true :iterations 0 :k 0.0}
      (= n 1) @{:pos @{(nodes 0) [(/ width 2) (/ height 2)]}
                :eligible true :iterations 0 :k 0.0}
      (let [step (/ (* 2 _pi) n)
            area (* width height)
            k (math/sqrt (/ area n))
            cx (/ width 2)
            cy (/ height 2)
            radius (* (min width height) 0.4)
            t0 (/ width 10)
            pos @{}
            edges (_cm__unique-edges g)
            disp (merge @{} ;(seq [u :in nodes] {u @[0.0 0.0]}))]
        # initial positions on circle with small jitter
        (math/seedrandom seed)
        (loop [i :range [0 n]]
          (def angle (* step i))
          (def jitter (* 0.05 (math/random)))
          (put pos (nodes i) [(+ cx (* radius (math/cos (+ angle jitter))))
                              (+ cy (* radius (math/sin (+ angle jitter))))]))
        # iterative force calculation
        (var temp t0)
        (loop [iter :range [0 iterations]]
          (each u nodes (let [d (disp u)] (put d 0 0) (put d 1 0)))
          # repulsive forces: act on all pairs
          (loop [i :range [0 n]]
            (loop [j :range [(+ i 1) n]]
              (let [u (nodes i)
                    v (nodes j)
                    pu (pos u)
                    pv (pos v)
                    dx (- (pu 0) (pv 0))
                    dy (- (pu 1) (pv 1))]
                (var dist (_norm dx dy))
                (when (< dist 0.01) (set dist 0.01))
                (let [force (/ (* k k) dist)
                      fx (* force (/ dx dist))
                      fy (* force (/ dy dist))
                      du (disp u)
                      dv (disp v)]
                  (put du 0 (+ (du 0) fx))
                  (put du 1 (+ (du 1) fy))
                  (put dv 0 (- (dv 0) fx))
                  (put dv 1 (- (dv 1) fy))))))
          # attractive forces: edges only, weighted by edge-weight
          (each edge edges
            (let [u (edge 0)
                  v (edge 1)
                  pu (pos u)
                  pv (pos v)
                  dx (- (pu 0) (pv 0))
                  dy (- (pu 1) (pv 1))
                  ew edge-weight]
              (var dist (_norm dx dy))
              (when (< dist 0.01) (set dist 0.01))
              (let [force (/ (* dist dist ew) k)
                    fx (* force (/ dx dist))
                    fy (* force (/ dy dist))
                    du (disp u)
                    dv (disp v)]
                (put du 0 (- (du 0) fx))
                (put du 1 (- (du 1) fy))
                (put dv 0 (+ (dv 0) fx))
                (put dv 1 (+ (dv 1) fy)))))
          # gravity: pull toward center
          (when (> gravity 0)
            (each u nodes
              (let [pu (pos u)
                    dx (- (pu 0) cx)
                    dy (- (pu 1) cy)
                    du (disp u)]
                (put du 0 (+ (du 0) (* gravity dx)))
                (put du 1 (+ (du 1) (* gravity dy))))))
          # apply displacements with temperature clamping
          (each u nodes
            (def du (disp u))
            (def dist (_norm (du 0) (du 1)))
            (when (> dist 0.01)
              (def scale (min 1 (/ temp dist)))
              (def pu (pos u))
              (def new-x (+ (pu 0) (* (du 0) scale)))
              (def new-y (+ (pu 1) (* (du 1) scale)))
              # clamp to canvas with margin
              (put pos u [(max 0.5 (min (- width 0.5) new-x))
                           (max 0.5 (min (- height 0.5) new-y))])))
          # cooling: exponential or linear
          (if (= cooling "exponential")
            (set temp (* t0 (math/exp (- (* 3 (/ (+ iter 1) iterations))))))
            (set temp (* t0 (- 1 (/ (+ iter 1) iterations))))))
        @{:pos pos :eligible true :iterations iterations :k k}))))
