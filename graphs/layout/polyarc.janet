# -*- mode: janet; -*-
# graphs/layout/polyarc: poly-arc Lombardi drawing by tangent-slot construction

#  A Lombardi drawing, after Mark Lombardi, as formalised by Duncan et. al.,
#  draws every edge as a circular arc, with the incident tangents at a vertex
#  equally spaced at 2*pi/deg(v). Not every graph has a single-arc drawing,
#  but every planar graph can be drawn with poly-arcs.
#
#    - Start from a base drawing :spring (default), :circle, :spectral,
#      :radial, :random or user-supplied positions.
#    - Order each vertex's incident edges circularly and give each a slot.
#      slot i has tangent direction i*2*pi/deg(v).
#    - For edge uv, if the unique circle through u and v, tangent to u's slot
#      direction, is also tangent to v's slot direction (within :tol), draw it
#      as that circle. Otherwise draw two arcs meeting at a joint.
#
#  The two-arc join relies on the arc locus, arcs leaving p at theta_p and q at
#  theta_q meet at a prescribed theta_pq on a circle of r = d_pq*csc(alpha)/2
#  where  alpha = (theta_p - theta_q - theta_pq)/2
#
#  With the default meeting angle of π the arcs should meet smoothly (creating
#  a smooth 2-Lombardi drawing)
#
#  References
#  - Lombardi Drawings of Graphs. arXiv:1009.0579
#  - Planar and Poly-Arc Lombardi Drawings. arXiv:1109.0345

(import deft :prefix "")
(import ../types :prefix "")
(import ../util :prefix "")
(import ../planarity :prefix "_pl_")
(import ./spectral :prefix "_s_")
(import ./radial :prefix "_r_")
(import ./spring :prefix "_sp_")
(import ./common :prefix "_cm_")

# ==============================================================================
#  Angles
# ==============================================================================

(def two-pi (* 2 math/pi))

(defn _wrap
  "Wrap an angle into [0, 2*pi)."
  [a] (mod a two-pi))

(defn _line-angle
  "Fold a directed angle into a tangent-line angle in [0, pi)."
  [a]  (mod a math/pi))

(defn _delta
  "Signed difference b - a folded into (-pi, pi]."
  [a b]
  (var d (- b a))
  (when (> d math/pi) (set d (- d two-pi)))
  (when (< d (- math/pi)) (set d (+ d two-pi)))
  d)


(defn _line-diff
  "Signed difference between tangent-line angles folded into (-pi/2, pi/2]."
  [a b]
  (def d (_delta a b))
  (cond
    (> d (/ math/pi 2)) (- d math/pi)
    (< d (- (/ math/pi 2))) (+ d math/pi)
    true d))

# ==============================================================================
#  Edge enumeration
# ==============================================================================

(defn _degrees
  "Degree of every endpoint of the edge list."
  [edges]
  (def deg @{})
  (each e edges
    (put deg (e 0) (+ (get deg (e 0) 0) 1))
    (put deg (e 1) (+ (get deg (e 1) 0) 1)))
  deg)

# ==============================================================================
#  Tangent slots
# ==============================================================================

(defn _slot-table
  ```Circular order of incident edges at every vertex from initial drawing.

Returns {vertex {edge-index slot}} where the tangent direction of slot i at v
is i * 2*pi/deg(v), slot 0 is the edge first in (ascending) angular order at v.```
  [pos edges]
  (def order @{})
  (loop [ei :range [0 (length edges)]]
    (let [e (edges ei)
          u (e 0) v (e 1)
          pu (get pos u) pv (get pos v)]
      (when (and pu pv)
        (unless (get order u) (put order u @[]))
        (unless (get order v) (put order v @[]))
        (array/push (order u)
                    @[(math/atan2 (- (pv 1) (pu 1)) (- (pv 0) (pu 0))) ei])
        (array/push (order v)
                    @[(math/atan2 (- (pu 1) (pv 1)) (- (pu 0) (pv 0))) ei]))))
  (def slots @{})
  (each v (keys order)
    (def list (order v))
    (sort list (fn [a b] (< (a 0) (b 0))))
    (def tbl @{})
    (loop [i :range [0 (length list)]]
      (put tbl ((list i) 1) i))
    (put slots v tbl))
  slots)

(defn _radial-centre
  "Centroid of the placed vertices, the centre used by the radial slot frame."
  [pos &opt nodes]
  (def vs (if nodes
            (seq [[v p] :in (pairs pos) :when (get nodes v)] p)
            (values pos)))
  (if (empty? vs)
    @[0.0 0.0]
    @[(/ (reduce (fn [s p] (+ s (p 0))) 0.0 vs) (length vs))
      (/ (reduce (fn [s p] (+ s (p 1))) 0.0 vs) (length vs))]))

(defn _radial-slots
  ```Circular order of incident edges at every vertex, anchored to the inward radius.
Returns {vertex {edge-index slot}}.```
  [pos edges]
  (def c (_radial-centre pos))
  (def order @{})
  (loop [ei :range [0 (length edges)]]
    (let [e (edges ei)
          u (e 0) v (e 1)
          pu (get pos u) pv (get pos v)]
      (when (and pu pv)
        (def ru (math/atan2 (- (c 1) (pu 1)) (- (c 0) (pu 0))))
        (def rv (math/atan2 (- (c 1) (pv 1)) (- (c 0) (pv 0))))
        (def au (math/atan2 (- (pv 1) (pu 1)) (- (pv 0) (pu 0))))
        (def av (math/atan2 (- (pu 1) (pv 1)) (- (pu 0) (pv 0))))
        (unless (get order u) (put order u @[]))
        (unless (get order v) (put order v @[]))
        (array/push (order u) @[(mod (- au ru) two-pi) ei])
        (array/push (order v) @[(mod (- av rv) two-pi) ei]))))
  (def slots @{})
  (each v (keys order)
    (def list (order v))
    (sort list (fn [a b]
                 (if (= (a 0) (b 0)) (< (a 1) (b 1)) (< (a 0) (b 0)))))
    (def tbl @{})
    (loop [i :range [0 (length list)]]
      (put tbl ((list i) 1) i))
    (put slots v tbl))
  slots)

(defn _radial-slot-rot
  "Per-vertex slot-frame rotation that makes slot 0 point at the centre."
  [pos]
  (def c (_radial-centre pos))
  (def rot @{})
  (eachp [v p] pos
         (put rot v (math/atan2 (- (c 1) (p 1)) (- (c 0) (p 0)))))
  rot)

(defn _embedding-slots
  ```Circular order of the incident edges at every vertex, from the planar
embedding's rotation system rather than from position angles.

Returns {vertex {edge-index slot}}, or nil when g has no planar faces or the
rotation system does not cover every incident edge at every vertex.
```
  [g edges]
  (def faces (_pl_planar-faces g))
  (when (and faces (> (length faces) 0))
    (def rot @{})
    (each f faces
      (let [vs (f :vs) k (length vs)]
        (when (>= k 3)
          (loop [i :range [0 k]]
            (let [v (vs i) nxt (vs (mod (+ i 1) k)) prv (vs (mod (- i 1) k))]
              (unless (get rot v) (put rot v @[]))
              (array/push (rot v) @[nxt prv]))))))
    (def order @{})
    (each [v plist] (pairs rot)
      (def next-of @{})
      (each [nxt prv] plist (put next-of nxt prv))
      (let [seen @{} seq2 @[]]
        (each [start _] plist
          (unless (get seen start)
            (var cur start)
            (var guard (length next-of))
            (while (and cur (not (get seen cur)) (> guard 0))
              (put seen cur true)
              (array/push seq2 cur)
              (set cur (get next-of cur))
              (set guard (- guard 1)))))
        (put order v seq2)))
    (def idx @{})
    (loop [ei :range [0 (length edges)]]
      (let [e (edges ei) u (e 0) v (e 1)]
        (unless (get idx u) (put idx u @{}))
        (unless (get idx v) (put idx v @{}))
        (put (get idx u) v ei)
        (put (get idx v) u ei)))
    (def slots @{})
    (each [v seq2] (pairs order)
      (def tbl @{})
      (loop [i :range [0 (length seq2)]]
        (def nb (seq2 i))
        (put tbl (get-in idx [v nb]) i))
      (put slots v tbl))
    (var complete true)
    (eachp [v d] (_degrees edges)
           (def mapping (get slots v))
           (unless (and mapping (= (length mapping) d))
             (set complete false)))
    (when complete slots)))

(defn _slot-angle
  ```Tangent direction (radians) of the slot edge ei occupies at vertex v.
 :slot-rot is either a global rotation (number) or a {vertex rotation}
 table of per-vertex offsets.```
  [slots deg v ei slot-rot]
  (def rot (if (number? slot-rot) slot-rot (get slot-rot v 0.0)))
  (+ rot (* (/ two-pi (get deg v)) (get (get slots v) ei))))

(defn _circle-base
  "Deterministic circle placement for every node of g."
  [g width height]
  (let [pos @{}
        n (length (nodes g))]
    (when (> n 0)
      (let [cx (/ width 2) cy (/ height 2)
            r (* 0.45 (min width height))]
        (loop [i :range [0 n]]
          (def a (- (/ (* two-pi i) n) (/ math/pi 2)))
          (put pos ((nodes g) i) @[(+ cx (* r (math/cos a)))
                                   (+ cy (* r (math/sin a)))]))))
    pos))

(defn _radius-slot-rot
  ```Per-vertex slot-frame rotation anchored on vertex's radius toward circle centre.```
  [pos width height]
  (let [rot @{} cx (/ width 2) cy (/ height 2)]
    (eachp [v p] pos
           (put rot v (math/atan2 (- cy (p 1)) (- cx (p 0)))))
    rot))

# ==============================================================================
#  Edge geometry
# ==============================================================================

(defn _arc-circle
  ```Unique circle through p and q whose tangent line at p is direction alpha.
Returns {:cx cy r cw} with cw true when the traversal from p to q is clockwise.```
  [p q alpha]
  (let [nx (- (math/sin alpha))
        ny (math/cos alpha)
        dx (- (q 0) (p 0))
        dy (- (q 1) (p 1))
        nd (+ (* nx dx) (* ny dy))]
    (if (or (< (+ (* dx dx) (* dy dy)) 1e-12)
            (< (math/abs nd) 1e-12))
      {:line true}
      (do
        (def lam (/ (+ (* dx dx) (* dy dy)) (* 2 nd)))
        {:cx (+ (p 0) (* lam nx))
         :cy (+ (p 1) (* lam ny))
         :r (math/abs lam)
         :cw (< lam 0)
         :line false}))))

(defn _out-angle
  "Directed tangent angle at q of the arc, taken away from q."
  [ac q]
  (let [wx (- (q 0) (ac :cx))
        wy (- (q 1) (ac :cy))]
    (if (ac :cw)
      # clockwise arrival velocity is rot_cw(q - c) = (wy, -wx)
      # outward direction is (-wy, wx)
      (_wrap (math/atan2 wx (- wy)))
      # counter-clockwise arrival velocity is rot_ccw(q - c) = (-wy, wx);
      # outward direction is (wy, -wx)
      (_wrap (math/atan2 (- wx) wy)))))

(defn _locus-joint
  ```Meeting point of the two-arc poly edge (ref. "arcLocus")
Returns [x y].```
  [p q alpha beta meet bend]
  (let [d (math/hypot (- (q 0) (p 0)) (- (q 1) (p 1)))
        sb (- (q 1) (p 1)) cb (- (q 0) (p 0))
        betap (math/atan2 sb cb)
        alphag (/ (- alpha beta meet) 2)
        sc (math/sin alphag)]
    (if (< (math/abs sc) 1e-9)
      @[(/ (+ (p 0) (q 0)) 2) (/ (+ (p 1) (q 1)) 2)]
      (let [rc (/ d (* 2 sc))
            rcabs (math/abs rc)
            cx (+ (p 0) (* rc (math/sin (+ alphag betap))))
            cy (- (p 1) (* rc (math/cos (+ alphag betap))))
            mx (/ (+ (p 0) (q 0)) 2) my (/ (+ (p 1) (q 1)) 2)
            vx (- mx cx) vy (- my cy)
            nv (math/hypot vx vy)]
        (if (> (* d 1e6) (math/abs rc))
          (let [dir (if (> nv 1e-12)
                      (math/atan2 vy vx)
                      (+ betap (/ math/pi 2)))
                phi (+ dir bend)]
            @[(+ cx (* rcabs (math/cos phi)))
              (+ cy (* rcabs (math/sin phi)))])
          @[(/ (+ (p 0) (q 0)) 2) (/ (+ (p 1) (q 1)) 2)])))))

(defn _poly-joint
  ```Splice point for the two-arc poly edge.
Uses the arc-locus meeting point, or the edge midpoint if `join` is :midpoint.
Returns [x y].```
  [p q alpha beta meet bend join]
  (if (= join :midpoint)
    @[(/ (+ (p 0) (q 0)) 2) (/ (+ (p 1) (q 1)) 2)]
    (_locus-joint p q alpha beta meet bend)))

(defn _seg
  "Render an _arc-circle result as a chained segment record ending at `end`."
  [ac end flip]
  (if (ac :line)
    @{:line true :x (end 0) :y (end 1)}
    @{:cx (ac :cx) :cy (ac :cy) :r (ac :r)
      :cw (if flip (not (ac :cw)) (ac :cw))
      :x (end 0) :y (end 1)}))

(defn _basis
  "The :arcs record header shared by every edge kind."
  [u v pu pv]
  @{:u u :v v :x0 (pu 0) :y0 (pu 1) :x1 (pv 0) :y1 (pv 1)})

(defn _seg-sweep
  ```Sweep span (radians) of arc segment `seg` whose chain starts at point `cur`.```
  [seg cur]
  (if (seg :line)
    0.0
    (let [c @[(seg :cx) (seg :cy)]
          ths (math/atan2 (- (cur 1) (c 1)) (- (cur 0) (c 0)))
          the (math/atan2 (- (seg :y) (c 1)) (- (seg :x) (c 0)))]
      (if (seg :cw)
        (mod (- ths the) two-pi)
        (mod (- the ths) two-pi)))))

(defn _record-max-sweep
  ```Largest arc-segment sweep span (radians) traversing segment chain from (x0 y0).```
  [rec]
  (var cur @[(rec :x0) (rec :y0)])
  (var mx 0.0)
  (each seg (rec :segments)
    (set mx (max mx (_seg-sweep seg cur)))
    (set cur @[(seg :x) (seg :y)]))
  mx)

(defn _record-curvature
  [rec]
  (var cur @[(rec :x0) (rec :y0)])
  (var total 0.0)
  (each seg (rec :segments)
    (set total (+ total (_seg-sweep seg cur)))
    (set cur @[(seg :x) (seg :y)]))
  total)

(defn- _order-better?
  "Does the new improve upon the old?"
  [old-cap new-cap old-total new-total old-cg old-cgd new-cg new-cgd
   old-curvature new-curvature old-energy new-energy]
  (or
    (and old-cap (not new-cap))
    (and old-cap new-cap
         (or (< new-total old-total)
             (and (= new-total old-total) (< new-cg old-cg))
             (and (= new-total old-total) (= new-cg old-cg) (< new-cgd old-cgd))
             (and (= new-total old-total) (= new-cg old-cg) (= new-cgd old-cgd)
                  (< new-curvature old-curvature))
             (and (= new-total old-total) (= new-cg old-cg) (= new-cgd old-cgd)
                  (= new-curvature old-curvature)
                  (< new-energy old-energy))))
    (and (not old-cap) (not new-cap)
         (or (< new-total old-total)
             (and (= new-total old-total) (< new-cg old-cg))
             (and (= new-total old-total) (= new-cg old-cg) (< new-cgd old-cgd))
             (and (= new-total old-total) (= new-cg old-cg) (= new-cgd old-cgd)
                  (< new-curvature old-curvature))
             (and (= new-total old-total) (= new-cg old-cg) (= new-cgd old-cgd)
                  (= new-curvature old-curvature)
                  (< new-energy old-energy))))))

(defn _edge-record
  ```An :arcs record for the edge u-v.

tangent :slot-alpha at u toward v and :slot-tgt at v toward u. A single circle
when the arc realises both tangent directions (within :tol), otherwise a two-arc
poly path meeting at the arc-locus joint. The :meet angle default of pi gives a
smooth C1 joint (ref. smooth 2-Lombardi corollary) or, when :joint is :midpoint,
the edge-midpoint splice. :bend rotates the locus joint around its circle.

Each segment is {:cx cy r cw x y} (arc) or {:line true x y} (straight), chaining
from the start point to (x y).
```
  [u v pu pv alpha tgt tol meet bend join &opt max-sweep line-mode]
  (def ac (_arc-circle pu pv alpha))
  (if (ac :line)
    (let [chord (_line-angle (math/atan2 (- (pv 1) (pu 1)) (- (pv 0) (pu 0))))
          udev (math/abs (_line-diff chord (_line-angle alpha)))
          vdev (math/abs (_line-diff chord (_line-angle tgt)))
          rec (_basis u v pu pv)]
      (put rec :kind :line)
      (put rec :residual (if (> vdev udev) vdev udev))
      (put rec :segments @[{:line true :x (pv 0) :y (pv 1)}])
      rec)
    (do
      (let [residual (if line-mode
                       (math/abs (_line-diff
                                  (_line-angle tgt)
                                  (_line-angle (_out-angle ac pv))))
                       (math/abs (_delta tgt (_out-angle ac pv))))
            rec (_basis u v pu pv)
            seg (_seg ac pv false)]
        (if (and (<= residual tol)
                 (or (nil? max-sweep)
                     (<= (_seg-sweep seg pu) max-sweep)))
          (do (put rec :kind :arc)
              (put rec :residual residual)
              (put rec :segments @[seg])
              rec)
          (let [P (_poly-joint pu pv alpha tgt meet bend join)]
            (put rec :kind :poly)
            (put rec :residual residual)
            (put rec :joint P)
            (put rec :segments @[(_seg (_arc-circle pu P alpha) P false)
                                 (_seg (_arc-circle pv P tgt) pv true)])
            rec))))))

# ==============================================================================
#  Crossing measurement and bend overlap reduction
# ==============================================================================

(defn- _arc-sweep-span
  ```Reference angle and sweep span of an arc segment.
The sweep runs counter-clockwise from the reference angle by `span` radians.```
  [seg s]
  (let [cx (seg :cx) cy (seg :cy)
        ths (math/atan2 (- (s 1) cy) (- (s 0) cx))
        the (math/atan2 (- (seg :y) cy) (- (seg :x) cx))]
    (if (seg :cw)
      @[the (mod (- ths the) two-pi)]
      @[ths (mod (- the ths) two-pi)])))

(defn- _arc-covers?
  "True when the circle point p lies on the swept arc segment seg (start s)."
  [seg s p]
  (let [fs (_arc-sweep-span seg s)
        ph (math/atan2 (- (p 1) (seg :cy)) (- (p 0) (seg :cx)))]
    (<= (mod (- ph (fs 0)) two-pi) (+ (fs 1) 1e-9))))

(defn- _circle-x-pts
  "0, 1 or 2 intersection points of the circles (c1 r1) and (c2 r2)."
  [c1 r1 c2 r2]
  (let [dx (- (c2 0) (c1 0)) dy (- (c2 1) (c1 1))
        d (math/hypot dx dy)
        pts @[]]
    (when (> d 1e-12)
      (let [a (/ (- (+ (* r1 r1) (* d d)) (* r2 r2)) (* 2 d))
            h2 (- (* r1 r1) (* a a))]
        (when (>= h2 0)
          (let [h (math/sqrt h2)
                ux (/ dx d) uy (/ dy d)
                px (+ (c1 0) (* a ux)) py (+ (c1 1) (* a uy))]
            (if (< h 1e-9)
              (array/push pts @[px py])
              (do
                (array/push pts @[(+ px (- (* uy h))) (+ py (* ux h))])
                (array/push pts @[(- px (- (* uy h))) (- py (* ux h))])))))))
    pts))

(defn- _push-seg-t
  ```Append point at t along a + t*(dx, dy) to pts, when t is on the segment.```
  [pts a dx dy t]
  (when (and (>= t -1e-9) (<= t 1.000000001))
    (array/push pts @[(+ (a 0) (* t dx)) (+ (a 1) (* t dy))])))

(defn- _line-x-pts
  "Intersection points of the line segment a-b with the circle (c r)."
  [a b c r]
  (let [dx (- (b 0) (a 0)) dy (- (b 1) (a 1))
        fx (- (a 0) (c 0)) fy (- (a 1) (c 1))
        A (+ (* dx dx) (* dy dy))]
    (def pts @[])
    (when (> A 1e-12)
      (let [B (* 2 (+ (* fx dx) (* fy dy)))
            C (- (+ (* fx fx) (* fy fy)) (* r r))
            disc (- (* B B) (* 4 A C))]
        (when (>= disc 0)
          (def sd (math/sqrt disc))
          (_push-seg-t pts a dx dy (/ (+ (- B) sd) (* 2 A)))
          (when (> sd 1e-9)
            (_push-seg-t pts a dx dy (/ (- (- B) sd) (* 2 A)))))))
    pts))

(defn- _record-segments
  "Each is {:line bool :a start :b end :seg x}, chained from start at (x0 y0)."
  [rec]
  (def segs @[])
  (var cur @[(rec :x0) (rec :y0)])
  (each seg (rec :segments)
    (def nxt @[(seg :x) (seg :y)])
    (array/push segs @{:line (get seg :line) :a cur :b nxt :seg seg})
    (set cur nxt))
  segs)

(defn- _record-overlaps
  "Reducer overlap count"
  [a b]
  (let [sa (_record-segments a)
        sb (_record-segments b)]
    (var cnt 0)
    (each x sa
      (each y sb
        (cond
          (and (x :line) (y :line))
          (when (_cm__segments-cross? (x :a) (x :b) (y :a) (y :b))
            (set cnt (+ cnt 1)))
          (x :line)
          (each p (_line-x-pts (x :a) (x :b) @[((y :seg) :cx) ((y :seg) :cy)]
                               ((y :seg) :r))
            (when (_arc-covers? (y :seg) (y :a) p)
              (set cnt (+ cnt 1))))
          (y :line)
          (each p (_line-x-pts (y :a) (y :b) @[((x :seg) :cx) ((x :seg) :cy)]
                               ((x :seg) :r))
            (when (_arc-covers? (x :seg) (x :a) p)
              (set cnt (+ cnt 1))))
          true
          (each p (_circle-x-pts @[((x :seg) :cx) ((x :seg) :cy)] ((x :seg) :r)
                                 @[((y :seg) :cx) ((y :seg) :cy)] ((y :seg) :r))
            (when (and (_arc-covers? (x :seg) (x :a) p)
                       (_arc-covers? (y :seg) (y :a) p))
              (set cnt (+ cnt 1)))))))
    cnt))

(defn- _line-line-x
  "Intersection point of segments a-b and c-d, or nil if parallel."
  [a b c d]
  (let [dx1 (- (b 0) (a 0)) dy1 (- (b 1) (a 1))
        dx2 (- (d 0) (c 0)) dy2 (- (d 1) (c 1))
        det (- (* dx1 dy2) (* dy1 dx2))]
    (when (not= det 0)
      (def t (/ (- (* (- (c 0) (a 0)) dy2) (* (- (c 1) (a 1)) dx2)) det))
      @[(+ (a 0) (* t dx1)) (+ (a 1) (* t dy1))])))

(defn- _prim-tangent
  "Tangent-line angle (mod pi) of a primitive segment at a point p."
  [prim p]
  (if (prim :line)
    (math/atan2 (- ((prim :b) 1) ((prim :a) 1)) (- ((prim :b) 0) ((prim :a) 0)))
    (let [c @[((prim :seg) :cx) ((prim :seg) :cy)]]
      (+ (math/atan2 (- (p 1) (c 1)) (- (p 0) (c 0))) (/ math/pi 2)))))

(defn- _cross-angle-at
  "Folded angle between the tangent lines of primitive segments x and y at p."
  [x y p]
  (math/abs (_line-diff (_prim-tangent x p) (_prim-tangent y p))))

(defn- _proper-intersections
  "Distinct proper intersection points of two edges"
  [a b]
  (def au (a :u)) (def av (a :v))
  (def bu (b :u)) (def bv (b :v))
  (if (or (= au bu) (= au bv) (= av bu) (= av bv))
    @[]
    (let [sa (_record-segments a)
          sb (_record-segments b)
          pts @[]]
      (defn add [x y p]
        (when (and p
                   (> (_cross-angle-at x y p) 1e-6)
                   (not (some (fn [q]
                                (<= (math/hypot (- (p 0) (q 0))
                                                (- (p 1) (q 1)))
                                    1e-7))
                              pts)))
          (array/push pts p)))
      (each x sa
        (each y sb
          (cond
            (and (x :line) (y :line))
            (let [p (_line-line-x (x :a) (x :b) (y :a) (y :b))]
              (when (and p (_cm__segments-cross? (x :a) (x :b) (y :a) (y :b)))
                (add x y p)))
            (x :line)
            (each p (_line-x-pts (x :a) (x :b) @[((y :seg) :cx) ((y :seg) :cy)]
                                 ((y :seg) :r))
              (when (_arc-covers? (y :seg) (y :a) p)
                (add x y p)))
            (y :line)
            (each p (_line-x-pts (y :a) (y :b) @[((x :seg) :cx) ((x :seg) :cy)]
                                 ((x :seg) :r))
              (when (_arc-covers? (x :seg) (x :a) p)
                (add x y p)))
            true
            (each p (_circle-x-pts @[((x :seg) :cx) ((x :seg) :cy)] ((x :seg) :r)
                                   @[((y :seg) :cx) ((y :seg) :cy)] ((y :seg) :r))
              (when (and (_arc-covers? (x :seg) (x :a) p)
                         (_arc-covers? (y :seg) (y :a) p))
                (add x y p))))))
      pts)))

(defn- _drawing-nodes
  "Endpoints of every record in `recs`, one entry per incidence (not unique)."
  [recs]
  (def nodes @[])
  (each rec recs
    (array/push nodes @[(rec :x0) (rec :y0)])
    (def segs (rec :segments))
    (def last (segs (- (length segs) 1)))
    (array/push nodes @[(last :x) (last :y)]))
  nodes)

(defn- _point-at-node?
  "True when p is within `radius` of a vertex. radius 0 for exact match."
  [nodes p radius]
  (def tol (if (> radius 0) radius 1e-7))
  (some (fn [q]
          (<= (math/hypot (- (p 0) (q 0)) (- (p 1) (q 1))) tol))
        nodes))

(defn- _graph-crossings
  "Graph-theoretic crossing count; Every transversal intersection of two non-adjacent edges."
  [recs]
  (def nodes (_drawing-nodes recs))
  (var total 0)
  (loop [i :range [0 (length recs)]]
    (loop [j :range [(+ i 1) (length recs)]]
      (each p (_proper-intersections (recs i) (recs j))
        (unless (_point-at-node? nodes p 0) (++ total)))))
  total)

(defn- _visible-crossings
  "Visibly distinct edge crossings."
  [recs radius]
  (def nodes (_drawing-nodes recs))
  (def seen @[])
  (loop [i :range [0 (length recs)]]
    (loop [j :range [(+ i 1) (length recs)]]
      (each p (_proper-intersections (recs i) (recs j))
        (when (and (not (_point-at-node? nodes p radius))
                   (not (some (fn [q]
                                (<= (math/hypot (- (p 0) (q 0))
                                                (- (p 1) (q 1)))
                                    1e-7))
                              seen)))
          (array/push seen p)))))
  (length seen))

(defn- _record-crossing-angle
  "Smallest folded angle (radians in [0, pi/2]) at which two edges cross."
  [a b]
  (let [sa (_record-segments a)
        sb (_record-segments b)]
    (var best math/inf)
    (each x sa
      (each y sb
        (cond
          (and (x :line) (y :line))
          (let [p (_line-line-x (x :a) (x :b) (y :a) (y :b))]
            (when (and p (_cm__segments-cross? (x :a) (x :b) (y :a) (y :b)))
              (set best (min best (_cross-angle-at x y p)))))
          (x :line)
          (each p (_line-x-pts (x :a) (x :b) @[((y :seg) :cx) ((y :seg) :cy)]
                               ((y :seg) :r))
            (when (_arc-covers? (y :seg) (y :a) p)
              (set best (min best (_cross-angle-at x y p)))))
          (y :line)
          (each p (_line-x-pts (y :a) (y :b) @[((x :seg) :cx) ((x :seg) :cy)]
                               ((x :seg) :r))
            (when (_arc-covers? (x :seg) (x :a) p)
              (set best (min best (_cross-angle-at x y p)))))
          true
          (each p (_circle-x-pts @[((x :seg) :cx) ((x :seg) :cy)] ((x :seg) :r)
                                 @[((y :seg) :cx) ((y :seg) :cy)] ((y :seg) :r))
            (when (and (_arc-covers? (x :seg) (x :a) p)
                       (_arc-covers? (y :seg) (y :a) p))
              (set best (min best (_cross-angle-at x y p))))))))
    best))

(defn- _record-degenerate?
  "True when a poly record contains a straight (:line) segment."
  [rec]
  (when (= (rec :kind) :poly)
    (not (empty? (seq [s :in (rec :segments) :when (get s :line)] true)))))

(defn- _bboxes-touch?
  "True when bounding boxes `a` and `b` touch or overlap."
  [a b]
  (and (<= (a 0) (b 2)) (>= (a 2) (b 0))
       (<= (a 1) (b 3)) (>= (a 3) (b 1))))

(defn- _box-grow
  "Grow the [x0 y0 x1 y1] box to contain the point (x y)."
  [b x y]
  (when (< x (b 0)) (put b 0 x))
  (when (> x (b 2)) (put b 2 x))
  (when (< y (b 1)) (put b 1 y))
  (when (> y (b 3)) (put b 3 y)))

(defn _record-box
  ```Bounding box [x0 y0 x1 y1] of segments in `rec`.```
  [rec]
  (def box @[(rec :x0) (rec :y0) (rec :x0) (rec :y0)])
  (var cur @[(rec :x0) (rec :y0)])
  (each seg (rec :segments)
    (_box-grow box (seg :x) (seg :y))
    (unless (seg :line)
      (def fs (_arc-sweep-span seg cur))
      (each ph @[0.0 (/ math/pi 2.0) math/pi (* 1.5 math/pi)]
            (when (<= (mod (- ph (fs 0)) two-pi) (fs 1))
              (_box-grow box (+ (seg :cx) (* (seg :r) (math/cos ph)))
                         (+ (seg :cy) (* (seg :r) (math/sin ph)))))))
    (set cur @[(seg :x) (seg :y)]))
  box)

(defn- _record-samples
  ```Flat points along a record's chain.```
  [rec n]
  (def pts @[@[(rec :x0) (rec :y0)]])
  (var cur @[(rec :x0) (rec :y0)])
  (each seg (rec :segments)
    (def end @[(seg :x) (seg :y)])
    (if (seg :line)
      (array/push pts end)
      (let [c @[(seg :cx) (seg :cy)]
            r (seg :r)
            fs (_arc-sweep-span seg cur)
            from (fs 0)
            span (fs 1)]
        (each i (range 1 (inc n))
          (def ph (+ from (* span (/ i (inc n)))))
          (array/push pts @[(+ (c 0) (* r (math/cos ph)))
                            (+ (c 1) (* r (math/sin ph)))]))))
    (set cur end))
  pts)

(defn- _spring-energy
  ```Smooth repulsion between two records' sampled points.```
  [sa sb cutoff]
  (var e 0.0)
  (def c2 (* cutoff cutoff))
  (each a sa
    (let [ax (a 0) ay (a 1)]
      (each p sb
        (let [dx (- ax (p 0)) dy (- ay (p 1))
              d2 (+ (* dx dx) (* dy dy))]
          (when (< d2 c2)
            (def d (math/sqrt d2))
            (set e (+ e (* (- cutoff d) (- cutoff d)))))))))
  e)

(defn- _sample-gap2
  "Smallest squared distance between two sampled chains."
  [sa sb]
  (var best math/inf)
  (each a sa
    (let [ax (a 0) ay (a 1)]
      (each b sb
        (def dx (- ax (b 0)))
        (def dy (- ay (b 1)))
        (def d2 (+ (* dx dx) (* dy dy)))
        (when (< d2 best) (set best d2)))))
  best)

(defn- _record-clearance
  ```Closeness of two non-crossing edges.```
  [sa sb clearance]
  (def c2 (* clearance clearance))
  (def best (_sample-gap2 sa sb))
  (if (< best c2)
    @[1 (- clearance (math/sqrt best))]
    @[0 0.0]))

(defn- _records-adjacent?
  "True when two edges share an endpoint."
  [a b]
  (or (= (a :u) (b :u)) (= (a :u) (b :v))
      (= (a :v) (b :u)) (= (a :v) (b :v))))

(defn- _min-arc-clearance
  "Smallest distance between sampled points of two non-crossing, non-adjacent edges."
  [recs]
  (var mn math/inf)
  (let [n (length recs)]
    (for i 0 n
      (for j (+ i 1) n
        (def a (recs i))
        (def b (recs j))
        (when (and (not (_records-adjacent? a b))
                   (= 0 (_record-overlaps a b)))
          (def gap (math/sqrt (_sample-gap2 (_record-samples a 12)
                                            (_record-samples b 12))))
          (set mn (min mn gap))))))
  (if (= mn math/inf) nil mn))

(defn- _boxes-near?
  "True when two boxes are closer than `margin` on both axes."
  [a b margin]
  (and (<= (- (a 0)) (+ (b 2) margin))
       (>= (+ (a 2) margin) (b 0))
       (<= (- (a 1)) (+ (b 3) margin))
       (>= (+ (a 3) margin) (b 1))))

(defn- _samples-node-violations
  ```Count and total depth of the vertex discs a sampled chain intrudes on.```
  [samples u v vpos clearance]
  (def c2 (* clearance clearance))
  (var cnt 0)
  (var depth 0.0)
  (eachp [w p] vpos
         (unless (or (= w u) (= w v))
           (var best c2)
           (each s samples
             (def dx (- (s 0) (p 0)))
             (def dy (- (s 1) (p 1)))
             (def d2 (+ (* dx dx) (* dy dy)))
             (when (< d2 best) (set best d2)))
           (when (< best c2)
             (set cnt (+ cnt 1))
             (set depth (+ depth (- clearance (math/sqrt best)))))))
  @[cnt depth])

(defn- _row-clearance
  "Non-crossing arc and node clearance violations contributed by edge i.
  Returns [violations depth]."
  [st i]
  (let [n (st :n) clear (st :arc-clearance) nclear (st :node-clearance)]
    (if (and (nil? clear) (nil? nclear))
      @[0 0.0]
      (let [recs (st :recs) boxes (st :boxes) samples (st :samples)
            matrix (st :matrix) rec (recs i) box (boxes i)]
        (var cnt 0)
        (var depth 0.0)
        (when clear
          (each j (range n)
            (when (and (not= j i)
                       (= (get (matrix i) j) 0)
                       (not (_records-adjacent? rec (recs j)))
                       (_boxes-near? box (boxes j) clear))
              (let [[c d] (_record-clearance (samples i) (samples j) clear)]
                (set cnt (+ cnt c))
                (set depth (+ depth d))))))
        (when nclear
          (let [[c d] (_samples-node-violations (samples i) (rec :u) (rec :v)
                                                (st :pacc) nclear)]
            (set cnt (+ cnt c))
            (set depth (+ depth d))))
        @[cnt depth]))))

(defn- _drawing-clearance
  "Non-crossing arc and node clearance over the entire drawing.
Returns [violations depth min-gap], with min-gap nil if no eligible pair exists."
  [st]
  (let [n (st :n) clear (st :arc-clearance) nclear (st :node-clearance)]
    (if (and (nil? clear) (nil? nclear))
      @[0 0.0 nil]
      (let [recs (st :recs) boxes (st :boxes) samples (st :samples)
            matrix (st :matrix)]
        (var cnt 0)
        (var depth 0.0)
        (var mn math/inf)
        (when clear
          (for i 0 n
            (for j (+ i 1) n
              (when (and (= (get (matrix i) j) 0)
                         (not (_records-adjacent? (recs i) (recs j)))
                         (_boxes-near? (boxes i) (boxes j) clear))
                (def gap (math/sqrt (_sample-gap2 (samples i) (samples j))))
                (set mn (min mn gap))
                (when (< gap clear)
                  (set cnt (inc cnt))
                  (set depth (+ depth (- clear gap))))))))
        (when nclear
          (for i 0 n
            (let [rec (recs i)
                  [c d] (_samples-node-violations (samples i) (rec :u) (rec :v)
                                                  (st :pacc) nclear)]
              (set cnt (+ cnt c))
              (set depth (+ depth d)))))
        @[cnt depth (if (= mn math/inf) nil mn)]))))

(defn- _incident-clearance
  "Non-crossing arc and node clearance violations over pairs with edge incident to v.
Returns [violations depth]."
  [st v]
  (let [n (st :n) clear (st :arc-clearance) nclear (st :node-clearance)]
    (if (and (nil? clear) (nil? nclear))
      @[0 0.0]
      (let [recs (st :recs) boxes (st :boxes) samples (st :samples)
            matrix (st :matrix)
            inset (let [m @{}] (each i ((st :incident) v) (put m i true)) m)]
        (var cnt 0)
        (var depth 0.0)
        (when clear
          (for i 0 n
            (for j (+ i 1) n
              (when (and (or (get inset i) (get inset j))
                         (= (get (matrix i) j) 0)
                         (not (_records-adjacent? (recs i) (recs j)))
                         (_boxes-near? (boxes i) (boxes j) clear))
                (let [[c d] (_record-clearance (samples i) (samples j) clear)]
                  (set cnt (+ cnt c))
                  (set depth (+ depth d)))))))
        (when nclear
          (each i ((st :incident) v)
            (let [rec (recs i)
                  [c d] (_samples-node-violations (samples i) (rec :u) (rec :v)
                                                  (st :pacc) nclear)]
              (set cnt (+ cnt c))
              (set depth (+ depth d)))))
        @[cnt depth]))))

(defn _min-node-clearance
  "Smallest distance from a sampled arc point to a vertex it does not touch."
  [recs vpos]
  (var mn math/inf)
  (each rec recs
    (def u (rec :u))
    (def v (rec :v))
    (each s (_record-samples rec 12)
      (eachp [w p] vpos
             (unless (or (= w u) (= w v))
               (def dx (- (s 0) (p 0)))
               (def dy (- (s 1) (p 1)))
               (set mn (min mn (math/sqrt (+ (* dx dx) (* dy dy)))))))))
  (if (= mn math/inf) nil mn))


# ==============================================================================
#  Reducer state
# ==============================================================================

(defn- _reducer-state
  "Build the reducer's state table and fill initial crossing matrix."
  [pos edges slots deg recs slot-rot bend spring-samples opts]
  (let [n (length recs)
        rot-acc (let [m @{}]
                  (each e edges
                    (unless (get m (e 0))
                      (put m (e 0) (if (number? slot-rot)
                                     slot-rot (get slot-rot (e 0) 0.0))))
                    (unless (get m (e 1))
                      (put m (e 1) (if (number? slot-rot)
                                     slot-rot (get slot-rot (e 1) 0.0)))))
                  m)
        incident (let [m @{}]
                   (loop [i :range [0 (length edges)]]
                     (let [e (edges i)]
                       (unless (get m (e 0)) (put m (e 0) @[]))
                       (unless (get m (e 1)) (put m (e 1) @[]))
                       (array/push (m (e 0)) i)
                       (array/push (m (e 1)) i)))
                   m)
        st (merge opts
                  {:n n :edges edges :slots slots :deg deg :pos pos
                   :rot-acc rot-acc :incident incident :recs recs
                   :pacc (let [m @{}] (eachp [k v] pos (put m k (array ;v))) m)
                   :boxes (array/new-filled n @[])
                   :matrix (array/new-filled n @[])
                   :bends (array/new-filled n bend)
                   :samples (array/new-filled n @[])})]
    (each i (range n)
      (put (st :boxes) i (_record-box (recs i)))
      (put (st :matrix) i (array/new-filled n 0))
      (put (st :samples) i (_record-samples (recs i) spring-samples)))
    (each i (range n)
      (for j (+ i 1) n
        (let [c (if (_bboxes-touch? ((st :boxes) i) ((st :boxes) j))
                  (_record-overlaps (recs i) (recs j))
                  0)]
          (put ((st :matrix) i) j c)
          (put ((st :matrix) j) i c))))
    st))

#  metrics

(defn- _matrix-total
  "Overlaps of every edge pair, i.e. total overlap count."
  [matrix n]
  (var cnt 0)
  (for i 0 n
    (for j (+ i 1) n
      (set cnt (+ cnt (get (matrix i) j)))))
  cnt)

(defn- _rowsum
  "Crossings counted in one row of the matrix excluding the diagonal."
  [matrix n i]
  (var cnt 0)
  (each j (range n)
    (when (not= j i)
      (set cnt (+ cnt (get (matrix i) j)))))
  cnt)

(defn- _total-curvature
  "Total arc sweep over a set of edges."
  [recs]
  (var c 0.0)
  (each rec recs (set c (+ c (_record-curvature rec))))
  c)

(defn- _poly-count
  "Number of edges drawn as a two-arc poly path."
  [recs]
  (var count 0)
  (each rec recs
    (when (= (rec :kind) :poly) (set count (inc count))))
  count)

(defn- _all-in-cap?
  "True when no edge sweeps a segment wider than `max-sweep`."
  [recs max-sweep]
  (if max-sweep
    (not (some (fn [rec] (> (_record-max-sweep rec) max-sweep)) recs))
    true))

(defn- _spring-sum
  "Repulsion energy of one edge against every other edge its box touches."
  [st i fbox fsamp]
  (let [n (st :n) boxes (st :boxes) samples (st :samples)]
    (var e 0.0)
    (each j (range n)
      (when (and (not= j i) (_bboxes-touch? fbox (boxes j)))
        (set e (+ e (_spring-energy fsamp (samples j) (st :spring-cutoff))))))
    e))

(defn- _drawing-energy
  "Repulsion energy over every pair of edges whose boxes touch."
  [st]
  (let [n (st :n)]
    (var e 0.0)
    (when (st :spring)
      (for i 0 n
        (for j (+ i 1) n
          (when (_bboxes-touch? ((st :boxes) i) ((st :boxes) j))
            (set e (+ e (_spring-energy ((st :samples) i) ((st :samples) j)
                                        (st :spring-cutoff))))))))
    e))

(defn- _incident-total
  "Crossings over every pair with at least one edge incident to v."
  [st v]
  (let [n (st :n) matrix (st :matrix)
        seen (array/new-filled n false)]
    (var cnt 0)
    (each i ((st :incident) v)
      (for j 0 n
        (when (and (not= i j) (not (get seen j)))
          (put seen j true)
          (set cnt (+ cnt (get (matrix i) j))))))
    cnt))

(defn- _incident-curvature
  "Total arc sweep over the edges incident to v."
  [st v]
  (var c 0.0)
  (each i ((st :incident) v)
    (set c (+ c (_record-curvature ((st :recs) i)))))
  c)

#  rebuilding

(defn- _rebuild-record
  "Rebuild edge i from the current rotations, bends, and positions."
  [st i]
  (let [e ((st :edges) i)
        pacc (st :pacc)
        pu (get pacc (e 0))
        pv (get pacc (e 1))]
    (when (and pu pv)
      (put (st :recs) i
           (_edge-record (e 0) (e 1) pu pv
                         (_slot-angle (st :slots) (st :deg) (e 0) i (st :rot-acc))
                         (_slot-angle (st :slots) (st :deg) (e 1) i (st :rot-acc))
                         (st :tol) (st :meet) ((st :bends) i) (st :join)
                         (st :max-sweep)))
      (put (st :boxes) i (_record-box ((st :recs) i)))
      (put (st :samples) i (_record-samples ((st :recs) i) (st :spring-samples))))))

(defn- _rebuild-all
  "Rebuild every record."
  [st] (each i (range (st :n)) (_rebuild-record st i)))

(defn- _rebuild-incident
  "Rebuild every record incident to v."
  [st v] (each i ((st :incident) v) (_rebuild-record st i)))

(defn- _refresh-row
  "Recount edge i crossings against every other edge, copy into other rows."
  [st i]
  (let [n (st :n) recs (st :recs) boxes (st :boxes) matrix (st :matrix)]
    (for j 0 n
      (when (not= i j)
        (let [newc (if (_bboxes-touch? (boxes i) (boxes j))
                     (_record-overlaps (recs i) (recs j))
                     0)]
          (put (matrix i) j newc)
          (put (matrix j) i newc))))))

(defn- _rebuild-rows
  "Recount the matrix rows of every edge incident to v."
  [st v] (each i ((st :incident) v) (_refresh-row st i)))

(defn- _any-degenerate?
  "True when an edge incident to v has degenerated to a straight joint."
  [st v]
  (var d false)
  (each i ((st :incident) v)
    (when (_record-degenerate? ((st :recs) i)) (set d true)))
  d)

#  joint-bend pass

(defn- _lex-better?
  "Is candidate (cap fn cg cgd ang ms e curv) better than the current best?"
  [cap fn cg cgd ang ms e curv cap2 fn2 cg2 cgd2 ang2 ms2 e2 curv2]
  (cond
    (and cap (not cap2)) true
    (and (not cap) cap2) false
    (and cap cap2)
    (or (< fn fn2)
        (and (= fn fn2) (< cg cg2))
        (and (= fn fn2) (= cg cg2) (< cgd cgd2))
        (and (= fn fn2) (= cg cg2) (= cgd cgd2) (< curv curv2))
        (and (= fn fn2) (= cg cg2) (= cgd cgd2) (= curv curv2) (> ang ang2))
        (and (= fn fn2) (= cg cg2) (= cgd cgd2) (= curv curv2) (= ang ang2) (< e e2)))
    :else
     (or (< ms ms2)
         (and (= ms ms2) (< fn fn2))
         (and (= ms ms2) (= fn fn2) (< cg cg2))
         (and (= ms ms2) (= fn fn2) (= cg cg2) (< cgd cgd2))
         (and (= ms ms2) (= fn fn2) (= cg cg2) (= cgd cgd2) (< curv curv2))
         (and (= ms ms2) (= fn fn2) (= cg cg2) (= cgd cgd2) (= curv curv2) (> ang ang2))
         (and (= ms ms2) (= fn fn2) (= cg cg2) (= cgd cgd2) (= curv curv2) (= ang ang2) (< e e2)))))

(defn- _trial
  "Try candidate bends for poly edge i and commit the best found.
Returns [committed? overlaps]."
  [st i u v pu pv cands]
  (let [n (st :n)
        recs (st :recs)
        boxes (st :boxes)
        spring (st :spring)
        clearance (st :arc-clearance)
        node-clearance (st :node-clearance)
        max-sweep (st :max-sweep)
        cur (_rowsum (st :matrix) n i)
        [cur-cg cur-cgd] (_row-clearance st i)
        cur-e (if spring (_spring-sum st i (boxes i) ((st :samples) i)) 0.0)
        cur-ms (if max-sweep (_record-max-sweep (recs i)) 0.0)
        cur-curv (_record-curvature (recs i))
        cur-ang (if (st :crossing-angle)
                  (do (var mn math/inf)
                      (each j (range n)
                        (when (and (not= j i) (_bboxes-touch? (boxes i) (boxes j)))
                          (let [aa (_record-crossing-angle (recs i) (recs j))]
                            (when (< aa mn) (set mn aa)))))
                      mn)
                  0.0)]
    (var picked false)
    (var best-fn cur)
    (var best-cg cur-cg)
    (var best-cgd cur-cgd)
    (var best-e cur-e)
    (var best-ms cur-ms)
    (var best-curv cur-curv)
    (var best-ang cur-ang)
    (var best-cap (if max-sweep (> cur-ms max-sweep) true))
    (var best-rec (recs i))
    (var best-box (boxes i))
    (var best-cand nil)
    (var best-samp ((st :samples) i))
    (each cand cands
      (let [flipped (_edge-record u v pu pv
                                  (_slot-angle (st :slots) (st :deg) u i (st :rot-acc))
                                  (_slot-angle (st :slots) (st :deg) v i (st :rot-acc))
                                  (st :tol) (st :meet) cand (st :join) max-sweep)]
        (when (not (_record-degenerate? flipped))
          (let [fbox (_record-box flipped)
                ms (if max-sweep (_record-max-sweep flipped) 0.0)
                cap (if max-sweep (<= ms max-sweep) true)
                doomed (or (and (not cap) best-cap)
                           (and (not cap) (not best-cap) (> ms best-ms)))]
            (unless doomed
              (var fn 0)
              (var cg 0)
              (var cgd 0.0)
              (var fsamp nil)
              (when (or spring clearance node-clearance)
                (set fsamp (_record-samples flipped (st :spring-samples))))
              (loop [j :range [0 n]]
                (when (not= j i)
                  (def touch (_bboxes-touch? fbox (boxes j)))
                  (def near (and clearance
                                 (_boxes-near? fbox (boxes j) clearance)))
                  (when touch
                    (def ov (_record-overlaps flipped (recs j)))
                    (set fn (+ fn ov))
                    (when (and clearance (= ov 0) near
                               (not (_records-adjacent? flipped (recs j))))
                      (let [[c d] (_record-clearance
                                   fsamp ((st :samples) j) clearance)]
                        (set cg (+ cg c))
                        (set cgd (+ cgd d)))))
                  (when (and clearance near (not touch)
                             (not (_records-adjacent? flipped (recs j))))
                    (let [[c d] (_record-clearance
                                 fsamp ((st :samples) j) clearance)]
                      (set cg (+ cg c))
                      (set cgd (+ cgd d))))
                  (when (and cap best-cap (> fn best-fn)) (break))))
              (when (and node-clearance fsamp)
                (let [[c d] (_samples-node-violations
                             fsamp u v (st :pacc) node-clearance)]
                  (set cg (+ cg c))
                  (set cgd (+ cgd d))))
              (var ang 0.0)
              (when (st :crossing-angle)
                (set ang math/inf)
                (each j (range n)
                  (when (and (not= j i) (_bboxes-touch? fbox (boxes j)))
                    (let [aa (_record-crossing-angle flipped (recs j))]
                      (when (< aa ang) (set ang aa))))))
              (var e 0.0)
              (when spring
                (set e (_spring-sum st i fbox fsamp)))
              (let [curv (_record-curvature flipped)]
                (when (_lex-better? cap fn cg cgd ang ms e curv
                                    best-cap best-fn best-cg best-cgd best-ang best-ms best-e best-curv)
                  (set picked true)
                  (set best-cand cand)
                  (set best-fn fn)
                  (set best-cg cg)
                  (set best-cgd cgd)
                  (set best-ang ang)
                  (set best-e e)
                  (set best-ms ms)
                  (set best-cap cap)
                  (set best-rec flipped)
                  (set best-box fbox)
                  (when (or spring clearance node-clearance)
                    (set best-samp fsamp)))))))))
    (if picked
      (do
        (put (st :recs) i best-rec)
        (put (st :boxes) i best-box)
        (put (st :bends) i best-cand)
        (when (or spring clearance node-clearance)
          (put (st :samples) i best-samp))
        (_refresh-row st i)
        @[true best-fn best-cg best-cgd])
      @[false cur cur-cg cur-cgd])))

(defn- _bend-pass
  "One greedy pass over the poly edges."
  [st]
  (var changed false)
  (each i (range (st :n))
    (let [rec ((st :recs) i)]
      (when (= (rec :kind) :poly)
        (let [[u v] ((st :edges) i)
              pu (get (st :pacc) u) pv (get (st :pacc) v)
              sweep (st :sweep)]
          (when (and pu pv)
            (let [coarse (seq [k :in (st :offsets)]
                              (+ ((st :bends) i) (* math/pi (/ k sweep))))
                  [c1 _] (_trial st i u v pu pv coarse)]
              (when c1 (set changed true)))
            (loop [level :range [1 (inc (st :relax-levels))]]
              (let [step (/ math/pi (* sweep (math/pow 2 level)))
                    fine (seq [k :range [(- (dec sweep)) (inc (dec sweep))]]
                              (+ ((st :bends) i) (* k step)))
                    [c2 _] (_trial st i u v pu pv fine)]
                (when c2 (set changed true)))))))))
  changed)

#  Tangent-frame sweep
#
# - rotate each vertex's slot frame across the :sweep angular grid
# - keep the rotation that lowers the drawing total
# - refine the frame angles below :sweep resolution
# - check bends on the rotated frames
# - rotations and bends are reverted if the sweep didn't improve the drawing

(defn- _frame-sweep
  "Perform a frame sweep."
  [st]
  (let [n (st :n)
        recs (st :recs)
        matrix (st :matrix)
        bends (st :bends)
        rot-acc (st :rot-acc)
        incident (st :incident)
        sweep (st :sweep)
        before (_matrix-total matrix n)
        [before-cg before-cgd] (_drawing-clearance st)
        before-curvature (_total-curvature recs)
        saved-frames (let [m @{}] (eachp [k v] rot-acc (put m k v)) m)
        saved-bends (array ;bends)
        saved-matrix (let [m (array/new-filled n @[])]
                       (each i (range n) (put m i (array ;(matrix i))))
                       m)]
    (var frame-changed true)
    (var frame-round 0)
    (while (and frame-changed (< frame-round 2))
      (set frame-changed false)
      (set frame-round (inc frame-round))
      (each v (sort (keys incident))
        (let [cur-r (rot-acc v)]
          (var best-r cur-r)
          (var best-total (_incident-total st v))
          (var [best-cg best-cgd] (_incident-clearance st v))
          (var best-curvature (_incident-curvature st v))
          (each cand (seq [k :in (st :offsets)] (+ cur-r (* two-pi (/ k sweep))))
            (when (not= cand cur-r)
              (put rot-acc v cand)
              (_rebuild-incident st v)
              (_rebuild-rows st v)
              (let [t (_incident-total st v)
                    [cg cgd] (_incident-clearance st v)
                    c (_incident-curvature st v)]
                (when (or (< t best-total)
                          (and (= t best-total) (< cg best-cg))
                          (and (= t best-total) (= cg best-cg) (< cgd best-cgd))
                          (and (= t best-total) (= cg best-cg) (= cgd best-cgd)
                               (< c best-curvature)))
                  (set best-total t)
                  (set best-cg cg)
                  (set best-cgd cgd)
                  (set best-curvature c)
                  (set best-r cand)))))
          (unless (= best-r cur-r)
            (put rot-acc v best-r)
            (_rebuild-incident st v)
            (_rebuild-rows st v)
            (set frame-changed true))))
      (loop [level :range [1 (inc (st :relax-levels))]]
        (let [step (/ two-pi (* sweep (math/pow 2 level)))]
          (each v (sort (keys incident))
            (let [cur-r (rot-acc v)]
              (var best-r cur-r)
              (var best-total (_incident-total st v))
              (var [best-cg best-cgd] (_incident-clearance st v))
              (var best-curvature (_incident-curvature st v))
              (each k (range (- (dec sweep)) (inc (dec sweep)))
                (when (not= k 0)
                  (let [cand (+ cur-r (* k step))]
                    (put rot-acc v cand)
                    (_rebuild-incident st v)
                    (_rebuild-rows st v)
                    (let [t (_incident-total st v)
                          [cg cgd] (_incident-clearance st v)
                          c (_incident-curvature st v)]
                      (when (or (< t best-total)
                                (and (= t best-total)
                                     (< cg best-cg))
                                (and (= t best-total)
                                     (= cg best-cg)
                                     (< cgd best-cgd))
                                (and (= t best-total)
                                     (= cg best-cg)
                                     (= cgd best-cgd)
                                     (< c best-curvature)))
                        (set best-total t)
                        (set best-cg cg)
                        (set best-cgd cgd)
                        (set best-curvature c)
                        (set best-r cand))))))
              (put rot-acc v best-r)
              (_rebuild-incident st v)
              (_rebuild-rows st v)))))
      (loop [p :range [0 (st :passes)]]
        (unless (_bend-pass st) (break))))
    (let [after-total (_matrix-total matrix n)
          [after-cg after-cgd] (_drawing-clearance st)
          after-curvature (_total-curvature recs)]
      (when (or (> after-total before)
                (and (= after-total before)
                     (> after-cg before-cg))
                (and (= after-total before)
                     (= after-cg before-cg)
                     (> after-cgd before-cgd))
                (and (= after-total before)
                     (= after-cg before-cg)
                     (= after-cgd before-cgd)
                     (>= after-curvature before-curvature)))
        (each v (sort (keys incident))
          (put rot-acc v (get saved-frames v 0.0)))
        (each i (range n)
          (put bends i (saved-bends i))
          (put matrix i (array ;(saved-matrix i))))
        (_rebuild-all st)))))

#  vertex relaxation

(defn- _bounds-of
  "Axis-aligned bounds of a position table, as @[[minx miny] [maxx maxy]]."
  [pos]
  (let [mmin @[math/inf math/inf]
        mmax @[(* -1 math/inf) (* -1 math/inf)]]
    (eachp [k p] pos
           (set (mmin 0) (min (mmin 0) (p 0)))
           (set (mmin 1) (min (mmin 1) (p 1)))
           (set (mmax 0) (max (mmax 0) (p 0)))
           (set (mmax 1) (max (mmax 1) (p 1))))
    @[mmin mmax]))

(defn- _clamp-pt
  "Clamp  point `p` to the drawing's bounds."
  [bounds p]
  @[(if (< (p 0) ((bounds 0) 0)) ((bounds 0) 0)
        (if (> (p 0) ((bounds 1) 0)) ((bounds 1) 0) (p 0)))
    (if (< (p 1) ((bounds 0) 1)) ((bounds 0) 1)
        (if (> (p 1) ((bounds 1) 1)) ((bounds 1) 1) (p 1)))])

(defn- _snapshot-incident
  "Copy the records, boxes and samples of every edge incident to v."
  [st v]
  (let [m @{}]
    (each i ((st :incident) v)
      (put m i @[((st :recs) i) ((st :boxes) i) ((st :samples) i)]))
    m))

(defn- _restore-incident
  "Put back what _snapshot-incident took."
  [st v m]
  (eachp [i trio] m
         (put (st :recs) i (trio 0))
         (put (st :boxes) i (trio 1))
         (put (st :samples) i (trio 2))))

(defn- _revert-vertex
  "Revert v's position, incident records and matrix rows."
  [st v save-pt snap]
  (put (st :pacc) v save-pt)
  (_restore-incident st v snap)
  (_rebuild-rows st v))

(defn- _incident-energy
  "Repulsion energy over every pair with at least one edge incident to v."
  [st v]
  (var e 0.0)
  (when (st :spring)
    (each i ((st :incident) v)
      (each j (range (st :n))
        (when (and (not= j i) (_bboxes-touch? ((st :boxes) i) ((st :boxes) j)))
          (set e (+ e (_spring-energy ((st :samples) i) ((st :samples) j)
                                      (st :spring-cutoff))))))))
  e)

(defn- _try-vertex
  "Try each of the given offsets for vertex v and commit the best one."
  [st v dirs bounds]
  (let [save-pt (array ;((st :pacc) v))
        snap (_snapshot-incident st v)]
    (var best-pt save-pt)
    (var best-total (_incident-total st v))
    (var [best-cg best-cgd] (_incident-clearance st v))
    (var best-curvature (_incident-curvature st v))
    (var best-energy (_incident-energy st v))
    (each [dx dy] dirs
      (let [cand (_clamp-pt bounds @[(+ (save-pt 0) dx) (+ (save-pt 1) dy)])]
        (when (not (deep= cand save-pt))
          (put (st :pacc) v cand)
          (_rebuild-incident st v)
          (if (_any-degenerate? st v)
            (_revert-vertex st v save-pt snap)
            (do
              (_rebuild-rows st v)
              (let [t (_incident-total st v)
                    [cg cgd] (_incident-clearance st v)
                    c (_incident-curvature st v)
                    e (_incident-energy st v)]
                (_revert-vertex st v save-pt snap)
                (when (or (< t best-total)
                          (and (= t best-total) (< cg best-cg))
                          (and (= t best-total) (= cg best-cg) (< cgd best-cgd))
                          (and (= t best-total) (= cg best-cg) (= cgd best-cgd)
                               (< c best-curvature))
                          (and (= t best-total) (= cg best-cg) (= cgd best-cgd)
                               (= c best-curvature) (< e best-energy)))
                  (set best-total t)
                  (set best-cg cg)
                  (set best-cgd cgd)
                  (set best-curvature c)
                  (set best-energy e)
                  (set best-pt cand))))))))
    (put (st :pacc) v best-pt)
    (_rebuild-incident st v)
    (_rebuild-rows st v)))

(defn- _vertex-relax
  "Each vertex takes small steps (coarse to fine)that lower the drawing total."
  [st]
  (let [n (st :n)
        matrix (st :matrix)
        bends (st :bends)
        recs (st :recs)
        incident (st :incident)
        vertex-step (st :vertex-step)
        bounds (_bounds-of (st :pos))
        before (_matrix-total matrix n)
        [before-cg before-cgd] (_drawing-clearance st)
        before-energy (_drawing-energy st)
        before-curvature (_total-curvature recs)
        saved-pos (let [m @{}]
                    (eachp [k v] (st :pacc) (put m k (array ;v))) m)
        saved-bends (array ;bends)
        saved-matrix (let [m (array/new-filled n @[])]
                       (each i (range n) (put m i (array ;(matrix i))))
                       m)
        coarse-dirs @[[(- vertex-step) 0] [vertex-step 0]
                      [0 (- vertex-step)] [0 vertex-step]
                      [(- vertex-step) (- vertex-step)]
                      [vertex-step (- vertex-step)]
                      [(- vertex-step) vertex-step]
                      [vertex-step vertex-step]]]
    (loop [round :range [0 2]]
      (each v (sort (keys incident))
        (_try-vertex st v coarse-dirs bounds))
      (loop [level :range [1 (inc (st :relax-levels))]]
        (let [step2 (/ vertex-step (math/pow 2 level))
              fine-dirs @[[(- step2) 0] [step2 0]
                          [0 (- step2)] [0 step2]
                          [(- step2) (- step2)] [step2 (- step2)]
                          [(- step2) step2] [step2 step2]]]
          (each v (sort (keys incident))
            (_try-vertex st v fine-dirs bounds)))))
    (loop [p :range [0 (st :passes)]]
      (unless (_bend-pass st) (break)))
    (let [final-total (_matrix-total matrix n)
          [final-cg final-cgd] (_drawing-clearance st)
          final-curvature (_total-curvature recs)]
      (when (or (> final-total before)
                (and (= final-total before)
                     (> final-cg before-cg))
                (and (= final-total before)
                     (= final-cg before-cg)
                     (> final-cgd before-cgd))
                (and (= final-total before)
                     (= final-cg before-cg)
                     (= final-cgd before-cgd)
                     (or (> final-curvature before-curvature)
                         (and (= final-curvature before-curvature)
                              (>= (_drawing-energy st) before-energy)))))
        (eachp [k v] saved-pos (put (st :pacc) k (array ;v)))
        (each i (range n)
          (put bends i (saved-bends i))
          (put matrix i (array ;(saved-matrix i))))
        (_rebuild-all st)))))

#  edge-order sweep and backtracking

(defn- _edge-order-sweep
  "Greedily swap adjacent edge orders at each vertex, save improvements."
  [st]
  (var changed true)
  (var round 0)
  (while (and changed (< round 2))
    (set changed false)
    (set round (inc round))
    (each v (sort (keys (st :incident)))
      (let [ids ((st :incident) v)]
        (loop [k :range [0 (- (length ids) 1)]]
          (let [a (ids k) b (ids (inc k))
                mapping (get (st :slots) v)
                sa (get mapping a) sb (get mapping b)]
            (when (and (number? sa) (number? sb) (not= sa sb))
              (let [old-cap (_all-in-cap? (st :recs) (st :max-sweep))
                    old-total (_matrix-total (st :matrix) (st :n))
                    [old-cg old-cgd] (_drawing-clearance st)
                    old-curvature (_total-curvature (st :recs))
                    old-energy (_drawing-energy st)]
                (put mapping a sb)
                (put mapping b sa)
                (put (st :slots) v mapping)
                (_rebuild-incident st v)
                (_rebuild-rows st v)
                (let [new-total (_matrix-total (st :matrix) (st :n))
                      [new-cg new-cgd] (_drawing-clearance st)
                      better
                      (_order-better? old-cap
                                      (_all-in-cap? (st :recs)
                                                    (st :max-sweep))
                                      old-total new-total
                                      old-cg old-cgd new-cg new-cgd
                                      old-curvature (_total-curvature (st :recs))
                                      old-energy (_drawing-energy st))]
                  (unless better
                    (put mapping a sa)
                    (put mapping b sb)
                    (put (st :slots) v mapping)
                    (_rebuild-incident st v)
                    (_rebuild-rows st v))
                  (when better (set changed true)))))))))))

(defn- _refresh-all
  "Rebuild every record and every crossing-matrix entry."
  [st]
  (let [n (st :n) recs (st :recs) boxes (st :boxes) matrix (st :matrix)]
    (_rebuild-all st)
    (for i 0 n
      (for j (+ i 1) n
        (let [c (if (_bboxes-touch? (boxes i) (boxes j))
                  (_record-overlaps (recs i) (recs j))
                  0)]
          (put (matrix i) j c)
          (put (matrix j) i c))))))

(defn- _snapshot-all
  "Copy the recs, boxes, samples and crossing matrix."
  [st]
  (let [n (st :n)
        m (array/new n)]
    (for i 0 n (put m i (array ;((st :matrix) i))))
    @[(array ;(st :recs)) (array ;(st :boxes)) (array ;(st :samples)) m]))

(defn- _restore-all
  "Put back what _snapshot-all took."
  [st snap]
  (for i 0 (st :n)
    (put (st :recs) i ((snap 0) i))
    (put (st :boxes) i ((snap 1) i))
    (put (st :samples) i ((snap 2) i))
    (put (st :matrix) i ((snap 3) i))))

(defn- _copy-slots
  "Copy the per-vertex slot tables, so a swap can be undone."
  [slots]
  (let [copy @{}]
    (eachp [v mapping] slots
           (put copy v (merge @{} mapping)))
    copy))

(defn- _backtrack
  "DFS over slot swaps at each vertex, overlaps first, then poly count, curvature last."
  [st depth]
  (let [n (st :n)]
    (when (< (st :bt-visited) (st :backtrack-nodes))
      (put st :bt-visited (inc (st :bt-visited)))
      (let [current-total (_matrix-total (st :matrix) n)
            [current-cg current-cgd] (_drawing-clearance st)
            current-poly (_poly-count (st :recs))
            current-curvature (_total-curvature (st :recs))]
        (when (or (< current-total (st :bt-best-total))
                  (and (= current-total (st :bt-best-total))
                       (or (< current-cg (st :bt-best-cg))
                           (and (= current-cg (st :bt-best-cg))
                                (< current-cgd (st :bt-best-cgd)))
                           (and (= current-cg (st :bt-best-cg))
                                (= current-cgd (st :bt-best-cgd))
                                (or (< current-poly (st :bt-best-poly))
                                    (and (= current-poly (st :bt-best-poly))
                                         (< current-curvature
                                            (st :bt-best-curvature))))))))
          (put st :bt-best-total current-total)
          (put st :bt-best-cg current-cg)
          (put st :bt-best-cgd current-cgd)
          (put st :bt-best-poly current-poly)
          (put st :bt-best-curvature current-curvature)
          (put st :bt-best-slots (_copy-slots (st :slots))))
        (when (> depth 0)
          (each i (range n)
            (when (= (((st :recs) i) :kind) :poly)
              (each endpoint ((st :edges) i)
                (each j ((st :incident) endpoint)
                  (when (not= i j)
                    (let [mapping (get (st :slots) endpoint)
                          si (get mapping i) sj (get mapping j)]
                      (when (and (number? si) (number? sj) (not= si sj))
                        (let [snap (_snapshot-all st)]
                          (put mapping i sj)
                          (put mapping j si)
                          (_refresh-all st)
                          (when (<= (_matrix-total (st :matrix) n)
                                    (st :bt-baseline))
                            (_backtrack st (- depth 1)))
                          (put mapping i si)
                          (put mapping j sj)
                          (_restore-all st snap))))))))))))))

(defn- _edge-order-backtrack
  "Bounded depth-first search over slot swaps (run after greedy edge-order)."
  [st]
  (let [n (st :n)
        baseline (_matrix-total (st :matrix) n)]
    (put st :bt-baseline baseline)
    (put st :bt-visited 0)
    (put st :bt-best-total baseline)
    (let [[cg cgd] (_drawing-clearance st)]
      (put st :bt-best-cg cg)
      (put st :bt-best-cgd cgd))
    (put st :bt-best-poly (_poly-count (st :recs)))
    (put st :bt-best-curvature (_total-curvature (st :recs)))
    (put st :bt-best-slots (_copy-slots (st :slots)))
    (_backtrack st (st :backtrack-depth))
    (eachp [v mapping] (st :bt-best-slots)
           (put (st :slots) v mapping))
    (_refresh-all st)))

# ==============================================================================
#  Overlap reduction
# ==============================================================================

(defn _reduce-overlap
  ```Deterministic greedy reduction of arc-overlap crossings.

  Returns [recs overlaps-before overlaps-after positions slot-rot
           smallest-crossing-angle].```
  [pos edges slots deg tol meet join base-bend recs passes slot-rot sweep
   relax-levels spring spring-cutoff spring-samples arc-clearance node-clearance
   frame-sweep max-sweep vertex-relax vertex-step crossing-angle
   edge-order-sweep edge-order-backtrack backtrack-depth backtrack-nodes]
  (def n (length recs))
  (if (or (< n 2) (< passes 1))
    @[recs 0 0 pos slot-rot 0.0]
    (do
      (let [st (_reducer-state pos edges slots deg recs slot-rot
                               base-bend spring-samples
                               @{:bend base-bend :passes passes :sweep sweep
                                 :offsets (range 0 sweep)
                                 :relax-levels relax-levels
                                 :spring spring :spring-cutoff spring-cutoff
                                 :spring-samples spring-samples
                                 :arc-clearance arc-clearance
                                 :node-clearance node-clearance
                                 :frame-sweep frame-sweep :max-sweep max-sweep
                                 :vertex-relax vertex-relax
                                 :vertex-step vertex-step
                                 :crossing-angle crossing-angle
                                 :edge-order-sweep edge-order-sweep
                                 :edge-order-backtrack edge-order-backtrack
                                 :backtrack-depth backtrack-depth
                                 :backtrack-nodes backtrack-nodes
                                 :tol tol :meet meet :join join})
            before (_matrix-total (st :matrix) n)]
        (loop [p :range [0 passes]]
          (unless (_bend-pass st) (break)))
        (when (st :frame-sweep) (_frame-sweep st))
        (when (st :vertex-relax) (_vertex-relax st))
        (when (st :edge-order-sweep) (_edge-order-sweep st))
        (when (and (st :edge-order-sweep) (st :edge-order-backtrack)
                   (> (st :backtrack-nodes) 0) (<= n 30))
          (_edge-order-backtrack st))
        (def final-angle
          (if (st :crossing-angle)
            (do (var mn math/inf)
                (each i (range n)
                  (for j (inc i) n
                    (when (> (get ((st :matrix) i) j) 0)
                      (def aa (_record-crossing-angle ((st :recs) i) ((st :recs) j)))
                      (when (< aa mn) (set mn aa)))))
                (if (= mn math/inf) nil mn))
            nil))
        @[(st :recs) before (_matrix-total (st :matrix) n)
          (st :pacc) (st :rot-acc) final-angle]))))


# ==============================================================================
#  Rotational symmetry
# ==============================================================================

(defn- _rotate-point
  [p c theta]
  (def co (math/cos theta))
  (def si (math/sin theta))
  (def dx (- (p 0) (c 0)))
  (def dy (- (p 1) (c 1)))
  @[(+ (c 0) (- (* dx co) (* dy si)))
    (+ (c 1) (+ (* dx si) (* dy co)))])

(defn- _rotate-record
  "Rotate every geometric field of an edge record about the drawing centre."
  [rec c theta]
  (defn rp [p] (_rotate-point p c theta))
  (def out (merge @{} rec))
  (def p0 (rp @[(rec :x0) (rec :y0)]))
  (def p1 (rp @[(rec :x1) (rec :y1)]))
  (put out :x0 (p0 0))
  (put out :y0 (p0 1))
  (put out :x1 (p1 0))
  (put out :y1 (p1 1))
  (def segs @[])
  (each s (rec :segments)
    (def end (rp @[(s :x) (s :y)]))
    (if (s :line)
      (array/push segs @{:line true :x (end 0) :y (end 1)})
      (let [cc (rp @[(s :cx) (s :cy)])]
        (array/push segs @{:line false :cx (cc 0) :cy (cc 1) :r (s :r)
                           :cw (s :cw) :x (end 0) :y (end 1)}))))
  (put out :segments segs)
  (when (rec :joint)
    (def j (rp (rec :joint)))
    (put out :joint j))
  out)


(defn- _reverse-record
  "Reverse an edge record's traversal direction keeping the same geometry."
  [rec]
  (def out (merge @{} rec))
  (put out :u (rec :v))
  (put out :v (rec :u))
  (put out :x0 (rec :x1))
  (put out :y0 (rec :y1))
  (put out :x1 (rec :x0))
  (put out :y1 (rec :y0))
  (def pairs @[])
  (var cur @[(rec :x0) (rec :y0)])
  (each seg (rec :segments)
    (array/push pairs @[cur seg])
    (set cur @[(seg :x) (seg :y)]))
  (def segs @[])
  (each pair (reverse pairs)
    (def a (pair 0))
    (def seg (pair 1))
    (if (seg :line)
      (array/push segs @{:line true :x (a 0) :y (a 1)})
      (array/push segs @{:line false :cx (seg :cx) :cy (seg :cy) :r (seg :r)
                         :cw (not (seg :cw)) :x (a 0) :y (a 1)})))
  (put out :segments segs)
  out)

(defn- _coverage-overlaps
  [recs]
  (var total 0)
  (loop [i :range [0 (length recs)]]
    (loop [j :range [(+ i 1) (length recs)]]
      (set total (+ total (_record-overlaps (recs i) (recs j))))))
  total)

(defn- _rotational-candidates
  "Folds that can act on n vertices, including a fixed central vertex."
  [n]
  (seq [k :range [2 (+ n 1)]
        :when (or (= 0 (mod n k)) (= 0 (mod (- n 1) k)))]
       k))

(defn- _find-rotational-symmetry
  "Best rotation of the placed graph that is both geometric and combinatorial."
  [g pos]
  (def ns (nodes g))
  (def n (length ns))
  (when (and (>= n 3) (<= n 128) (all |(get pos $) ns))
    (def pts (map |(get pos $) ns))
    (def nset @{})
    (each v ns (put nset v true))
    (def c (_radial-centre pos nset))
    (def span (reduce max 0.0
                      (map |(math/hypot (- ($ 0) (c 0)) (- ($ 1) (c 1))) pts)))
    (var min-gap math/inf)
    (loop [i :range [0 n]]
      (loop [j :range [(+ i 1) n]]
        (def d (math/hypot (- ((pts i) 0) ((pts j) 0))
                           (- ((pts i) 1) ((pts j) 1))))
        (when (< d min-gap) (set min-gap d))))
    (def edges (_cm__unique-edges g))
    (def eset @{})
    (each e edges
      (def u (e 0)) (def v (e 1))
      (unless (get eset u) (put eset u @{}))
      (unless (get eset v) (put eset v @{}))
      (put (get eset u) v true)
      (put (get eset v) u true))
    (var best nil)
    (each k (_rotational-candidates n)
      (each dir [1 -1]
        (def theta (* dir (/ two-pi k)))
        (def perm @{})
        (def used @{})
        (var ok true)
        (var worst 0.0)
        (var sum 0.0)
        (each v ns
          (def q (_rotate-point (get pos v) c theta))
          (var bi nil)
          (var bd math/inf)
          (each w ns
            (def p (get pos w))
            (def d (math/hypot (- (q 0) (p 0)) (- (q 1) (p 1))))
            (when (< d bd) (set bd d) (set bi w)))
          (if (get used bi) (set ok false) (put used bi true))
          (put perm v bi)
          (when (> bd worst) (set worst bd))
          (set sum (+ sum bd)))
        (when ok
          (var good true)
          (each e edges
            (def u (e 0)) (def v (e 1))
            (unless (get-in eset [(get perm u) (get perm v)])
              (set good false)))
          (when good
            (each v ns
              (var w v)
              (for _ 0 k (set w (get perm w)))
              (unless (= w v) (set good false))))
          (when (and good
                     (> span 1e-9)
                     (> min-gap 1e-9)
                     (<= worst (* 0.08 span))
                     (<= worst (* 0.5 min-gap)))
            (when (or (nil? best)
                      (> k (best :k))
                      (and (= k (best :k)) (< worst (best :worst))))
              (set best @{:k k :theta theta :perm perm :centre c
                          :worst worst :mean (/ sum n) :span span
                          :min-gap min-gap}))))))
    best))

(defn- _snap-positions
  "Move each vertex to the average of its orbit, then rotate it back into place."
  [pos sym]
  (def perm (sym :perm))
  (def k (sym :k))
  (def theta (sym :theta))
  (def c (sym :centre))
  (def seen @{})
  (def out @{})
  (eachp [v _] pos
         (when (and (get perm v) (not (get seen v)))
           (def orbit @[])
           (var w v)
           (for _ 0 k
             (array/push orbit w)
             (set w (get perm w)))
           (var sx 0.0)
           (var sy 0.0)
           (for j 0 k
             (def q (_rotate-point (get pos (orbit j)) c (* (- j) theta)))
             (set sx (+ sx (q 0)))
             (set sy (+ sy (q 1))))
           (def base @[(/ sx k) (/ sy k)])
           (for j 0 k
             (def u (orbit j))
             (put out u (_rotate-point base c (* j theta)))
             (put seen u true))))
  out)

(defn- _symmetry-orbits
  "Directed edge steps of each rotation orbit, starting at a canonical edge."
  [edges perm k]
  (def done @{})
  (def orbits @[])
  (each e edges
    (def u0 (e 0))
    (def v0 (e 1))
    (unless (get-in done [u0 v0])
      (def orbit @[])
      (var u u0)
      (var v v0)
      (for j 0 k
        (array/push orbit @[u v])
        (set u (get perm u))
        (set v (get perm v)))
      (each e2 orbit
        (def a (e2 0))
        (def b (e2 1))
        (unless (get done a) (put done a @{}))
        (unless (get done b) (put done b @{}))
        (put (get done a) b true)
        (put (get done b) a true))
      (array/push orbits orbit)))
  orbits)

(defn- _symmetrise-arcs
  "Route one edge orbit from a chosen vertex, then rotate into every slot."
  [arcs edges perm theta c k &opt starts]
  (def idx @{})
  (eachp [ei e] edges
         (def u (e 0)) (def v (e 1))
         (unless (get idx u) (put idx u @{}))
         (unless (get idx v) (put idx v @{}))
         (put (get idx u) v ei)
         (put (get idx v) u ei))
  (def by-key @{})
  (def orbits (_symmetry-orbits edges perm k))
  (eachp [oi orbit] orbits
         (def s (if starts (get starts oi 0) 0))
         (def [su sv] (orbit s))
         (def rep (let [r (arcs (get-in idx [su sv]))]
                    (if (< su sv) r (_reverse-record r))))
         (eachp [j e] orbit
                (def u (e 0))
                (def v (e 1))
                (def rr (if (= j s)
                          (do (def copy (merge @{} rep))
                              (put copy :u u) (put copy :v v)
                              copy)
                          (_rotate-record rep c (* theta (- j s)))))
                (put rr :u u)
                (put rr :v v)
                (unless (get by-key u) (put by-key u @{}))
                (put (get by-key u) v rr)
                (unless (get by-key v) (put by-key v @{}))
                (put (get by-key v) u rr)))
  (var complete true)
  (def out @[])
  (each e edges
    (def u (e 0)) (def v (e 1))
    (def rr (get-in by-key [u v]))
    (if rr
      (array/push out rr)
      (do (set complete false) (array/push out nil))))
  (if complete out nil))


(defn- _symmetry-result
  "Return a symmetry-routed result, or nil if the default route is better."
  [g kws result recur]
  (when (and (get kws :symmetry false)
             (not (structurally-directed? g))
             (nil? (get kws :arcs)))
    (def pos (result :pos))
    (def sym (_find-rotational-symmetry g pos))
    (when sym
      (def snapped (_snap-positions pos sym))
      (def moved
        (reduce max 0.0
                (map (fn [v]
                       (def a (get pos v))
                       (def b (get snapped v))
                       (math/hypot (- (a 0) (b 0)) (- (a 1) (b 1))))
                     (nodes g))))
      (def exact? (< moved (* 1e-9 (max 1.0 (sym :span)))))
      (def sub-kws (table ;(kvs kws)))
      (put sub-kws :base :pos)
      (put sub-kws :pos snapped)
      (put sub-kws :slot-order :radial)
      (put sub-kws :slot-rot nil)
      (put sub-kws :reduce-overlap false)
      (put sub-kws :symmetry false)
      (def sub (recur sub-kws))
      (def original (if (result :arcs) (result :arcs) @[]))
      (def before (or (result :overlaps-after) (_coverage-overlaps original)))

      (defn build [source starts]
        (def sym-arcs
          (_symmetrise-arcs (source :arcs) (source :edges)
                            (sym :perm) (sym :theta) (sym :centre) (sym :k)
                            starts))
        (when sym-arcs
          @{:arcs sym-arcs
            :total (_coverage-overlaps sym-arcs)
            :residual (/ (reduce (fn [s a] (+ s (a :residual))) 0.0 sym-arcs)
                         (max 1 (length sym-arcs)))
            :edges (source :edges)
            :slots (source :slots)
            :deg (source :deg)
            :slot-rot (source :slot-rot)}))

      (defn better? [c best]
        (and c
             (or (nil? best)
                 (< (c :total) (best :total))
                 (and (= (c :total) (best :total))
                      (< (c :residual) (best :residual))))))

      (defn greedy [source orbits starts best]
        (var b best)
        (var pass 0)
        (while (< pass 2)
          (def start-total (b :total))
          (eachp [oi orbit] orbits
                 (var chosen (get starts oi 0))
                 (eachp [s _] orbit
                        (put starts oi s)
                        (def c (build source starts))
                        (when (better? c b)
                          (set b c)
                          (set chosen s)))
                 (put starts oi chosen))
          (when (= (b :total) start-total) (break))
          (++ pass))
        b)

      (defn pair-pass [source orbits starts best]
        (var b best)
        (for i 0 (length orbits)
          (for j (+ i 1) (length orbits)
            (def oi (orbits i))
            (def oj (orbits j))
            (def bi (get starts i 0))
            (def bj (get starts j 0))
            (var found nil)
            (eachp [si _] oi
                   (eachp [sj _] oj
                          (put starts i si)
                          (put starts j sj)
                          (def c (build source starts))
                          (when (better? c b)
                            (set b c)
                            (set found @[si sj]))))
            (if found
              (do (put starts i (found 0))
                  (put starts j (found 1)))
              (do (put starts i bi)
                  (put starts j bj)))))
        b)

      (defn candidate [source search?]
        (def orbits
          (_symmetry-orbits (source :edges) (sym :perm) (sym :k)))
        (def starts @{})
        (var best (build source starts))
        (when (and search? best (<= (sym :k) 8))
          (set best (greedy source orbits starts best))
          (when (<= (length orbits) 16)
            (set best (pair-pass source orbits starts best)))
          (set best (greedy source orbits starts best)))
        best)

      (def raw-candidates
        (if (and exact? (result :arcs) (result :edges))
          [(candidate result true) (candidate sub false)]
          [(candidate sub false)]))

      (def sym-candidate
        (reduce (fn [best c]
                  (if (nil? best)
                    c
                    (let [bt (best :total) ct (c :total)]
                      (if (or (< ct bt)
                              (and (= ct bt)
                                   (< (c :residual) (best :residual))))
                        c best))))
                nil
                (seq [c :in raw-candidates :when c] c)))

      (when sym-candidate
        (def total (sym-candidate :total))
        (def sym-residual (sym-candidate :residual))
        (def old-residual (or (result :residual) sym-residual))
        (def node-radius (get kws :node-radius 0.25))
        (when (and (<= total before)
                   (or (< total before)
                       (<= sym-residual (+ old-residual 1e-9))))
          @{:pos snapped :eligible true :iterations 0
            :residual sym-residual
            :lombardi true
            :poly (not (empty? (seq [a :in (sym-candidate :arcs)
                                     :when (= (a :kind) :poly)] true)))
            :arcs (sym-candidate :arcs)
            :edges (sym-candidate :edges)
            :slots (sym-candidate :slots)
            :deg (sym-candidate :deg)
            :slot-rot (sym-candidate :slot-rot)
            :overlaps-before (or (result :overlaps-before) total)
            :overlaps-after total
            :crossings-graph-before (_graph-crossings original)
            :crossings-graph-after (_graph-crossings (sym-candidate :arcs))
            :crossings-visible-before (_visible-crossings original node-radius)
            :crossings-visible-after (_visible-crossings (sym-candidate :arcs) node-radius)
            :min-arc-clearance (_min-arc-clearance (sym-candidate :arcs))
            :min-node-clearance (_min-node-clearance (sym-candidate :arcs) snapped)
            :crossing-angle nil
            :symmetry @{:k (sym :k) :theta (sym :theta)}})))))

# ==============================================================================
#  layout-polyarc
# ==============================================================================

(defn layout-polyarc
  ```Poly-arc Lombardi drawing using tangent-slot construction.

 Every edge is drawn as a circular arc (or a pair of spliced arcs) with the
 tangent directions at every vertex equally spaced at 2*pi/deg(v).

  &keys
    :width          — canvas width  (default 10.0)
    :height         — canvas height (default 8.0)

    :base           — initial vertex placement: :spring (default),
                      :circle, :spectral, :radial, or :pos

    :pos            — {node [x y]} used when :base is :pos

    :spring-iterations — iterations of the spring layout (default 300)

    :slot-rot       — rotation of the tangent slot frame, or nil for none
                      (default), a global rotation in radians, or a
                      {vertex [radian]} table of per-vertex offsets

    :slot-order     — :embedding (default) takes the circular order from the
                      planar rotation system, :radial sorts each vertex's
                      incident edges by angle from the inward radius

    :symmetry       — detect rotational symetry and use exact orbits when
                      true (default false)

    :tol            — single-arc acceptance, endpoint tangent mismatch
                      in radians (default 0.25) larger values keep more
                      edges as single arcs

    :meet-angle     — meeting angle of the arcs of a poly edge, in radians
                     (default pi)

    :joint          — poly-edge splice: :locus (default) or :midpoint

    :bend           — rotation of the locus joint around its circle, in
                      radians (default 0.0)

    :reduce-overlap — the edge reduction stages. set to false, crossing quality
                      will decrease, but not correctness.

    :reduce-passes  — greedy sweep iterations (default 2) stops as soon as
                      a pass doesn't change the result.

    :sweep          — candidate joint rotations per poly-edge per pass,
                      evenly spaced in [0 .. pi) (default 8)

    :node-radius    — node radius in layout units (default 0.25)

    :max-sweep      — cap on any single arc segment's sweep, in radians
                      (default 1.0).

    :relax-levels   — refinement rounds run after the coarse sweep (default 4)

    :frame-sweep    — when true (default) finds rotations that minimizes
                      arc crossings.

    :vertex-relax   — when true (default), relax vertex positions with
                      steps that lower the drawing total

    :vertex-step    — size of vertex relaxation steps (default 0.05)

    :crossing-angle — when true, rank candidates by a larger smallest crossing
                      angle (arc-RAC) before repulsion energy (default false)

    :spring         —  break overlap-count ties by minimizing residual
                      arc-arc proximity energy (default true)

    :spring-cutoff  — pairwise sample distance below which spring energy
                      accrues (default 0.35)

    :spring-samples — samples per arc used to estimate spring energy
                      (default 8)

    :arc-clearance  — minimum desired distance between sampled points of
                      non-crossing, non-adjacent arcs (default 0.35)
                      false when :symmetry is true, false disables.

    :node-clearance — minimum desired distance between nodes.

    :edge-order-sweep — greedy adjacent edge-order swaps at each vertex
                      (default true)

    :edge-order-backtrack — bounded DFS to to reduce overlaps (default true)
    :backtrack-depth — maximum slot-swap depth (default 3)
    :backtrack-nodes — maximum backtracking nodes (default 8)

    :arcs           — use precomputed edges rather than construction
                      ({:x0 :y0 :x1 :y1 :kind :segments ...})
                      (default nil)

  Returns
     {:pos {node [x y]}            ; vertex positions (the spring drawing)
      :eligible true
      :iterations 0                ; the construction is direct
      :residual f                  ; mean single-arc tangent mismatch (radians)
      :lombardi true               ; perfect angular resolution
      :poly bool                   ; any edge needed two arcs
      :edges [[u v] ...]           ; structural edges, in :arcs order
      :slots {v {edge-index slot}} ; tangent slot table used for embedding
                                   ;   and position fallback
      :deg {v degree}
      :overlaps-before n         ; reducer overlap/touch count before reduction
      :overlaps-after n          ; reducer overlap/touch count after reduction

      :min-arc-clearance f       ; closest sampled approach between two
                                 ;   non-crossing, non-adjacent arcs (nil when
                                 ;   there is no such pair)

      :min-node-clearance f      ; closest sampled approach from an arc to a
                                 ;   node it does not touch (nil when there is
                                 ;   no such pair)

      :crossings-graph-before n   ; non-adjacent transversal crossings before
      :crossings-graph-after n    ;   and after reduction (graph-theoretic)

      :crossings-visible-before n ; visible distinct crossings before and
      :crossings-visible-after n  ;   after reduction

      :symmetry {:k n :theta a}   ; rotation used, when symmetry routing won

      :arcs [{:u u :v v :x0 y0    ; one record per edge; :segments chain
              :x1 y1 :kind ...}   ;   from (x0 y0) to (x1 y1)

             {:kind :line ...} | {:kind :arc ...} | {:kind :poly ...}]}
```
  [g & opts]
  (let [kws (table ;opts)
        width (get kws :width 10.0)
        height (get kws :height 8.0)
        spring-iterations (get kws :spring-iterations 300)
        tol (get kws :tol 0.25)
        slot-order (get kws :slot-order :embedding)
        slot-rot (get kws :slot-rot nil)
        meet (get kws :meet-angle math/pi)
        bend (get kws :bend 0.0)
        join (get kws :joint :locus)
        reduce-overlap (get kws :reduce-overlap true)
        reduce-passes (get kws :reduce-passes 2)
        sweep (get kws :sweep 8)
        max-sweep (get kws :max-sweep 1.0)
        node-radius (get kws :node-radius 0.25)
        relax-levels (get kws :relax-levels 4)
        frame-sweep (get kws :frame-sweep true)
        spring (get kws :spring true)
        spring-cutoff (get kws :spring-cutoff 0.35)
        spring-samples (get kws :spring-samples 8)
        arc-clearance (let [v (get kws :arc-clearance :arc-clearance-unset)
                            clear-default (if (get kws :symmetry false)
                                            nil
                                            spring-cutoff)]
                        (cond
                          (= v :arc-clearance-unset) clear-default
                          v v
                          :else nil))
        node-clearance (let [v (get kws :node-clearance :node-clearance-unset)
                             clear-default (if (get kws :symmetry false)
                                             nil
                                             (+ node-radius 0.2))]
                         (cond
                           (= v :node-clearance-unset) clear-default
                           v v
                           :else nil))
        arcs (get kws :arcs nil)
        vertex-relax (get kws :vertex-relax true)
        vertex-step (get kws :vertex-step 0.05)
        crossing-angle (get kws :crossing-angle false)
        edge-order-sweep (get kws :edge-order-sweep true)
        edge-order-backtrack (get kws :edge-order-backtrack true)
        backtrack-depth (get kws :backtrack-depth 3)
        backtrack-nodes (get kws :backtrack-nodes 8)
        n (length (nodes g))]

    (def result
      (cond
        (<= n 0)
        @{:pos @{} :eligible true :iterations 0 :residual 0.0
          :lombardi true :poly false :arcs @[]}
        (= n 1)
        (let [v ((nodes g) 0)]
          @{:pos @{v [(/ width 2) (/ height 2)]} :eligible true
            :iterations 0 :residual 0.0
            :lombardi true :poly false :arcs @[]})
        true
        (do
          (let [pos (case (get kws :base :spring)
                      :spring (get (_sp_layout-spring
                                    g :width width :height height
                                    :iterations spring-iterations
                                    :smooth-iterations 0) :pos)
                      :circle (_circle-base g width height)
                      :circular (_circle-base g width height) # alias
                      :spectral (get (_s_layout-spectral
                                      g :width width
                                      :height height) :pos)
                      :radial (get (_r_layout-radial
                                    g :width width
                                    :height height) :pos)
                      :pos (get kws :pos @{}))
                effective-slot-rot (cond
                                     (and (= slot-order :radial) (nil? slot-rot))
                                     (_radial-slot-rot pos)
                                     (let [base (get kws :base)]
                                       (or (= base :circle) (= base :circular)))
                                     (_radius-slot-rot pos width height)
                                     true 0.0)]
            (if arcs
              @{:pos pos :eligible true :iterations 0 :residual 0.0
                :lombardi true :poly false :arcs arcs
                :edges (_cm__unique-edges g) :slots @{} :deg @{}}
              (do
                (let [edges (_cm__unique-edges g)
                      rec-edges (seq [e :in edges
                                      :when (and (get pos (e 0)) (get pos (e 1)))]
                                     e)
                      deg (_degrees rec-edges)
                      slots (if (= slot-order :radial)
                              (_radial-slots pos rec-edges)
                              (or (_embedding-slots g rec-edges)
                                  (_slot-table pos rec-edges)))
                      slot-rot* (if (and (= slot-order :radial) (nil? slot-rot))
                                  (_radial-slot-rot pos)
                                  slot-rot)]
                  (def recs @[])
                  (var residual 0.0)
                  (var any-poly false)
                  (loop [ei :range [0 (length rec-edges)]]
                    (let [e (rec-edges ei)
                          u (e 0) v (e 1)
                          pu (get pos u) pv (get pos v)]
                      (when (and pu pv)
                        (def rec (_edge-record u v pu pv
                                               (_slot-angle slots deg u ei slot-rot*)
                                               (_slot-angle slots deg v ei slot-rot*)
                                               tol meet bend join max-sweep))
                        (array/push recs rec)
                        (set residual (+ residual (rec :residual)))
                        (when (= (rec :kind) :poly) (set any-poly true)))))
                  (if (and reduce-overlap (not (empty? recs)))
                    (do
                      (let [[recs2 before after pos2 final-slot-rot final-angle]
                            (_reduce-overlap
                             pos rec-edges slots deg tol meet join
                             bend recs reduce-passes effective-slot-rot sweep
                             relax-levels spring spring-cutoff
                             spring-samples arc-clearance node-clearance
                             frame-sweep max-sweep
                             vertex-relax vertex-step crossing-angle edge-order-sweep
                             edge-order-backtrack backtrack-depth backtrack-nodes)]
                        (def graph-before (_graph-crossings recs))
                        (def graph-after (_graph-crossings recs2))
                        (def visible-before (_visible-crossings recs node-radius))
                        (def visible-after (_visible-crossings recs2 node-radius))

                        @{:pos pos2 :eligible true :iterations 0
                          :residual (/ (reduce (fn [s a] (+ s (a :residual))) 0.0 recs2)
                                       (max 1 (length recs2)))
                          :lombardi true
                          :poly (not (empty? (seq [a :in recs2
                                                   :when (= (a :kind) :poly)] true)))
                          :arcs recs2
                          :edges edges :slots slots :deg deg
                          :slot-rot final-slot-rot
                          :overlaps-before before :overlaps-after after
                          :min-arc-clearance (_min-arc-clearance recs2)
                          :min-node-clearance (_min-node-clearance recs2 pos2)
                          :crossings-graph-before graph-before
                          :crossings-graph-after graph-after
                          :crossings-visible-before visible-before
                          :crossings-visible-after visible-after
                          :crossing-angle final-angle}))

                    @{:pos pos :eligible true :iterations 0
                      :residual (/ residual (length recs))
                      :lombardi true :poly any-poly :arcs recs
                      :edges edges :slots slots :deg deg
                      :overlaps-before 0 :overlaps-after 0
                      :min-arc-clearance (_min-arc-clearance recs)
                      :min-node-clearance (_min-node-clearance recs pos)
                      :crossings-graph-before (_graph-crossings recs)
                      :crossings-graph-after (_graph-crossings recs)
                      :crossings-visible-before (_visible-crossings recs node-radius)
                      :crossings-visible-after (_visible-crossings recs node-radius)}))))))))

    (or (_symmetry-result
         g kws result (fn [sub-kws] (layout-polyarc g ;(kvs sub-kws))))
        result)))
