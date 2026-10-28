# -*- mode: janet; -*-
# graphs/layout/spring: spring-force layout with planar embedding

#  A layout for undirected graphs using the deterministic Fruchterman-Reingold
#  spring simulation. If the graph is planar and connected builds a barycentric
#  straight-line embedding (Tutte) from a planar embedding of the graph.
#  The better of the two layouts is returned as measured by the number of edge
#  crossings. A planar graph will produce a crossing free layout whenever the
#  spring model tangles. Non-planar graphs use the spring layout and metadata.

(import deft :prefix "")
(import ../types :prefix "")
(import ../util :prefix "")
(import ../planarity :prefix "")
(import ./force :prefix "_f_")
(import ./common :prefix "_cm_")


# ==============================================================================
#  Straight-line crossing count
# ==============================================================================

(defn- _box-of
  "Closed bounding box [x0 x1 y0 y1] of edge `e` in `pos`, or
[inf -inf inf -inf] when an endpoint has no position."
  [pos e]
  (let [pa (get pos (e 0))
        pb (get pos (e 1))]
    (if (and pa pb)
      @[(min (pa 0) (pb 0)) (max (pa 0) (pb 0))
        (min (pa 1) (pb 1)) (max (pa 1) (pb 1))]
      @[math/inf (* -1 math/inf) math/inf (* -1 math/inf)])))

(defn- _edge-boxes
  "Per edge box for the drawing in `pos`, in edge order (see _box-of)."
  [edges pos]
  (def boxes @[])
  (loop [i :range [0 (length edges)]]
    (array/push boxes (_box-of pos (edges i))))
  boxes)

(defn- _refit-box
  "Recompute edge `i`'s box after one of its endpoints moved."
  [boxes edges pos i]
  (put boxes i (_box-of pos (edges i))))

(defn- _boxes-overlap?
  "True when two edge boxes overlap, so the edges might cross."
  [bi bj]
  (and (<= (get bi 0) (get bj 1))
       (<= (get bj 0) (get bi 1))
       (<= (get bi 2) (get bj 3))
       (<= (get bj 2) (get bi 3))))

(defn- count-crossings
  "Number of proper crossings of the straight-line drawing given by pos."
  [edges pos]
  (def boxes (_edge-boxes edges pos))
  (var cnt 0)
  (loop [i :range [0 (length edges)]]
    (def a ((edges i) 0)) (def b ((edges i) 1))
    (def pa (get pos a)) (def pb (get pos b))
    (when (and pa pb)
      (loop [j :range [(+ i 1) (length edges)]]
        (when (_boxes-overlap? (boxes i) (boxes j))
          (def c ((edges j) 0)) (def d ((edges j) 1))
          (def pc (get pos c)) (def pd (get pos d))
          (when (and (not= a c) (not= a d) (not= b c) (not= b d)
                     (_cm__segments-cross? pa pb pc pd))
            (set cnt (+ cnt 1)))))))
  cnt)


# ==============================================================================
#  Crossing-reduction post-pass
# ==============================================================================

(defn- _incident-index
  "vertex -> {:inc [edge indices ascending] :set {edge indices}}."
  [edges]
  (let [inc @{}
        sets @{}]
    (loop [i :range [0 (length edges)]]
      (def e (edges i))
      (each v [(e 0) (e 1)]
        (put inc v (array/push (get inc v @[]) i))
        (put (get sets v @{}) i true)))
    @{:inc inc :sets sets}))

(defn- _touch-crossings
  "Number of crossings among edge pairs where at least one edge has vertex v."
  [edges pos boxes v inc inc-set]
  (var cnt 0)
  (def m (length edges))
  (defn pair-counts [i j]
    (when (_boxes-overlap? (boxes i) (boxes j))
      (def e1 (edges i)) (def a (e1 0)) (def b (e1 1))
      (def e2 (edges j)) (def c (e2 0)) (def d (e2 1))
      (when (and (not= a c) (not= a d) (not= b c) (not= b d)
                 (let [pa (get pos a) pb (get pos b) pc (get pos c) pd (get pos d)]
                   (_cm__segments-cross? pa pb pc pd)))
        (set cnt (+ cnt 1)))))
  (loop [i :range [0 m]]
    (if (get inc-set i)
      # i has v. it can pair with any later edge
      (loop [j :range [(+ i 1) m]] (pair-counts i j))
      # i does not have v. the only partners are v's other edges above i
      (each j inc (when (> j i) (pair-counts i j)))))
  cnt)

(defn- _rewind
  "Undo position writes at indices `mark` and above by replaying the journal backwards."
  [pos journal mark]
  (var i (dec (length journal)))
  (while (>= i mark)
    (def e (journal i))
    (def v (e 0))
    (def p (pos v))
    (put p 0 (e 1))
    (put p 1 (e 2))
    (set i (dec i))))

(defn- _smooth-crossings
  "Deterministic stochastic descent which lowers the crossing count."
  [g edges pos width height iterations base shrink]
  (math/seedrandom 42)
  (def idx (_incident-index edges))
  (def incs (idx :inc))
  (def sets (idx :sets))
  (each [k v] (pairs pos) (put pos k @[(v 0) (v 1)]))
  (var best (count-crossings edges pos))
  (var cur best)
  (var s base)
  # Per-edge bounding boxes
  (def boxes (_edge-boxes edges pos))
  # Journal of every position write, as [vertex old-x old-y]
  (def journal @[])
  (var mark 0)
  (loop [i :range [0 iterations]]
    (each v (nodes g)
      (let [inc (get incs v @[])
            inc-set (get sets v @{})
            base-n (_touch-crossings edges pos boxes v inc inc-set)
            dx (- (math/random) 0.5)
            dy (- (math/random) 0.5)
            nx (max 0.5 (min (- width 0.5) (+ ((pos v) 0) (* dx s))))
            ny (max 0.5 (min (- height 0.5) (+ ((pos v) 1) (* dy s))))
            p (pos v)]
        (array/push journal @[v (p 0) (p 1)])
        (put p 0 nx)
        (put p 1 ny)
        (each j inc (_refit-box boxes edges pos j))
        (set cur (+ cur (- (_touch-crossings edges pos boxes v inc inc-set) base-n)))
        (when (< cur best)
          (set best cur)
          (set mark (length journal)))))
    (set s (* s shrink)))
  (_rewind pos journal mark)
  best)


# ==============================================================================
#  Barycentric (Tutte) embedding from a planar embedding
# ==============================================================================

(defn- _outer-face
  "Face with most boundary vertices in a face list."
  [faces]
  (var best nil)
  (var best-len -1)
  (each f faces
    (when (> (length (f :vs)) best-len)
      (set best f)
      (set best-len (length (f :vs)))))
  best)

(defn- _connected?
  "True when g is connected (BFS from the first node)."
  [g]
  (def all (nodes g))
  (when (> (length all) 0)
    (let [start (all 0)
          seen @{}
          qq @[start]]
      (put seen start true)
      (var head 0)
      (while (< head (length qq))
        (def cur (qq head))
        (set head (+ head 1))
        (each y (neighbours g cur)
          (unless (get seen y)
            (put seen y true)
            (array/push qq y))))
      (= (length seen) (length all)))))

(defn- _tutte-embed
  ```Barycentric straight-line embedding of a connected planar graph.

Puts the vertices of the largest face on a circle (ordered along the boundary
walk) and solves, by Gauss-Seidel sweeps, x_v = (1/deg v) sum_{u~v} x_u  for
every interior vertex.

Returns @{:pos ...} or nil when the graph has no cyclic face.
```
  [g faces sweeps width height]
  (def outer (_outer-face faces))
  (when (and outer (>= (length (outer :vs)) 3))
    (def boundary (array/slice (outer :vs)))
    (def b-set @{})
    (each v boundary (put b-set v true))
    (def m (length boundary))
    (def step (/ (* 2 math/pi) m))
    (def cx (/ width 2)) (def cy (/ height 2))
    (def radius (* (min width height) 0.4))
    (def pos @{})
    (var i 0)
    (each v boundary
      (def ang (- (+ math/pi (* step i)) 0.0001))
      (put pos v [(+ cx (* radius (math/cos ang)))
                  (+ cy (* radius (math/sin ang)))])
      (set i (+ i 1)))
    (def interior (seq [v :in (nodes g) :when (nil? (get b-set v))] v))
    (each v interior (put pos v [cx cy]))
    (when (> (length interior) 0)
      (loop [s :range [0 sweeps]]
        (var max-disp 0.0)
        (each v interior
          (var sx 0.0) (var sy 0.0) (var cnt 0)
          (each u (neighbours g v)
            (def pu (get pos u))
            (when pu
              (set sx (+ sx (pu 0)))
              (set sy (+ sy (pu 1)))
              (set cnt (+ cnt 1))))
          (when (> cnt 0)
            (def ox ((pos v) 0)) (def oy ((pos v) 1))
            (def nx (/ sx cnt)) (def ny (/ sy cnt))
            (put pos v [nx ny])
            (set max-disp (max max-disp
                               (+ (math/abs (- nx ox)) (math/abs (- ny oy)))))))
        (when (< max-disp 1e-9) (break))))
    @{:pos pos}))


# ==============================================================================
#  layout-spring
# ==============================================================================

(define layout-spring
  ```Deterministic spring-force layout with planar-embedding fallback.

Runs a Fruchterman-Reingold spring simulation on the graph. For planar connected
undirected graphs it also computes a barycentric (Tutte) embedding from the
graph's planar faces. Whichever drawing has fewer edge crossings is returned.
The Tutte drawing of a planar graph is planar, i.e. zero crossings.

  &keys
    :width           — canvas width  (default 10.0)
    :height          — canvas height (default 8.0)
    :iterations      — spring cooling iterations (default 300)
    :gravity         — spring pull toward center (default 0.05)
    :planar-fallback — enable the Tutte fallback for planar graphs
                       (default true)
    :tutte-sweeps    — Gauss-Seidel sweeps of the barycentric solve
                       (default 1000)
    :smooth-iterations — stochastic-descent crossing-reduction pass run on
                       the spring drawing when the planar fallback was not
                       used (default 600; set to 0 to disable)

The crossing-reduction pass is deterministic (when given a fixed seed), so the
same graph should always produce the same drawing.

  Returns
     {:pos {node [x y]}
      :eligible true
      :iterations n
      :planar bool          ; g is planar
      :fallback bool        ; a planar embedding was used
      :crossings-before n   ; crossings of the spring drawing
      :crossings-after n}   ; crossings of the returned drawing
```
  [g :weighted & keys] :table
  (let [keys (table ;keys)
        width (get keys :width 10.0)
        height (get keys :height 8.0)
        iterations (get keys :iterations 300)
        gravity (get keys :gravity 0.05)
        planar-fallback (get keys :planar-fallback true)
        tutte-sweeps (get keys :tutte-sweeps 1000)
        smooth-iterations (get keys :smooth-iterations 600)
        n (length (nodes g))
        edges (_cm__unique-edges g)]
    (cond
      (<= n 0) @{:pos @{} :eligible true :iterations 0
                 :planar false :fallback false :crossings-before 0 :crossings-after 0}
      (= n 1)
      @{:pos (let [v ((nodes g) 0)] @{v [(/ width 2) (/ height 2)]})
        :eligible true :iterations 0
        :planar false :fallback false :crossings-before 0 :crossings-after 0}
      (let [spring (_f_layout-force g :width width :height height
                                    :iterations iterations :gravity gravity)
            spos (spring :pos)
            k (get spring :k 0.0)
            spring-cr (count-crossings edges spos)
            planar? (and planar-fallback
                         (not (structurally-directed? g))
                         (_connected? g)
                         (is-planar? g))
            faces (when planar? (planar-faces g))
            tp (when faces (_tutte-embed g faces tutte-sweeps width height))
            tpos (when tp (tp :pos))
            tutte-cr (when tpos (count-crossings edges tpos))
            use-tutte (when (and planar? tpos tutte-cr)
                        (< tutte-cr spring-cr))]
        (if use-tutte
          @{:pos tpos :eligible true :iterations iterations :k k
            :planar true :fallback true
            :crossings-before spring-cr :crossings-after tutte-cr}
          (let [base (* (min width height) 0.04)]
            (if (and (> smooth-iterations 0) (> (length edges) 0))
              (let [after (_smooth-crossings g edges spos width height
                                              smooth-iterations base 0.998)]
                @{:pos spos :eligible true :iterations iterations :k k
                  :planar planar? :fallback false
                  :crossings-before spring-cr :crossings-after after})
              @{:pos spos :eligible true :iterations iterations :k k
                :planar planar? :fallback false
                :crossings-before spring-cr
                :crossings-after (or (when tpos tutte-cr) spring-cr)})))))))
