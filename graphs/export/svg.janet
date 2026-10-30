# -*- mode: janet; -*-
# graphs/export/svg: SVG export via graphviz

(import deft :prefix "")
(import ../types :prefix "")
(import ../util :prefix "_u_")
(import ./dot :prefix "")
(import ./common :prefix "")
(import ../layout :prefix "")

(defn- tmp-dot-path
  "Construct a temporary .dot file in the system temp directory."
  []
  (def tmpdir (or (os/getenv "TMPDIR") "/tmp"))
  (string tmpdir "/snark-dot-" (string (os/time)) "-"
          (string (math/floor (* 1000000 (math/random)))) ".dot"))

(def graphviz-engines
  "Engine names handled natively by graphviz (vs. library layouts)."
  @{"dot" true "neato" true "fdp" true "sfdp" true
    "twopi" true "circo" true "osage" true "patchwork" true})


# ==============================================================================
#  Lombardi SVG renderer (circular-arc edges)
# ==============================================================================

(defn- _arc-flags
  ```SVG arc flags (large, sweep) for the path from `s` to `t` on the circle
centred at `c` with radius `r`, matching traversal sense `cw` (true when the
segment is traversed clockwise in layout coordinates y-up, SVG frame is y-down.
```
  [s t c r cw]
  (let [as (math/atan2 (- (s 1) (c 1)) (- (s 0) (c 0)))
        at (math/atan2 (- (t 1) (c 1)) (- (t 0) (c 0)))
        ccw ((fn [a b] (mod (- b a) (* 2 math/pi))) as at)
        span (if cw ccw (- (* 2 math/pi) ccw))]
    @[(if (> span math/pi) 1 0) (if cw 1 0)]))

(defn- _arc-sweep-points
  ```Extreme points of the arc segment that are not chord endpoints.```
  [c r s t seg]
  (let [ths (math/atan2 (- (s 1) (c 1)) (- (s 0) (c 0)))
        the (math/atan2 (- (t 1) (c 1)) (- (t 0) (c 0)))
        from (if (seg :cw) the ths)
        span (if (seg :cw) (mod (- ths the) (* 2 math/pi))
                           (mod (- the ths) (* 2 math/pi)))
        pts @[]]
    (each ph @[0.0 (/ math/pi 2.0) math/pi (* 1.5 math/pi)]
      (when (<= (mod (- ph from) (* 2 math/pi)) span)
        (array/push pts @[(+ (c 0) (* r (math/cos ph)))
                          (+ (c 1) (* r (math/sin ph)))])))
    pts))

(defn- _drawing-extent
  ```Return [minx miny maxx maxy] of the entire drawing in layout coordinates.

Computed from node positions (padded by node radius) and every edge's extent,
i.e. chord endpoints and the bulge extremes for circular arcs.
The svg canvas must be resized to these dimensions to avoid cliping.
```
  [pos arcs node-r]
  (var mnx math/inf) (var mny math/inf)
  (var mxx (* -1 math/inf)) (var mxy (* -1 math/inf))
  (defn incl [p r]
    (set mnx (min mnx (- (p 0) r))) (set mny (min mny (- (p 1) r)))
    (set mxx (max mxx (+ (p 0) r))) (set mxy (max mxy (+ (p 1) r))))
  (each p (values pos) (incl p node-r))
  (each a arcs
    (def s @[(a :x0) (a :y0)])
    (incl s 0)
    (if (nil? (a :segments))
      (incl @[(a :x1) (a :y1)] 0)
      (do
        (var cur s)
        (each seg (a :segments)
          (def nxt @[(seg :x) (seg :y)])
          (incl nxt 0)
          (unless (seg :line)
            (def c @[(seg :cx) (seg :cy)])
            (def r (seg :r))
            (each pt (_arc-sweep-points c r cur nxt seg) (incl pt 0)))
          (set cur nxt)))))
  (if (or (= mnx math/inf) (= mny math/inf)
          (= mxx (* -1 math/inf)) (= mxy (* -1 math/inf)))
    @[0.0 0.0 10.0 8.0]
    @[mnx mny mxx mxy]))

(defn- _degenerate-nodes
  ```Nodes sharing a position with another node, or nil.

A layout can fail visually when two nodes are placed extremely close.
Any renderer that positions nodes could draw them overlaping, so an explicit
check may be required for detecting nodes that share positions.

The graphviz render doesn't always reduce overlap, layout-spectral reports it
itself as :degeneracy. The check is primarily intended to refuse a position
that a renderer can't reasonably represent.
```
  [pos]
  (let [seen @{}
        hits @[]]
    (eachp [v p] pos
      (def key (string (math/round (* 1e9 (p 0))) "," (math/round (* 1e9 (p 1)))))
      (if (get seen key)
        (array/push hits v)
        (put seen key v)))
    (if (empty? hits) nil hits)))

(defn- _element-colour
  "The per-element colour for `key` in the map `m`, or nil."
  [m key]
  (when (and m (not (nil? (get m key))))
    (string (get m key))))

(defn- _escaped-colour
  "The colour `c` escaped for an attribute value, or nil when there is none."
  [c]
  (when c (_u_xml-escape c)))

(defn- kw-index
  "Index of keyword `k` in the flat keyword/value list `kvs`, or nil."
  [kvs k]
  (var i 0)
  (var found nil)
  (while (< i (length kvs))
    (when (= (kvs i) k) (set found i) (break))
    (set i (+ i 2)))
  found)

(defn- kw-get
  "The value of keyword `k` in the flat keyword/value list `kvs`, or nil."
  [kvs k]
  (def i (kw-index kvs k))
  (when i (get kvs (+ i 1))))

(defn- kw-set
  "Set keyword `k` in the keyword/value list `kvs`, adding it if it not present."
  [kvs k v]
  (def i (kw-index kvs k))
  (if i
    (put kvs (+ i 1) v)
    (do (array/push kvs k) (array/push kvs v)))
  kvs)

(defn- colour-into-opts
  ```Set `attr` to `colour` inside the table-valued option `opt` of `kvs`,
unless the attribute is already set.```
  [kvs opt attr colour]
  (when colour
    (def current (or (kw-get kvs opt) @{}))
    (unless (or (has-key? current attr) (has-key? current (string attr)))
      (kw-set kvs opt (merge current @{attr colour}))))
  kvs)

(defn canvas-error
  "Raise an error if the canvas can't be opened."
  [engine cap w h]
  (error (string "to-svg: layout '" engine "' produced a "
                 (string/format "%.0fx%.0f" w h)
                 "pt canvas, above the limit of "
                 (string/format "%.0f" cap) "pt. "
                 "Try a different layout (e.g. force, circle or radial) "
                 "or raise the size of :max-canvas.")))

(defn- _inside-label-scale
  "Font size, as a multiple of the node radius, to fit the longest label inside the disc."
  [names]
  (var longest 0)
  (each n names (set longest (max longest (length (string n)))))
  (if (<= longest 0)
    1.0
    (min 1.0 (/ 1.84 (* 0.62 longest)))))

(defn- _lombardi-svg
  "Render a Lombardi layout to an SVG string without using graphviz."
  [g layout opts]
  (def pos (get layout :pos))
  (def arcs (get layout :arcs @[]))
  # Refuse a layout with nodes that are not distinguishable.
  (def collided (_degenerate-nodes pos))
  (when (and collided (not (get opts :allow-coincident-nodes false)))
    (def blamed (get opts :base "an earlier layout"))
    (error (string "to-svg: the " blamed " placement put " (length collided)
                   " node(s) on top of another: "
                   (string/join
                    (map string (slice collided 0 (min 4 (length collided))))
                    ", ")
                   (if (> (length collided) 4) ", ..." "")
                   ". The embedding is degenerate, try :layout force, "
                   "circle or radial, a different :base, or pass "
                   ":allow-coincident-nodes true to draw it anyway.")))
  (def scale (get opts :scale 40.0))
  (def node-radius (get opts :node-radius 0.12))
  (def node-r-scale (* node-radius scale))
  (def label-nodes? (get opts :label-nodes false))
  (def label-inside? (= (get opts :label-position :below) :inside))
  (def label-scale (get opts :label-scale
                      (if label-inside? (_inside-label-scale (keys pos)) 2.0)))
  (def edge-width (get opts :edge-width))
  (def node-width (get opts :node-width))
  (def node-shape (get opts :node-shape "circle"))
  (def stroke (_u_xml-escape (or (get opts :edge-color) "black")))
  (def fill (_u_xml-escape (or (get opts :node-color) "white")))
  # Per-element colour overrides drawing-wide defaults
  (def node-colors (get opts :node-colors))
  (def edge-colors (get opts :edge-colors))
  (def arc-directed? (get opts :directed? (_u_directed? g)))
  (def ext (_drawing-extent pos arcs node-radius))
  (def mnx (ext 0)) (def mny (ext 1))
  (def mxx (ext 2)) (def mxy (ext 3))
  (def span-x (- mxx mnx)) (def span-y (- mxy mny))
  (def margin (* 0.03 (max span-x span-y)))
  (def pad (+ margin node-radius))
  (def w (+ span-x (* 2 pad)))
  (def h (+ span-y (* 2 pad)))
  # limit maximum drawing size
  (def cap (get opts :max-canvas 20000.0))
  (when (and (> cap 0) (or (> (* w scale) cap) (> (* h scale) cap)))
    (canvas-error
     (get opts :layout-name "lombardi") cap (* w scale) (* h scale)))
  (def px (fn [x] (* (+ (- x mnx) pad) scale)))
  (def py (fn [y] (* (- (+ mxy pad) y) scale)))
  (def lines @[])
  (each a arcs
    (def sx (px (a :x0))) (def sy (py (a :y0)))
    (def tx (px (a :x1))) (def ty (py (a :y1)))
    (def stroke (or (_escaped-colour
                       (_element-colour edge-colors
                                (edge-colour-key arc-directed? (a :u) (a :v))))
                     stroke))
    (if (= (a :kind) :line)
      (array/push lines
                  (string/format
                    "    <line x1=\"%.2f\" y1=\"%.2f\" x2=\"%.2f\" y2=\"%.2f\" stroke=\"%s\" fill=\"none\"/>"
                    sx sy tx ty stroke))
      (do
        (def parts @[(string/format "    <path d=\"M %.2f %.2f" sx sy)])
        (var curx sx) (var cury sy)
        (each seg (a :segments)
          (def gx (px (seg :x))) (def gy (py (seg :y)))
          (if (seg :line)
            (array/push parts (string/format " L %.2f %.2f" gx gy))
            (let [c @[(px (seg :cx)) (py (seg :cy))]
                  r (* (seg :r) scale)
                  flags (_arc-flags @[curx cury] @[gx gy] c r (seg :cw))]
              (array/push parts
                          (string/format " A %.2f %.2f 0 %d %d %.2f %.2f"
                                         r r (flags 0) (flags 1) gx gy))))
          (set curx gx) (set cury gy))
        (array/push parts (string "\" stroke=\"" stroke "\" fill=\"none\"/>"))
        (array/push lines (string/join parts "")))))
  (def nw (if node-width (string " stroke-width=\"" node-width "\"") ""))
  (def node-elts @[])
  (each v (_u_nodes g)
    (def p (get pos v))
    (when p
      (def cx (px (p 0)))
      (def cy (py (p 1)))
      (def r node-r-scale)
      (def fill (or (_escaped-colour (_element-colour node-colors v)) fill))
      (if (= node-shape "box")
        (array/push node-elts
                    (string/format
                      "    <rect x=\"%.2f\" y=\"%.2f\" width=\"%.2f\" height=\"%.2f\" fill=\"%s\" stroke=\"%s\"%s/>"
                      (- cx r) (- cy r) (* 2 r) (* 2 r) fill stroke nw))
        (array/push node-elts
                    (string/format
                      "    <circle cx=\"%.2f\" cy=\"%.2f\" r=\"%.2f\" fill=\"%s\" stroke=\"%s\"%s/>"
                      cx cy r fill stroke nw)))
      (when label-nodes?
        (array/push node-elts
                    (string/format
                      "    <text x=\"%.2f\" y=\"%.2f\" font-family=\"sans-serif\" font-size=\"%.2f\"%s text-anchor=\"middle\">%s</text>"
                      cx (if label-inside? cy (+ cy (* r 2.2)))
                      (* r label-scale)
                      (if label-inside? " dominant-baseline=\"central\"" "")
                      (_u_xml-escape v))))))
  (string "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n"
          "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\""
          (string/format "%.1f" (* w scale)) "\" height=\""
          (string/format "%.1f" (* h scale)) "\" viewBox=\"0 0 "
          (string/format "%.1f" (* w scale)) " "
          (string/format "%.1f" (* h scale)) "\">\n"
          "  <g stroke=\"" stroke "\""
          (if edge-width (string " stroke-width=\"" edge-width "\"") "")
          ">\n"
          (string/join lines "\n") "\n"
          "  </g>\n  <g>\n"
          (string/join node-elts "\n") "\n"
          "  </g>\n</svg>\n"))


# ==============================================================================
#  SVG export (via graphviz)
# ==============================================================================

(defn- typical-spacing
  ```Approximate mean distance from points in `pos` to its closest other point.

Uses sampling rather than an exhaustive comparison for efficiency. Mean is used
rather than the min or max to avoid one coincident pair determining the scale.

Returns nil for fewer than two points.
```
  [pos]
  (def pts @[])
  (eachp [node p] pos (array/push pts p))
  (def n (length pts))
  (if (< n 2)
    nil
    (do
      (def step (max 1 (math/floor (/ n 256.0))))
      (var total 0.0)
      (var count 0)
      (for i 0 n step
        (var best math/inf)
        (for j 0 n
          (when (not= i j)
            (def dx (- (get (get pts i) 0) (get (get pts j) 0)))
            (def dy (- (get (get pts i) 1) (get (get pts j) 1)))
            (def d (math/sqrt (+ (* dx dx) (* dy dy))))
            (when (< d best) (set best d))))
        (when (< best math/inf)
          (+= total best)
          (++ count)))
      (if (zero? count) nil (/ total count)))))


(defn capped-scale-warning
  "A warning message when :position-scale-max bound the derived scale."
  [cap]
  (string "to-svg: warning: the layout's own spacing needs a scale above "
          ":position-scale-max and is at the limit of " cap
          " and may not render well. Try :layout force, circle or radial, "
          "or raise :position-scale-max. "
          "Setting :position-scale-warn false will silence this warning."))

(defn auto-position-scale
  "Points per layout unit required for nodes drawn with `spacing` between them."
  [pos spacing floor cap]
  (def mnn (typical-spacing pos))
  (if (or (nil? mnn) (<= mnn 0))
    [floor false]
    (let [want (/ spacing mnn)]
      [(min cap (max floor want)) (> want cap)])))


(define to-svg
 ```Export a graph to an SVG file via Graphviz.

Generates a DOT string with to-dot, writes it to a file, runs the graphviz
layout engine selected by :layout, and writes the resulting SVG to :path.

:layout accepts any graphviz engine (dot, neato, fdp, sfdp, twopi, circo,
osage, patchwork, etc) or an internal layout registered with graph-layout
(e.g. tala, force, spring, circle, etc.). Internal layouts are computed
in-process, with positions pinned and rendered via dot's '-n2 no-layout' mode.

 &keys
   :path     — output .svg file path (default: "graph.svg")

   :dot-path — intermediate .dot file path (default: auto-generated
               temp file, removed after rendering)

   :layout   — graphviz engine or library layout (default: "dot").
               dot, neato, fdp, sfdp, twopi, circo, osage, patchwork,
               tala, spring, force, circle, lombardi, etc.

   :label-edges — include edge weights as labels (default true)

   :node-positions — table {node [x y]} in points, as the starting positions
                for the engine. Some engines can benefit from a starting layout,
                others (e.g. dot and circo) will ignore it. Internal layouts
                will use given positions, and the layout computes the rest.

   :fixed-positions — pin :node-positions exactly, instead of using them as
                starting configuration. Graphviz can only a pin only when every
                node has one and a partial table is refused.

   :node-color  — fill colour for every vertex.

   :edge-color  — stroke colour for every edge.

   accepts `to-dot` &keys; :graph-opts, :node-opts, :edge-opts, etc.

 Lombardi poly-arc renderer only

   :node-radius — node radius in layout units (default 0.12).

   :edge-width  — stroke width in points for every edge (default: unset,
                the renderer's own width).

   :node-width  — stroke width in points for a node outline (default: unset).

   :label-nodes — draw each node's name at each node (default false).

   :label-scale — label font size as a multiple of the node radius. Default
                inside the node: the largest size that keeps the longest
                label in the disc, never more than the radius; default below
                the node: 2.0.

   :label-position — where :label-nodes puts the text: :below (default) under
                the node, or :inside centred in it. A label inside needs a
                node radius large enough for the longest one: a
                three-character label fits a circle of radius r while the font
                stays under about r. It is what a compact drawing wants, since
                a label under the node collides with the arcs that leave it.

   :max-canvas   — refuse a rendered canvas larger than this, in points
                (default 20000, about 50cm; 0 disables).
                This check runs after the engine returns.

   :position-scale — points per layout unit for pinned library layouts.
                Defaults to a value derived from the layout's own spacing (see
                :node-spacing), floor at 40.

   :node-spacing — target distance in points between neighbouring nodes when
                deriving :position-scale (default 60)

   :position-scale-floor — lower bound for the derived scale (default 40)

   :position-scale-max — upper bound for the derived scale (default 1500).
                A layout whose embedding has collapsed cannot increase the
                canvas size beyond this limit.

 Plus all to-dot options (:name :rankdir :label-edges :node-opts :edge-opts
                          :node-colors :edge-colors :graph-opts)

 and, for library layouts, the layout's own &keys (:iterations :width
 :height ...).

Returns the SVG content as a string.
```
  [g :weighted & graph-keys] :string
  (let [opts (table ;graph-keys)
        out-path (get opts :path "graph.svg")
        engine (get opts :layout "dot")
        library-layout? (not (get graphviz-engines engine))]
    # compute library layouts in-process: positions are pinned via
    # to-dot's :node-positions key and drawn without re-layout (-n2)
    (def layout (when library-layout?
                  (graph-layout g engine ;graph-keys)))
    (def layout-pos (when layout (get layout :pos)))
    (when (and library-layout? (nil? layout-pos))
      (error (string "to-svg: layout '" engine "' ineligible: "
                     (get layout :reason "no reason"))))
    # Lombardi layouts use :arcs and are rendered internally
    (def l-arcs (when layout (get layout :arcs nil)))
    (if (and l-arcs (> (length l-arcs) 0))
      (do
        (spit out-path (_lombardi-svg g layout (merge opts {:layout-name engine})))
        (string (slurp out-path)))
      (do
    (def fwd (array ;graph-keys))
    # drawing-wide colour options
    (colour-into-opts fwd :node-opts :fillcolor (get opts :node-color))
    (colour-into-opts fwd :edge-opts :color (get opts :edge-color))
    (when layout-pos
      (def pos-scale
        (if (has-key? opts :position-scale)
          (get opts :position-scale)
          (let [[scale capped?] (auto-position-scale
                                  layout-pos
                                  (get opts :node-spacing 60.0)
                                  (get opts :position-scale-floor 40.0)
                                  (get opts :position-scale-max 1500.0))]
            (when (and capped? (get opts :position-scale-warn true))
              (eprintf (capped-scale-warning (get opts :position-scale-max 1500.0))))
            scale)))
      (def scaled @{})
      (eachp [node [x y]] layout-pos
        (put scaled node [(* x pos-scale) (* y pos-scale)]))
      (def given (get opts :node-positions))
      (when (table? given)
        (each k (keys given) (put scaled k (get given k))))
      (array/push fwd :node-positions)
      (array/push fwd scaled))
    # forward all keys to to-dot (except the :layout key)
    (def dot-src (to-dot g ;fwd))
    # write dot source to a file
    (def my-dot-path (get opts :dot-path))
    (def use-temp (not my-dot-path))
    (def dot-file (or my-dot-path (tmp-dot-path)))
    (spit dot-file dot-src)
    # run graphviz
    (def pins (get opts :node-positions))
    (def fixed
      (when (and (not layout-pos)
                 (not (= engine "tala"))
                 (get opts :fixed-positions false)
                 (table? pins)
                 (pos? (length pins)))
        (def nodes-of-g (_u_nodes g))
        (def missing (filter (fn [n] (not (has-key? pins n))) nodes-of-g))
        (def matched (length (filter (fn [k] (not (nil? (index-of k nodes-of-g))))
                                  (keys pins))))
        (def stray (- (length pins) matched))
        (when (pos? (length missing))
          (error (string "to-svg: :fixed-positions with a graphviz engine needs a "
                         "position for every node, and there is no layout to "
                         "supply the missing ones. :node-positions pins "
                         matched " of " (length nodes-of-g) " nodes"
                         (if (pos? stray)
                           (string " (and " stray " of its " (length pins)
                                   " entries name no node at all)")
                           "")
                         ". Unpinned: "
                         (string/join (map string (slice missing 0 (min 5 (length missing))))
                                      ", ")
                         (if (> (length missing) 5) ", ..." "")
                         ". Give the rest a position, name a library layout "
                         "such as :layout force to compute the rest, or drop "
                         ":fixed-positions to treat the positions as starts "
                         "rather than as pins.")))
        true))
    (def pinned? (or (= engine "tala") layout-pos fixed))
    (def rendering (if pinned? "neato" engine))
    (def args @["dot" "-K" rendering])
    (when pinned? (array/push args "-n2"))
    (array/push args "-Tsvg")
    (array/push args "-o")
    (array/push args out-path)
    (array/push args dot-file)
    (def err-file (file/open (string dot-file ".err") :w))
    (def rc (os/execute args :p {:err err-file}))
    (file/close err-file)
    (when (not= rc 0)
      # read stderr before cleaning up the err file
      (def stderr (try (slurp (string dot-file ".err")) ([_] "")))
      # cleanup
      (if use-temp
        (do (try (os/rm dot-file) ([_] nil))
            (try (os/rm (string dot-file ".err")) ([_] nil)))
        (do (try (os/rm (string dot-file ".err")) ([_] nil))))
      (error (string "graphviz " engine " failed (exit code " rc ")"
                     (if (not= stderr "") (string ": " stderr) ""))))
    # An engine can succeed and still produce an unusable drawing.
    (def svg (string (slurp out-path)))
    (def cap (get opts :max-canvas 20000.0))
    (when (> cap 0)
      (def head (string/slice svg 0 (min 2000 (length svg))))
      (def attr-pt
        (fn [name]
          (def at (string/find (string name "=\"") head))
          (when at
            (def tail (string/slice head (+ at (+ 2 (length name))) (min (length head) (+ at 60))))
            (def sp (or (string/find "pt" tail) (string/find "\"" tail) (length tail)))
            (let [r (scan-number (string/slice tail 0 sp))]
              r))))
      (def cw (or (attr-pt "width") 0.0))
      (def ch (or (attr-pt "height") 0.0))
      (when (or (> cw cap) (> ch cap))
        (canvas-error engine cap cw ch)))
    # cleanup temp (on success)
    (if use-temp
      (do (try (os/rm dot-file) ([_] nil))
          (try (os/rm (string dot-file ".err")) ([_] nil)))
      (do (try (os/rm (string dot-file ".err")) ([_] nil))))
      svg))))
