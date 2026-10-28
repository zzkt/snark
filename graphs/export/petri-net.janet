# -*- mode: janet; -*-
# graphs/export/petri-net: Petri-net exporter to Graphviz and TikZ

(import deft :prefix "")
(import ../petri-net :prefix "")
(import ./common :prefix "_ec_")

(deftfn- dot-id [s :dynamic] :string (_ec_q s))

(deftfn- place-degrees
  "Degree of each place, self arcs counted once per incidence."
  [pn :petri-net arcs :array] :table
  (let [deg @{}]
    (each [u v _] arcs
      (when (petri-net-place? pn u)
        (update deg u (fn [x] (+ (or x 0) 1))))
      (when (petri-net-place? pn v)
        (update deg v (fn [x] (+ (or x 0) 1)))))
    deg))

(deftfn- shared-places
  "Places of degree at least :thresh."
  [deg :table thresh :number] :table
  (let [shared @{}]
    (each [p d] (pairs deg)
      (when (>= d thresh) (put shared p true)))
    shared))

(deftfn- free-arcs
  ```Free arcs that must not constrain ranking, keyed "u v".```
  [pn :petri-net arcs :array shared :table] :table
  (let [free @{}]
    (each [u v _] arcs
      (when (or (get shared u) (get shared v))
        (put free (string u " " v) true)))
    (let [adj @{}]
      (each [u v _] arcs
        (unless (get free (string u " " v))
          (update adj u (fn [xs] (array/push (or xs @[]) v)))))
      (let [color @{}]
        (each s (pn :places)
          (when (and (get adj s) (not (get color s)))
            (put color s 1)
            (def stack @[[s 0]])
            (while (pos? (length stack))
              (def top (stack (- (length stack) 1)))
              (def u (top 0))
              (def succs (get adj u @[]))
              (def i (top 1))
              (if (< i (length succs))
                (do
                  (put stack (- (length stack) 1) @[u (inc i)])
                  (def v (get succs i))
                  (if (get color v)
                    (when (= (get color v) 1)
                      (put free (string u " " v) true))
                    (do
                      (put color v 1)
                      (array/push stack @[v 0]))))
                (do
                  (put color u 2)
                  (array/pop stack))))))))
    free))

(deftfn- rank-nodes
  "Longest-path ranking of the non-free arcs, treated as a DAG."
  [pn :petri-net arcs :array free :table] :table
  (let [rank @{}
        dag (seq [[u v _] :in arcs
                  :when (not (get free (string u " " v)))]
                 (tuple u v))]
    (var changed true)
    (while changed
      (set changed false)
      (each [u v] dag
        (let [candidate (inc (get rank u 0))]
          (when (> candidate (get rank v 0))
            (put rank v candidate)
            (set changed true)))))
    rank))

(deftfn- shared-placement
  "For each shared (degree >= 4) place, the node whose rank is closest to the median rank of its neighbours."
  [pn :petri-net arcs :array deg :table rank :table] :table
  (let [same @{}
        allnodes (array ;(pn :places))]
    (each t (pn :transitions) (array/push allnodes t))
    (each [p d] (pairs deg)
      (when (>= d 4)
        (def nbs (sort
                   (array ;(seq [[u v _] :in arcs
                                 :when (or (= u p) (= v p))]
                                (get rank (if (= u p) v u) 0)))))
        (when (pos? (length nbs))
          (def mid (/ (+ (nbs 0) (nbs (- (length nbs) 1))) 2))
          (var best nil)
          (var bestd nil)
          (each x allnodes
            (when (not= x p)
              (def dx (math/abs (- (get rank x 0) mid)))
              (when (or (nil? bestd) (< dx bestd))
                (set best x)
                (set bestd dx))))
          (when best (put same p best)))))
    same))

(deftfn- component-rows
  "Vertical row index per node.
Components disconnected from the shared places sit above (row +1) or
below (row -1) a shared place's row 0."
  [pn :petri-net arcs :array shared :table] :table
  (let [adj2 @{}]
    (each [u v _] arcs
      (unless (or (get shared u) (get shared v))
        (update adj2 u (fn [xs] (array/push (or xs @[]) v)))
        (update adj2 v (fn [xs] (array/push (or xs @[]) u)))))
    (let [_comp @{}
          allnodes (array ;(pn :places))]
      (each t (pn :transitions) (array/push allnodes t))
      (var next-comp 0)
      (each node allnodes
        (when (and (get adj2 node) (not (get _comp node)))
          (def stack @[node])
          (put _comp node next-comp)
          (while (pos? (length stack))
            (def u (array/pop stack))
            (each v (get adj2 u @[])
              (when (not (get _comp v))
                (put _comp v next-comp)
                (array/push stack v))))
          (++ next-comp)))
      (let [row @{}]
        (each p (pn :places)
          (put row p
            (if (get shared p) 0 (if (= (get _comp p 0) 0) 1 -1))))
        (each t (pn :transitions)
          (put row t (if (= (get _comp t 0) 0) 1 -1)))
        row))))

(deftfn- dot-layout-plan
  ```Layout annotations for dot and tikz.

- `:free` holds arcs that must not constrain ranking
- `:same` maps each such shared place to the node whose rank is closest to the
        median rank of its neighbours
- `:row` assigns a vertical row index to every node.
```
  [pn :petri-net] :table
  (let [arcs (petri-net-arcs pn)
        deg (place-degrees pn arcs)
        shared (shared-places deg 4)
        free (free-arcs pn arcs shared)
        rank (rank-nodes pn arcs free)
        same (shared-placement pn arcs deg rank)
        row (component-rows pn arcs shared)]
    @{:free free :shared shared :same same :rank rank :row row}))

(deftfn- cubic-clearance
  "Smallest margin between the cubic  p0..p3 and each obstacle disc [x y r].."
  [p0 :dynamic p1 :dynamic p2 :dynamic p3 :dynamic obs :dynamic cutoff :number] :number
  (var mn 1e308)
  (var done false)
  (loop [tt :in (range 0.1 0.95 0.1) :while (not done)]
    (def u (- 1 tt))
    (def b0 (* u u u))
    (def b1 (* 3 u u tt))
    (def b2 (* 3 u tt tt))
    (def b3 (* tt tt tt))
    (def x (+ (* b0 (p0 0)) (* b1 (p1 0)) (* b2 (p2 0)) (* b3 (p3 0))))
    (def y (+ (* b0 (p0 1)) (* b1 (p1 1)) (* b2 (p2 1)) (* b3 (p3 1))))
    (loop [o :in obs :while (not done)]
      (def d (math/hypot (- x (o 0)) (- y (o 1))))
      (set mn (min mn (- d (o 2))))
      (when (< mn cutoff) (set done true))))
  mn)

(deftfn- free-arc-controls
  "Control points for a smooth arc (ax,ay) -> (bx,by) chosen to maximise clearance."
  [ax :number ay :number bx :number by :number obs :array
   dist-ratio :number fallback-off :number] :array
  (def dx (- bx ax))
  (def dy (- by ay))
  (def L (math/hypot dx dy))
  (def phi (math/atan2 dy dx))
  (var best nil)
  (var best-clr -1e308)
  (each ang (range 0 51 5)
    (each sgn [1 -1]
      (def a (if (zero? ang) 0 (* sgn ang)))
      (def outa (+ phi a))
      (def ina (- phi a))
      (def ctrl (* L dist-ratio))
      (def c1 [(+ ax (* ctrl (math/cos outa))) (+ ay (* ctrl (math/sin outa)))])
      (def c2 [(- bx (* ctrl (math/cos ina))) (- by (* ctrl (math/sin ina)))])
      (def clr (cubic-clearance @[ax ay] c1 c2 @[bx by] obs best-clr))
      (when (> clr best-clr)
        (set best @[c1 c2])
        (set best-clr clr))))
  (if best
    best
    @[[(+ ax fallback-off) (+ ay fallback-off)]
      [(- bx fallback-off) (- by fallback-off)]]))

(deftfn- dot-node-boundary-pt
  "Point on the drawn edge of a node whose centre is at (cx,cy)."
  [nd :array ux :number uy :number] :array
  (def shape (get nd 3))
  (if (= shape :circle)
    @[(+ (nd 0) (* ux (get nd 5)))
      (+ (nd 1) (* uy (get nd 5)))]
    (if (and (zero? ux) (zero? uy))
      @[(nd 0) (nd 1)]
      (let [hw (get nd 4)
            hh (get nd 5)
            tx (if (zero? ux) 1e9 (/ hw (math/abs ux)))
            ty (if (zero? uy) 1e9 (/ hh (math/abs uy)))
            t (if (< tx ty) tx ty)]
        @[(+ (nd 0) (* ux t)) (+ (nd 1) (* uy t))]))))


(define petri-net-to-dot
  ```Render the Petri net as a Graphviz digraph.

- Places are white-filled circles (double if they currently hold tokens)
- Transitions are solid-filled boxes with white labels
- Arcs are labelled with their weight (omitted when 1) and an arrowhead.
- Layout follows convention
   - The main flow reads left to right
   - cycle-closing edges do not constrain ranking
   - shared interaction places (degree >= 4) are anchored to the ranking centre

Options: :name, :show-marking (default true), :rankdir (default "LR").
```
  [pn :petri-net & kws] :string
  (let [opts (table ;kws)
        name (get opts :name "petri")
        show-marking (get opts :show-marking true)
        rankdir (get opts :rankdir "LR")
        m (pn :marking)
        buf @"digraph "
        plan (dot-layout-plan pn)
        free (get plan :free)
        rank (get plan :rank)
        plan-row (get plan :row)
        same (get plan :same)
        shared (get plan :shared)
        pos-scale 56.7]
    (buffer/push-string buf (dot-id name))
    (buffer/push-string buf
      (string " {\n  rankdir=" rankdir
              ";\n  nodesep=\"0.6\";\n  ranksep=\"0.7\";\n"
              "  splines=true;esep=12;\n"
              "  node [fontname=\"Fira Sans\",margin=0];\n"))
    (each p (pn :places)
      (let [n (get m p 0)
            plab (if show-marking (string p " (" n ")") (string p))
            shape (if (> n 0) "doublecircle" "circle")
            prank (get rank (get same p p) 0)
            px (* prank pos-scale)
            py (* (get plan-row p 0) pos-scale)
            pos-str (string/format "pos=\"%.1f,%.1f!\"" px py)]
        (buffer/push-string buf
          (string "  " (dot-id p)
                  " [shape=" shape ",style=filled,fillcolor=white,"
                  "label=" (dot-id plab) ",fixedsize=true,"
                  "width=0.55,height=0.55,fontsize=11,"
                  pos-str "];\n"))))
    (each t (pn :transitions)
      (let [tx (* (get rank t 0) pos-scale)
            ty (* (get plan-row t 0) pos-scale)
            pos-str (string/format "pos=\"%.1f,%.1f!\"" tx ty)]
        (buffer/push-string buf
          (string "  " (dot-id t)
                  " [shape=box,style=filled,fillcolor=\"#1f2937\","
                  "fontcolor=white,label=" (dot-id t)
                  ",fixedsize=true,width=0.45,height=0.22,fontsize=9,"
                  pos-str "];\n"))))
    (each [p x] (pairs (get plan :same))
      (buffer/push-string buf
        (string "  { rank=same; " (dot-id p) "; " (dot-id x) "; }\n")))
    (def npos @{})
    (each p (pn :places)
      (put npos (string p)
        @[(* (get rank (get same p p) 0) pos-scale)
          (* (get plan-row p 0) pos-scale)
          19.8 :circle 19.8 (if (> (get m p 0) 0) 23.8 19.8)]))
    (each t (pn :transitions)
      (put npos (string t)
        @[(* (get rank t 0) pos-scale)
          (* (get plan-row t 0) pos-scale)
          11.0 :box 16.2 7.92]))
    (def obs @[])
    (each [_ d] (pairs npos)
      (array/push obs (array/slice d 0 3)))
    (def shared-arc? (fn [u v] (or (get shared u) (get shared v))))
    (each [u v w] (petri-net-arcs pn)
      (let [free? (get free (string u " " v))
            cycle-arc? (and free? (not (shared-arc? u v)))
            attrs @["arrowhead=vee"]]
        (when (not= w 1) (array/push attrs (string "label=" w)))
        (when free? (array/push attrs "constraint=false"))
        (when cycle-arc?
          (def da (get npos (string u)))
          (def db (get npos (string v)))
          (def ctl (free-arc-controls
                     (da 0) (da 1) (db 0) (db 1) obs 0.45 20))
          (def c1 (ctl 0))
          (def c2 (ctl 1))
          (def dir1 (math/hypot (- (c1 0) (da 0)) (- (c1 1) (da 1))))
          (def dir2 (math/hypot (- (c2 0) (db 0)) (- (c2 1) (db 1))))
          (def sa (if (pos? dir1)
                     (dot-node-boundary-pt da
                       (/ (- (c1 0) (da 0)) dir1)
                       (/ (- (c1 1) (da 1)) dir1))
                     (array/slice da 0 2)))
          (def sb (if (pos? dir2)
                     (dot-node-boundary-pt db
                       (/ (- (c2 0) (db 0)) dir2)
                       (/ (- (c2 1) (db 1)) dir2))
                     (array/slice db 0 2)))
          (array/push attrs
            (string "pos=\""
                    (string/format "%.1f,%.1f" (sa 0) (sa 1)) " "
                    (string/format "%.1f,%.1f" (c1 0) (c1 1)) " "
                    (string/format "%.1f,%.1f" (c2 0) (c2 1)) " "
                    (string/format "%.1f,%.1f" (sb 0) (sb 1))
                    "\"")))
        (buffer/push-string buf
          (string "  " (dot-id u) " -> " (dot-id v)
                  " [" (string/join attrs ",") "];\n"))))
    (buffer/push-string buf "}\n")
    (string buf)))


# ==============================================================================
#  TikZ export
# ==============================================================================

(deftfn- tikz-escape-label
  "Escape a label for safe embedding in LaTeX/TikZ text."
  [s :dynamic] :string
  (def raw (string s))
  (->> raw
      (string/replace-all "&" "\\&")
      (string/replace-all "%" "\\%")
      (string/replace-all "$" "\\$")
      (string/replace-all "#" "\\#")
      (string/replace-all "_" "\\_")
      (string/replace-all "{" "\\{")
      (string/replace-all "}" "\\}")
      (string/replace-all "~" "\\textasciitilde{}")
      (string/replace-all "^" "\\textasciicircum{}")))

(deftfn- tikz-seg-samples
  "Sample a straight segment as overlapping obstacle discs [x y r uv]."
  [ax :number ay :number bx :number by :number r :number] :array
  (def L (math/hypot (- bx ax) (- by ay)))
  (if (or (zero? L) (zero? r))
    @[[ax ay r]]
    (do
      (def out @[])
      (def n (max 2 (math/ceil (/ L (* 1.4 r)))))
      (each i (range 0 (inc n))
        (def t (/ i n))
        (array/push out (array (+ ax (* (- bx ax) t))
                               (+ ay (* (- by ay) t))
                               r)))
      out)))

(deftfn- tikz-cubic-samples
  "Sample a cubic bezier as overlapping obstacle discs [x y r uv]."
  [p0 :dynamic c1 :dynamic c2 :dynamic p3 :dynamic r :number] :array
  (def out @[])
  (each i (range 0 41)
    (def t (/ i 40))
    (def u (- 1 t))
    (def x (+ (* u u u (p0 0))
              (* 3 u u t (c1 0))
              (* 3 u t t (c2 0))
              (* t t t (p3 0))))
    (def y (+ (* u u u (p0 1))
              (* 3 u u t (c1 1))
              (* 3 u t t (c2 1))
              (* t t t (p3 1))))
    (array/push out (array x y r)))
  out)

(deftfn- tikz-graph-name
  "Sanitise a node label into a valid TikZ \\graph node name."
  [s :dynamic] :string
  (def buf @"")
  (each ch (string/bytes (string s))
    (def lc (if (<= 65 ch 90) (+ ch 32) ch))
    (when (or (<= 97 lc 122)
              (<= 48 lc 57)
              (= lc 95))
      (buffer/push-byte buf lc)))
  (if (empty? buf)
    (string "n" (string/replace-all " " "_" (string s)))
    (string buf)))

(define petri-net-to-tikz
  ```Render the net to a TikZ string.

- Places are white-filled circles (double if they currently hold tokens)
- Transitions are solid-filled boxes with white labels
- Arcs are labelled with their weight (omitted when 1) and an arrowhead.
- Layout follows convention
   - The main flow reads left to right
   - cycle-closing edges do not constrain ranking
   - shared interaction places (degree >= 4) are anchored to the ranking centre

Cycle-closing arcs have control points chosen to route around other
nodes and attempt to avoid arc crossing.

   LaTeX preamble required for the default output:
     \usepackage{tikz}
     \usetikzlibrary{petri, arrows.meta}

 &keys
   :show-marking — draw each place's tokens as dots (up to 4; larger counts
                   are shown as a numeric label) (default true)
   :direction — "LR" (default), "RL", "TB", or "BT"
   :sep — column step between ranks in cm (default 2)
   :row-sep — row step within a rank column in cm (default 1.3)
   :edge-style — extra TikZ style applied to all edges (default "")
   :tikz-opts — extra options for the tikzpicture environment (default "")
```
  [pn :petri-net & _keys] :string
  (let [opts (table ;_keys)
        show-marking (get opts :show-marking true)
        direction (get opts :direction "LR")
        sep (get opts :sep 2)
        row-sep (get opts :row-sep 1.3)
        edge-style (get opts :edge-style "")
        tikz-opts (get opts :tikz-opts "")
        m (pn :marking)
        plan (dot-layout-plan pn)
        free (get plan :free)
        shared (get plan :shared)
        rank (plan :rank)
        same (plan :same)
        vertical (or (= direction "TB") (= direction "BT"))
        bar (if vertical
              "minimum width=4mm, minimum height=2mm"
              "minimum width=2mm, minimum height=4mm")
        col @{}
        row @{}
        coord (fn [n]
          (def cx (get col n 0))
          (def cy (get row n 0))
          (case direction
            "RL" (tuple (* -1 cx sep) (* cy row-sep))
            "TB" (tuple (* cy row-sep) (* -1 cx sep))
            "BT" (tuple (* cy row-sep) (* cx sep))
            (tuple (* cx sep) (* cy row-sep))))
        fmt (fn [v] (string/format "%.1f" v))
        lines @[]
        buf @"\\begin{tikzpicture}["]
    (each p (pn :places)
      (put col p (get rank (if (get same p) (get same p) p) 0)))
    (each t (pn :transitions)
      (put col t (get rank t 0)))
    (def plan-row (get plan :row))
    (each p (pn :places)
      (put row p (get plan-row p 0)))
    (each t (pn :transitions)
      (put row t (get plan-row t 0)))
    (each p (pn :places)
      (let [n (get m p 0)
            nm (tikz-graph-name p)
            pt (coord p)
            _opts @["place"
                   (string "label=below:{" (tikz-escape-label p) "}")]]
        (when (and show-marking (<= 1 n 4))
          (array/push _opts (string "tokens=" n)))
        (when (and show-marking (> n 4))
          (array/push _opts (string "label=45:{" (tikz-escape-label n) "}")))
        (array/push lines
          (string "  \\node (" nm ") [" (string/join _opts ", ") "] at ("
                  (fmt (pt 0)) ", " (fmt (pt 1)) ") {};"))))
    (each t (pn :transitions)
      (let [nm (tikz-graph-name t)
            pt (coord t)]
        (array/push lines
          (string "  \\node (" nm ") [transition, label=above:{"
                  (tikz-escape-label t) "}] at ("
                  (fmt (pt 0)) ", " (fmt (pt 1)) ") {};"))))
    (def obstacles @[])
    (each p (pn :places)
      (let [pt (coord p)]
        (array/push obstacles [(pt 0) (pt 1) 0.35])
        (array/push obstacles [(pt 0) (- (pt 1) 0.44) 0.26])))
    (each t (pn :transitions)
      (let [pt (coord t)]
        (array/push obstacles [(pt 0) (pt 1) 0.32])
        (array/push obstacles [(pt 0) (+ (pt 1) 0.44) 0.26])))
    (def arcs (petri-net-arcs pn))
    (def shared-arc? (fn [u v] (or (get shared u) (get shared v))))
    (def cycle-arc? (fn [u v] (and (get free (string u " " v))
                                   (not (shared-arc? u v)))))
    (def straight-obs @[])
    (each [u v _] arcs
      (when (not (cycle-arc? u v))
        (def up (coord u))
        (def vp (coord v))
        (each s (tikz-seg-samples (up 0) (up 1) (vp 0) (vp 1) 0.12)
          (array/push straight-obs (array (s 0) (s 1) (s 2) u v)))))
    (def placed-obs @[])
    (each [u v w] arcs
      (let [eopts @["post"]
            up (coord u)
            vp (coord v)
            shares? (fn [o] (or (= (o 3) u) (= (o 4) u)
                                (= (o 3) v) (= (o 4) v)))
            cycle? (cycle-arc? u v)
            path (if cycle?
                   (do
                     (def obs @[])
                     (each o obstacles (array/push obs o))
                     (each o straight-obs (when (not (shares? o)) (array/push obs o)))
                     (each o placed-obs (when (not (shares? o)) (array/push obs o)))
                     (def cs (free-arc-controls (up 0) (up 1) (vp 0) (vp 1) obs 0.5 0.5))
                     (def c1 (cs 0))
                     (def c2 (cs 1))
                     (each s (tikz-cubic-samples @[(up 0) (up 1)] c1 c2 @[(vp 0) (vp 1)] 0.12)
                       (array/push placed-obs (array (s 0) (s 1) (s 2) u v)))
                     (string ".. controls (" (fmt (c1 0)) ", " (fmt (c1 1))
                             ") and (" (fmt (c2 0)) ", " (fmt (c2 1)) ") .."))
                   "--")]
        (when (not= edge-style "") (array/push eopts edge-style))
        (when cycle? (array/push eopts "shorten <=3.2mm, shorten >=3.2mm"))
        (array/push lines
          (string "  \\draw[" (string/join eopts ", ") "] ("
                  (tikz-graph-name u) ") " path " "
                  (if (not= w 1)
                    (string "node[auto, font=\\scriptsize] {" (tikz-escape-label (string w)) "} ")
                    "")
                  "(" (tikz-graph-name v) ");"))))
    (buffer/push-string buf
      "thick, place/.style={circle, draw=black!50, fill=white, "
      "inner sep=0pt, minimum size=6mm}, transition/.style={rectangle, "
      "draw=black!50, fill=black, inner sep=0pt, " bar
      "}, every token/.style={fill=black}, "
      "every label/.style={black!60, font=\\scriptsize}, "
      "post/.style={->, shorten >=1pt, >={Stealth}}")
    (when (not= tikz-opts "") (buffer/push-string buf (string ", " tikz-opts)))
    (buffer/push-string buf "]\n")
    (buffer/push-string buf (string/join lines "\n"))
    (buffer/push-string buf "\n\\end{tikzpicture}\n")
    (string buf)))


# ==============================================================================
#  Reachability graph
# ==============================================================================

(deftfn- format-marking-compact
  "Compact string representation of a marking. The names of places holding
at least one token, sorted and comma-separated. Empty marking -> \"{}\"."
  [pn :petri-net m :table] :string
  (def marked
    (sort (seq [p :in (pn :places)
                :when (> (get m p 0) 0)]
               (string p))))
  (if (empty? marked) "{}" (string "(" (string/join marked ", ") ")")))

(deftfn- marking-key-for-export
  [pn :petri-net m :table] :tuple
  (tuple ;(map (fn [p] (get m p 0)) (pn :places))))

(deftfn- marking-dead?
  [pn :petri-net m :table] :boolean
  (not (some (fn [t] (petri-net-enabled? pn t m)) (pn :transitions))))

(define petri-net-state-space-to-dot
  "Render the reachability graph of pn as a Graphviz digraph."
  [pn :petri-net & kws] :string
  (let [opts (table ;kws)
        name (get opts :name "rg")
        limit (get opts :limit 10000)
        rankdir (get opts :rankdir "TB")
        res (petri-net-state-space pn :limit limit)
        states (res :states)
        edges (res :edges)
        truncated (res :truncated)
        last-index (- (length states) 1)
        key->idx @{}
        buf @"digraph "]
    (each i (range (length states))
      (put key->idx (marking-key-for-export pn (get (states i) :marking)) i))
    (buffer/push-string buf (dot-id name))
    (buffer/push-string buf
      (string " {\n  rankdir=" rankdir
              ";\n  nodesep=\"0.8\";\n  ranksep=\"1.0\";\n"
              "  node [fontname=\"Fira Sans\",shape=box,style=filled];\n"))
    (each i (range (length states))
      (def m (get (states i) :marking))
      (def lab (format-marking-compact pn m))
      (def is-init (= i 0))
      (def is-dead (and (not truncated) (marking-dead? pn m)))
      (def fill (cond is-init "#a8e6cf" is-dead "#ffcdd2" true "#f5f5f5"))
      (def fc (if is-dead "#b71c1c" "#212121"))
      (buffer/push-string buf
        (string "  " (dot-id (string i))
                " [label=" (dot-id lab)
                ",fillcolor=" (dot-id fill) ",fontcolor=" (dot-id fc)
                (when (and truncated (= i last-index))
                  ",color=\"#f9a825\",penwidth=2")
                "];\n")))
    (each [from to t] edges
      (buffer/push-string buf
        (string "  " (dot-id (string (get key->idx from)))
                " -> " (dot-id (string (get key->idx to)))
                " [label=" (dot-id (string t))
                ",fontname=\"Fira Sans\",fontsize=10];\n")))
    (when truncated
      (buffer/push-string buf
        (string "  " (dot-id "truncated")
                " [shape=plaintext,fontcolor=\"#f9a825\",label="
                (dot-id (string "TRUNCATED (limit " limit ")")) "];\n")))
    (buffer/push-string buf "}\n")
    (string buf)))
