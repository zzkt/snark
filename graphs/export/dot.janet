# -*- mode: janet; -*-
# graphs/export/dot: Graphviz DOT export

(import deft :prefix "")
(import ../record :prefix "rec-")
(import ../types :prefix "")
(import ../util :prefix "")
(import ../layout :prefix "_tala_")
(import ./common :prefix "")

# ==============================================================================
#  Graphviz export
# ==============================================================================

(defn- kw->str
  "Convert all keyword keys in a table to string keys. Returns the same table."
  [t]
  (each k (keys t)
    (when (keyword? k)
      (let [v (get t k)]
        (put t k nil)
        (put t (string k) v))))
  t)

(defn- dot-edge-op
  "Return '->' for directed, '--' for undirected."
  [directed?]
  (if directed? "->" "--"))

(defn- dot-header
  [directed?]
  (if directed? "digraph" "graph"))

(defn- dot-attr-value
  ```Quote a DOT attribute value `v` only when necessary.
Bare IDs such as true/false/numbers/colors stay unquoted, values containing
spaces or special characters are quoted.```
  [v]
  (def s (string v))
  (def safe
    (and (> (length s) 0)
         (peg/match
               ~(* (any (choice (range "az")
                                (range "AZ")
                                (range "09")
                                (set "_.")))
                   -1)
           s)))
  (if safe s (q s)))

(defn- dot-attrs-str
  "Join a table of DOT attribute key-value pairs into a bracketed string."
  [attrs]
  (when (pos? (length attrs))
    (string " ["
      (string/join
        (seq [[k v] :pairs attrs] (string k "=" (q v)))
        ",")
      "]")))

(defn- dot-edge-parts
  ```Attribute table for an edge with weight `w` on a `directed?` graph.```
  [w label-edges directed?]
  (let [parts @{}
        wn (if (number? w) w 1.0)]
    (put parts "fontname" "Fira Sans")
    (when label-edges
      (put parts "label" (string w)))
    (put parts "weight" wn)
    (def pw (math/floor (+ 0.5 (math/abs wn))))
    (put parts "penwidth" (string (max 0.5 (min pw 4))))
    (if directed?
      (put parts "color" (if (< wn 0) "#E74C3C" "#27AE60"))
      (do (put parts "color" "#555555") (put parts "fontcolor" "#555555")))
    parts))

(defn- dot-edge-attrs
 ```Return DOT edge attribute string for weight `w` on a `directed?` graph.
Merges weight-derived attributes with user-edge-opts.```
  [w label-edges directed? user-edge-opts]
  (def parts (dot-edge-parts w label-edges directed?))
  (merge-into parts user-edge-opts)
  (dot-attrs-str parts))

(defn- flip-y
  "DOT positions use an upward y axis."
  [p]
  (string (p 0) "," (- 0 (p 1))))

(defn- dot-node-attrs-str
  ```Return the bracketed attribute string for a node, or nil when it has none.
`pos` is the pinned position or nil, `colour` the per-node colour or nil.```
  [pos colour]
  (var s "")
  (when pos (set s (string "pos=\"" (flip-y pos) "!\"")))
  (when colour
    (set s (string s (if (= s "") "" ",") "fillcolor=" (q colour))))
  (if (= s "") nil (string " [" s "]")))

(defn- edge-pos-str
  ```DOT ortho edge position from a staircase [(sx sy) ... (ex ey)].

Returns 'e,<end> <points...>', the arrowhead first, then the remaining waypoints
in reverse order back toward the source.```
  [pts]
  (def buf @"e,")
  (buffer/push-string buf (flip-y (pts (- (length pts) 1))))
  (var i (- (length pts) 2))
  (while (>= i 0)
    (buffer/push-string buf (string " " (flip-y (pts i))))
    (-- i))
  (string buf))

(define to-dot
 ```Export a graph to a DOT format string.

 &keys
   :name        — graph name (default "G")
   :rankdir     — layout direction: LR, TB, BT, RL (default "LR")
   :layout      — graphviz layout engine (default: "dot").
                  dot, neato, fdp, sfdp, twopi, circo, osage, patchwork.

   :label-edges — include edge weights as labels (default false)

   :node-positions — table {node [x y]} pinning node positions
                     can be used with graphviz -n2 for library layouts

   :node-opts   — table of DOT node attributes applied to all nodes.
                  e.g. @{:style "filled" :fillcolor "#FFEEDD"}

   :edge-opts   — table of DOT edge attributes merged with weight-derived attrs
                  e.g. @{:color "#333" :penwidth "2" :arrowsize "0.8"}

   :node-colors  — table {node colour} for colouring of individual vertices
                   keyed by node label

   :edge-colors  — table {edge colour} for colouring of individual edges
                   keyed by a (tuple u v) pair

   :graph-opts  — table of DOT graph attributes
                   e.g. @{:splines "true" :label "A Graph"}

   :directed?   — force output as a `digraph` with `->` or `graph` with `--`.
                  Directedness is auto-detected from the graph by default.

 Returns a string suitable for writing to a .dot file.
```
  [g :weighted & graph-keys] :string
  (def opts (table ;graph-keys))
  (def graph-name (get opts :name "G"))
  (def rankdir (get opts :rankdir "LR"))
  (def label-edges (get opts :label-edges false))
  (def node-opts (kw->str (get opts :node-opts @{})))
  (def edge-opts (kw->str (get opts :edge-opts @{})))
  (def graph-opts (kw->str (get opts :graph-opts @{})))
  # Per-element colour maps
  (def node-colors (get opts :node-colors))
  (def edge-colors (get opts :edge-colors))
  (def directed? (get opts :directed? (structurally-directed? g)))
  (def arrow (dot-edge-op directed?))
  (def seen-nodes (table))
  (def buf @"")
  (buffer/push-string buf
                      (string (dot-header directed?) " " graph-name " {\n"))
  # default graph-level attrs
  (def default-graph-attrs (table))
  (put default-graph-attrs "splines" "true")
  (put default-graph-attrs "rankdir" rankdir)
  (put default-graph-attrs "nodesep" "0.5")
  (put default-graph-attrs "ranksep" "1.0")
  (put default-graph-attrs "pad" "0.5")
  (put default-graph-attrs "fontname" "Fira Sans")
  (merge-into default-graph-attrs graph-opts)
  # Use precomputed node positions from a layout function
  (def node-positions (get opts :node-positions @{}))
  # optional TALA hierarchical layout
  (def tala-direction
    (when (= (get opts :layout) "tala")
      (case rankdir "TB" "tb" "BT" "bt" "LR" "lr" "RL" "rl" "lr")))
  (def tala (when tala-direction
              (_tala_layout-dag g :direction tala-direction)))
  (def tala-eligible (and tala (tala :eligible)))
  (when (zero? (length node-positions))
    (when tala-eligible
      (put default-graph-attrs "splines" "ortho")))
  (loop [[k v] :pairs default-graph-attrs]
    (buffer/push-string buf (string "  " k "=" (dot-attr-value v) ";\n")))
  # default node-level attributes
  (def default-node-attrs (table))
  (put default-node-attrs "shape" "ellipse")
  (put default-node-attrs "style" "filled")
  (put default-node-attrs "fillcolor" "#FFF3CD")
  (put default-node-attrs "penwidth" "1.5")
  (put default-node-attrs "fontname" "Fira Sans")
  (merge-into default-node-attrs node-opts)
  (buffer/push-string buf "  node")
  (buffer/push-string buf (dot-attrs-str default-node-attrs))
  (buffer/push-string buf ";\n")
  # emit nodes
  (each u (rec-nodes g)
    (unless (get seen-nodes u)
      (put seen-nodes u true)
      (var p (get node-positions u))
      (when (nil? p) (set p (when tala-eligible (get (tala :pos) u))))
      (def colour (when node-colors (get node-colors u)))
      (buffer/push-string
        buf
        (string "  " (q u)
               (or (dot-node-attrs-str p (when colour (string colour))) "")
               ";\n"))))
  # emit edges
  (each-edge g directed? (fn [u v w]
    (def pts (when tala-eligible
               (or (get (tala :control-points) (tuple u v))
                   (get (tala :control-points) (tuple v u)))))
    (def colour (when edge-colors
                  (get edge-colors (edge-colour-key directed? u v))))
    (def attrs
      (if (nil? colour)
        (if pts
          (do
            (def parts (dot-edge-parts w label-edges directed?))
            (put parts "pos" (edge-pos-str pts))
            (merge-into parts edge-opts)
            (dot-attrs-str parts))
          (dot-edge-attrs w label-edges directed? edge-opts))
        # a specific edge colour overrides drawing-wide :edge-opts
        (do
          (def parts (dot-edge-parts w label-edges directed?))
          (when pts (put parts "pos" (edge-pos-str pts)))
          (merge-into parts edge-opts)
          (put parts "color" (string colour))
          (dot-attrs-str parts))))
    (buffer/push-string buf
                        (string "  " (q u) " " arrow " " (q v) attrs "\n"))))
  (buffer/push-string buf "}\n")
  (string buf))
