# -*- mode: janet; -*-
# graphs/export/tikz: TikZ/LaTeX export

(import deft :prefix "")
(import ../record :prefix "rec-")
(import ../types :prefix "")
(import ../util :prefix "")
(import ../layout :prefix "_tala_")
(import ../layout :prefix "_gd_")
(import ./common :prefix "")


# ==============================================================================
#  TikZ / LaTeX export
# ==============================================================================

(defn- tikz-escape
  "Escape a node label for safe embedding in LaTeX/TikZ text."
  [s]
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

(defn- tikz-sanitise
  "Sanitise a label to a valid TikZ node name (alphanumeric)."
  [s]
  (def buf @"")
  # a-z => 97-122, A-Z => 65-90, 0-9 => 48-57, '_' => 95
  (each ch (string/bytes (string s))
    (def lc (if (<= 65 ch 90) (+ ch 32) ch)) # downcase
    (when (or (<= 97 lc 122)
              (<= 48 lc 57)
              (= lc 95))
      (buffer/push-byte buf lc)))
  (string buf))

(defn- tikz-node-name
  "A TikZ node name for `s`, unique within the drawing."
  [s idx used]
  (def base (tikz-sanitise s))
  (cond
    (= base "") (string "n" idx)
    (not (get used base)) base
    true (string "n" idx base)))

(defn- tikz-chain-pos
  "Return [x y] for node index `idx` in a chain with `n` nodes.
   direction LR: horizontal row; TB: vertical column."
  [idx n direction]
  (def spacing 1.5)
  (case direction
    "TB" [0 (* (- 0 idx) spacing)]
    "BT" [0 (* (- idx (- n 1)) spacing)]
    "RL" [(* (- 0 idx) spacing) 0]
    [(* idx spacing) 0]))

(defn- tikz-chains
  ```Decompose the edges of `g` into simple paths for compact TikZ \graph output.```
  [g directed?]
  (def adj (out-adjacency g))
  (defn ekey [u v]
    (if directed? (tuple u v) (tuple (min u v) (max u v))))
  (def pending (table))
  (each-edge g directed? (fn [u v w]
    (put pending (ekey u v) @[u v w])))
  (var left 0)
  (each [_ e] (pairs pending) (when e (++ left)))
  # reverse adjacency for directed predecessor lookup
  (def rev
    (if directed?
      (do
        (def rt (table))
        (each u (rec-nodes g) (put rt u (table)))
        (loop [[u tbl] :pairs adj]
          (loop [[v _] :pairs tbl]
            (when (not= u v) (put-in rt [v u] true))))
        rt)
      nil))
  (defn succs
    "Sorted successor candidates of `cur` with pending edges [v w]."
    [cur]
    (def out @[])
    (loop [[v _] :pairs (get adj cur)]
      (def e (get pending (ekey cur v)))
      (when e (array/push out @[v (e 2)])))
    (sort out (fn [a b] (< (a 0) (b 0)))))
  (defn preds
    "Sorted predecessor candidates of `cur` with pending edges [v w]."
    [cur]
    (def out @[])
    (loop [[v _] :pairs (get (if directed? rev adj) cur)]
      (def e (get pending (ekey v cur)))
      (when e (array/push out @[v (e 2)])))
    (sort out (fn [a b] (< (a 0) (b 0)))))
  (defn consume
    "Remove and return the pending edge (u v)."
    [u v]
    (def e (get pending (ekey u v)))
    (put pending (ekey u v) nil)
    (-- left)
    e)
  (def chains @[])
  (while (pos? left)
    # seed the chain with lexicographically smallest pending edge
    (var su nil)
    (var sv nil)
    (loop [[k _] :pairs pending]
      (when (get pending k)
        (when (or (nil? su)
                  (< (k 0) su)
                  (and (= (k 0) su) (< (k 1) sv)))
          (set su (k 0))
          (set sv (k 1)))))
    (def seed (consume su sv))
    # extend forward from the seed's target
    (def fwd @[seed])
    (var cur (seed 1))
    (while true
      (def cands (succs cur))
      (when (empty? cands) (break))
      (def [nxt nw] (cands 0))
      (consume cur nxt)
      (array/push fwd @[cur nxt nw])
      (set cur nxt))
    # extend backward from the seed's source
    (def bwd @[])
    (var bcur (seed 0))
    (while true
      (def pcands (preds bcur))
      (when (empty? pcands) (break))
      (def [prd pw] (pcands 0))
      (consume prd bcur)
      (array/push bwd @[prd bcur pw])
      (set bcur prd))
    (def chain @[])
    (each s (reverse bwd) (array/push chain s))
    (each s fwd (array/push chain s))
    (array/push chains chain))
  # nodes with no incident edge
  (def used (table))
  (each chain chains
    (each s chain
      (put used (s 0) true)
      (put used (s 1) true)))
  (def isolated @[])
  (each u (rec-nodes g)
    (unless (get used u) (array/push isolated u)))
  {:chains chains :isolated isolated})

(defn- tikz-node-spec
 ```A \graph node specification.```
  [u name-map]
  (def info (get name-map u))
  (if (= (info :name) (info :label))
    (string (info :name))
    (string (info :name) "[as={" (info :label) "}]")))

(defn- tikz-edge-part
 ```A \graph edge connector for a step.
i.e. '->' or '--' with optional [style, "weight"] options for that edge.```
  [directed? label-edges w edge-style]
  (let [connector (if directed? "->" "--")
        opts @[]]
    (when (not= edge-style "") (array/push opts edge-style))
    (when (and label-edges (number? w))
      # weight labels are annotations
      (array/push opts
        (string "\"" (tikz-escape (string w)) "\"{fill=none, draw=none, "
                "text=black!40, font=\\scriptsize}")))
    (if (pos? (length opts))
      (string connector "[" (string/join opts ", ") "]")
      connector)))


(defn- tala-pos-scale
 "Remap TALA units, preserving aspect ratio.
Returns nil (unchanged) if the box is empty."
  [pos]
  (when (pos? (length pos))
    (var min-x 1e18)
    (var max-x -1e18)
    (var min-y 1e18)
    (var max-y -1e18)
    (each [u p] (pairs pos)
      (set min-x (min min-x (p 0)))
      (set max-x (max max-x (p 0)))
      (set min-y (min min-y (p 1)))
      (set max-y (max max-y (p 1))))
    (def span (max (- max-x min-x) (- max-y min-y) 1.0))
    (/ 9.0 span)))

(defn- tkz-ex
  "Format one coordinate for TikZ node/edge output (%.4f, TALA-scaled)."
  [v scale]
  (string/format "%.4f" (if scale (* scale v) v)))


(define to-tikz
  ```Export a graph to a TikZ picture string.

By default a picture uses the compact `\graph` path syntax from the TikZ `graphs`
library (e.g. `a ->["1"] b`) which lets TikZ place the nodes itself.

Pass `:layout` to emit explicit `\node` and `\draw` commands with coordinates.

  LaTeX preamble required for the default output
    \usepackage{tikz}
    \usetikzlibrary{graphs}
    \usetikzlibrary{quotes}     % needed if edge labels are shown

Directed edges use the built-in `>=` latex arrow tip, no arrows library needed.

 &keys
   :directed? — force directed edge arrows (default: auto-detected)

   :label-edges — include numeric edge weights as labels (default true)

   :node-style — TikZ node style applied to all nodes (default
                 "circle, draw=black!60, fill=blue!10, line width=0.5pt,
                  minimum size=2.0em, inner sep=1pt")

   :direction — "LR" (default), "TB", "BT", or "RL".

   :sep — \graph chain separation, e.g. "1.5cm" (default)

   :graph-opts — extra \graph options (default "")

   :layout — layout mode for explicit \node + \draw commands:
        "tala"        — deterministic hierarchical layout (requires DAG)
        "force"       — Fruchterman-Reingold force-directed (any graph)
        "circle"      — nodes on a circle (good for cyclic graphs)
        "tree"        — rooted BFS tree layout (good for trees/forests)
        "radial"      — concentric circles from root (trees, hub-and-spoke)
        "orthogonal"  — grid-based layout with -| edge routing
        "spectral"    — eigenvectors of the graph Laplacian (Puppe 2008)

        "phylogenetic" — rooted rectangular phylogram (see: gd-phylogenetics)
                         branch lengths as horizontal distance, leaves
                         stacked in rows (rooted weighted trees)

        "phylogram" — rooted rectangular phylogram. same as "phylogenetic"
                      with branch lengths proportional to edge weights

        "phylogenetic-tree" — rooted phylogram with each leaf flushed to
                      the deepest tip column (classic aligned cladogram)

        "unrooted-phylogram" — phylogram split about the tree centre,
                               edge weights as branch lengths.

        "unrooted-phylogenetic-tree" — unrooted phylogram with aligned tips

   :root — root node for the tree/radial/orthogonal/phylogenetic layouts
           (default: heuristic). For the unrooted-* modes it picks the hub node

   :edge-style — extra TikZ style applied to all edges (default "")

   :tikz-opts — extra options for the tikzpicture environment (default "")


Each graph's edges are decomposed into maximal chain paths so paths and trees
should read naturally. Node labels are sanitised into valid unique TikZ names.
A drawing should never merge two distinct nodes.

Returns a string suitable for writing to a .tex file or embedding in a document.
```
  [g :weighted & keys] :string
  (def opts (table ;keys))
  (def directed? (get opts :directed? (structurally-directed? g)))
  (def label-edges (get opts :label-edges true))
  (def node-style (get opts :node-style
    "circle, draw=black!60, fill=blue!10, line width=0.5pt, minimum size=2.0em, inner sep=1pt"))
  (def direction (get opts :direction "LR"))
  (def layout-mode (get opts :layout))
  (def edge-style (get opts :edge-style ""))
  (def tikz-opts (get opts :tikz-opts ""))
  (def sep (get opts :sep "1.5cm"))
  (def graph-opts (get opts :graph-opts ""))
  # layout: tala, force, circle, tree, radial, orthogonal, spectral, or nil (\graph mode)
  (def tala-mode (= layout-mode "tala"))
  (def gd-modes @{ "force" true "circle" true "tree" true
                  "radial" true "orthogonal" true "spectral" true
                  "phylogenetic" true "phylogram" true
                  "phylogenetic-tree" true "unrooted-phylogram" true
                  "unrooted-phylogenetic-tree" true })
  (def use-gd (and layout-mode (get gd-modes layout-mode)))
  (def tala
    (when tala-mode
      (do
        (def tala-dir
          (case direction "TB" "tb" "BT" "bt" "LR" "lr" "RL" "rl" "lr"))
        (def tl (_tala_layout-dag g :direction tala-dir))
        (and tl (tl :eligible) tl))))
  (def gd-layout
    (when use-gd
      (def root-opt (get opts :root))
      # phylogram family: forward :root plus per-direction growth
      (defn- phylo-layout [f]
        (def pkeys @[])
        (when root-opt (array/push pkeys :root) (array/push pkeys root-opt))
        (case direction
          "TB" (array/push pkeys :vertical true)
          "BT" (do (array/push pkeys :vertical true) (array/push pkeys :mirror true))
          "RL" (array/push pkeys :mirror true))
        (when (get opts :hspacing)
          (array/push pkeys :hspacing) (array/push pkeys (get opts :hspacing)))
        (when (get opts :vspacing)
          (array/push pkeys :vspacing) (array/push pkeys (get opts :vspacing)))
        (f g ;pkeys))
      (case layout-mode
        "force" (_gd_layout-force g)
        "circle" (_gd_layout-circle g)
        "tree" (if root-opt
                 (_gd_layout-tree g :root root-opt)
                   (_gd_layout-tree g))
        "radial" (if root-opt
                   (_gd_layout-radial g :root root-opt)
                   (_gd_layout-radial g))
        "orthogonal" (if root-opt
                       (_gd_layout-orthogonal g :root root-opt)
                       (_gd_layout-orthogonal g))
        "spectral" (_gd_layout-spectral g)
        "phylogenetic" (phylo-layout _gd_layout-phylogenetic)
        "phylogram" (phylo-layout _gd_layout-phylogram)
        "phylogenetic-tree" (phylo-layout _gd_layout-phylogenetic-tree)
        "unrooted-phylogram" (phylo-layout _gd_layout-unrooted-phylogram)
        "unrooted-phylogenetic-tree" (phylo-layout _gd_layout-unrooted-phylogenetic-tree))))
  (def use-layout (or tala gd-layout))
  (def explicit-mode (or tala-mode use-gd))
  # phylogram layouts return their own child-first draw order in :edges
  (def layout-edges (when gd-layout (get gd-layout :edges)))
  (def phylo-parents @{})
  (when layout-edges (each uv layout-edges (put phylo-parents (uv 0) true)))
  (def phylo-parent @{})
  (when layout-edges (each uv layout-edges (put phylo-parent (uv 1) (uv 0))))
  (def phylo-style
    (and use-gd
         (truthy? layout-edges)
         (pos? (length layout-edges))
         (nil? (get opts :node-style))))
  (def tip-anchor
    (case direction "TB" "north" "BT" "south" "RL" "east" "west"))
  # build name mapping: original label -> {tikz-name, display-label}
  (def name-map @{})
  (var idx 0)
  (def seen-nodes (table))
  (def used-names (table))
  (each u (rec-nodes g)
    (unless (get seen-nodes u)
      (put seen-nodes u true)
      (def nm (tikz-node-name u idx used-names))
      (put used-names nm true)
      (put name-map u @{:name nm
                        :label (tikz-escape u) :idx idx})
      (++ idx)))
  (def buf @"")
  # tikzpicture opening
  (buffer/push-string buf "\\begin{tikzpicture}")
  (def env-parts @[])
  (when (and (not phylo-style) (not= node-style ""))
    (array/push env-parts (string "every node/.style={" node-style "}")))
  (array/push env-parts ">=latex")
  (when (not= tikz-opts "")
    (array/push env-parts tikz-opts))
  (if (> (length env-parts) 0)
    (buffer/push-string buf (string "[" (string/join env-parts ", ") "]")))
  (buffer/push-string buf "\n")
  (if explicit-mode
    (do
      (def tala-scale (when tala (tala-pos-scale (tala :pos))))
      (defn tip-anchor-for [u px py]
        (def np (get phylo-parent u))
        (if (nil? np)
          tip-anchor
          (do
            (def pp (get (use-layout :pos) np))
            (def ax px)
            (def ay (- 0 py))
            (def bx (pp 0))
            (def by (- 0 (pp 1)))
            (case direction
              "TB" (if (> ay by) "south" tip-anchor)
              "BT" (if (< ay by) "north" tip-anchor)
              "RL" (if (> ax bx) "west" tip-anchor)
              (if (< ax bx) "east" tip-anchor)))))
      # emit nodes with explicit coordinates
      (loop [[u info] :pairs name-map]
        (def p (when use-layout (get (use-layout :pos) u)))
        (if p
          (do
            (def is-tip (and phylo-style (not (get phylo-parents u))))
            (if is-tip
              (buffer/push-string buf
                (string "  \\node[anchor=" (tip-anchor-for u (p 0) (p 1))
                        ", inner sep=1.5pt, text=black!80] ("
                        (info :name) ") at ("
                        (tkz-ex (p 0) tala-scale)
                        "," (tkz-ex (- 0 (p 1)) tala-scale)
                        ") {" (info :label) "};\n"))
              (if (and phylo-style (get phylo-parents u))
                (buffer/push-string buf
                  (string "  \\node[circle, fill=black!70, line width=0.5pt, inner sep=0pt, minimum size=3.5pt] ("
                          (info :name) ") at ("
                          (tkz-ex (p 0) tala-scale)
                          "," (tkz-ex (- 0 (p 1)) tala-scale)
                          ") {};\n"))
                (buffer/push-string buf
                  (string "  \\node (" (info :name) ") at ("
                          (tkz-ex (p 0) tala-scale)
                          "," (tkz-ex (- 0 (p 1)) tala-scale)
                          ") {" (info :label) "};\n")))))
          (do
            (def pp (tikz-chain-pos (info :idx) (length name-map) direction))
            (buffer/push-string buf
              (string "  \\node (" (info :name) ") at ("
                      (string/format "%.4f" (pp 0))
                      "," (string/format "%.4f" (pp 1))
                      ") {" (info :label) "};\n")))))
      # emit edges
      (def orthogonal? (get gd-layout :orthogonal false))
      (def phylo-hook? (and orthogonal? (truthy? layout-edges)))
      (def edge-connector (if phylo-hook? "|-" (if orthogonal? "-|" "--")))
      # weight labels sit on the edge as plain text
      (def label-pos
        (cond
          phylo-hook?
          "near start,right, fill=none, draw=none, text=black!40, font=\\scriptsize"
          orthogonal?
          "near start,above, fill=none, draw=none, text=black!40, font=\\scriptsize"
          (= direction "TB")
          "midway,right, fill=none, draw=none, text=black!40, font=\\scriptsize"
          "midway,above, fill=none, draw=none, text=black!40, font=\\scriptsize"))
      (defn draw-head []
        (def opts @[])
        (when directed? (array/push opts "->"))
        (array/push opts "semithick")
        (when (not= edge-style "") (array/push opts edge-style))
        (if (pos? (length opts))
          (string "  \\draw[" (string/join opts ", ") "]")
          "  \\draw"))
      (defn emit-edge [u v w]
        (def ui (get name-map u))
        (def vi (get name-map v))
        (def parts @[])
        (when (and label-edges (number? w))
          (array/push parts
            (string "node[" label-pos "] {" (tikz-escape (string w)) "}")))
        # orthogonal connections route through node centres by default.
        # ends anchored so stroke joins parent at side and child at top/bottom
        (var from (ui :name))
        (var to (vi :name))
        (when orthogonal?
          (def p (get gd-layout :pos))
          (def pu (get p u))
          (def pv (get p v))
          (when (and pu pv)
            (def px (pu 0))
            (def py (- 0 (pu 1)))
            (def cx (pv 0))
            (def cy (- 0 (pv 1)))
            (if phylo-hook?
              # vertical first. leave parent through top/bottom border
              # and connect child at left/right border
              (do
                # north when child is above in TikZ coordinates, south is below
                (cond
                  (> cy py) (set from (string (ui :name) ".north"))
                  (< cy py) (set from (string (ui :name) ".south")))
                (if (and phylo-style (not (get phylo-parents v)))
                  (set to (string (string/format "%.4f" cx) ","
                                  (string/format "%.4f" cy)))
                  (cond
                    (> cx px) (set to (string (vi :name) ".west"))
                    (< cx px) (set to (string (vi :name) ".east")))))
              (do
                (if (> cx px)
                  (set from (string (ui :name) ".east"))
                  (set from (string (ui :name) ".west")))
                # tips draw as plain text with box starting at leaf hook
                # branch must stop at the tip coordinate to avoid label
                (if (and phylo-style (not (get phylo-parents v)))
                  (set to (string (string/format "%.4f" cx) ","
                                  (string/format "%.4f" cy)))
                  (if (> cy py)
                    (set to (string (vi :name) ".south"))
                    (set to (string (vi :name) ".north"))))))))
        (if (> (length parts) 0)
          (buffer/push-string buf
            (string (draw-head)
                    " (" from ") " edge-connector " "
                    (string/join parts (string " " edge-connector " "))
                    " (" to ");\n"))
          (buffer/push-string buf
            (string (draw-head)
                    " (" from ") " edge-connector " (" to ");\n"))))
      (if layout-edges
        (each uv layout-edges
          (def u (uv 0))
          (def v (uv 1))
          (def w (get-in (out-adjacency g) [u v]))
          (emit-edge u v (if (number? w) w nil)))
        (each-edge g directed? emit-edge)))
    (do
      # write \graph block
      (def grow
        (case direction
          "TB" "grow down"
          "BT" "grow up"
          "RL" "grow left"
          "grow right"))
      (def grow-opts @[])
      (array/push grow-opts (string grow " sep=" sep))
      (when (not= graph-opts "") (array/push grow-opts graph-opts))
      (array/push grow-opts "edges={semithick}")
      (buffer/push-string buf
        (string "  \\graph [" (string/join grow-opts ", ") "] {\n"))
      (def dec (tikz-chains g directed?))
      (def lines @[])
      (each chain (dec :chains)
        (var text (tikz-node-spec ((chain 0) 0) name-map))
        (each step chain
          (set text (string text " "
                            (tikz-edge-part directed?
                                            label-edges (step 2) edge-style)
                            " " (tikz-node-spec (step 1) name-map))))
        (array/push lines (string "    " text ";")))
      (each u (dec :isolated)
        (array/push lines (string "    " (tikz-node-spec u name-map) ";")))
      (when (pos? (length lines))
        (buffer/push-string buf (string/join lines "\n")))
      (buffer/push-string buf "\n  };\n")))
  (buffer/push-string buf "\\end{tikzpicture}\n")
  (string buf))
