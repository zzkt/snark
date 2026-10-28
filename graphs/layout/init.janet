# -*- mode: janet; -*-
# graphs/layout/init: layout package aggregator and dispatcher

(import deft :prefix "")
(import ../types :prefix "")

(import ./dag :prefix "_d_")
(import ./force :prefix "_f_")
(import ./circle :prefix "_c_")
(import ./tree :prefix "_t_")
(import ./radial :prefix "_r_")
(import ./orthogonal :prefix "_o_")
(import ./spectral :prefix "_s_")
(import ./phylo :prefix "_p_")
(import ./spring :prefix "_sp_")
(import ./polyarc :prefix "_pa_")

(def layout-dag _d_layout-dag)
(def layout-force _f_layout-force)
(def layout-circle _c_layout-circle)
(def layout-tree _t_layout-tree)
(def layout-radial _r_layout-radial)
(def layout-orthogonal _o_layout-orthogonal)
(def layout-spectral _s_layout-spectral)
(def layout-spring _sp_layout-spring)
(def layout-polyarc _pa_layout-polyarc)
(def layout-lombardi _pa_layout-polyarc) # alias
(def layout-phylogenetic _p_layout-phylogenetic)
(def layout-phylogram _p_layout-phylogram)
(def layout-phylogenetic-tree _p_layout-phylogenetic-tree)
(def layout-unrooted-phylogram _p_layout-unrooted-phylogram)
(def layout-unrooted-phylogenetic-tree _p_layout-unrooted-phylogenetic-tree)

# ==============================================================================
#  graph-layout — named dispatcher
# ==============================================================================

(define graph-layout
  ```Dispatch to a named layout algorithm by string.

  (graph-layout g "force")
  (graph-layout g "tree" :root "r")

  The layout name is passed positionally as a string.  Supported names:

    "dag", "tala"                         — hierarchical TALA pipeline
    "force"                               — Fruchterman-Reingold
    "spring"                              — FR spring + planar (Tutte) fallback
    "lombardi"                            — strict LM solver, single-arc
                                            Lombardi when the graph admits it
    "polyarc"                             — direct tangent-slot poly-arc Lombardi
    "circle"                              — nodes on a circle
    "tree"                                — rooted BFS tree
    "radial"                              — concentric rings around a root
    "orthogonal"                          — grid placement, orthogonal edges
    "spectral"                            — Laplacian eigenvectors
    "phylogenetic", "phylogram",
    "phylogenetic-tree",
    "unrooted-phylogram",
    "unrooted-phylogenetic-tree"          — phylogram family

  &keys are forwarded to the chosen layout.  :direction is translated per
  family: for the dag/tala family "TB"/"BT"/"LR"/"RL" map to the layout's
  "tb"/"bt"/"lr"/"rl"; for the phylogram family "TB" picks vertical growth,
  "BT" vertical + mirror, "RL" mirror.  Unknown layout names raise an error.

  Returns the chosen layout's {:pos {node [x y]}} table.```
  [g :weighted layout-name :string & kvs]
  (let [opts (table ;kvs)
        registry @{ "dag" layout-dag
                    "tala" layout-dag
                    "force" layout-force
                    "spring" layout-spring
                    "lombardi" layout-lombardi
                    "polyarc" layout-polyarc
                    "circle" layout-circle
                    "tree" layout-tree
                    "radial" layout-radial
                    "orthogonal" layout-orthogonal
                    "spectral" layout-spectral
                    "phylogenetic" layout-phylogenetic
                    "phylogram" layout-phylogram
                    "phylogenetic-tree" layout-phylogenetic-tree
                    "unrooted-phylogram" layout-unrooted-phylogram
                    "unrooted-phylogenetic-tree" layout-unrooted-phylogenetic-tree }
        layout (get registry layout-name)
        phylo? (get @{"phylogenetic" true "phylogram" true "phylogenetic-tree" true
                       "unrooted-phylogram" true "unrooted-phylogenetic-tree" true}
                     layout-name)
        direction (get opts :direction)
        fwd (reduce (fn [acc k]
                      (if (= k :direction)
                        acc
                        (do (array/push acc k)
                            (array/push acc (get opts k))
                            acc)))
                    @[]
                    (keys opts))]
    (when (nil? layout)
      (error (string "graph-layout: unknown layout '" layout-name "'")))
    (when direction
      (if phylo?
        (case direction
          "TB" (array/push fwd :vertical true)
          "BT" (do (array/push fwd :vertical true) (array/push fwd :mirror true))
          "RL" (array/push fwd :mirror true))
        (array/push fwd :direction
          (case direction "TB" "tb" "BT" "bt" "LR" "lr" "RL" "rl" "lr"))))
    (apply layout g fwd)))
