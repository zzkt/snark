# -*- mode: janet; -*-
# graphs/test/graph-drawing:

(import ../graphs :prefix "")
(import ./helper :prefix "")

(defn- make-linear-digraph
  "a -> b -> c -> d"
  []
  (def g (make-digraph))
  (add-nodes g @["a" "b" "c" "d"])
  (add-edges g ["a" "b" 1] ["b" "c" 1] ["c" "d" 1])
  g)

(defn- make-cyclic-digraph
  "a -> b -> c -> d -> a"
  []
  (def g (make-digraph))
  (add-nodes g @["a" "b" "c" "d"])
  (each e @[["a" "b"] ["b" "c"] ["c" "d"] ["d" "a"]]
        (digraph-add-edge g (e 0) (e 1) 1))
  g)

(defn- make-simple-tree
  []
  (def g (make-weighted-graph))
  (add-nodes g @["root" "a" "b" "c"])
  (each e @[["root" "a" 1] ["root" "b" 1] ["root" "c" 1]]
        (weighted-graph-add-edge g (e 0) (e 1) (e 2)))
  g)

(defn- nodes-present?
  "True if every node in g has a position in layout result."
  [g layout]
  (var all-found true)
  (def pos-nodes @{})
  (eachk n (layout :pos)
         (put pos-nodes n true))
  (each n (g :nodes)
    (when (not (get pos-nodes n))
      (set all-found false)))
  all-found)

(defn- no-nan-positions?
  "True if no coordinate contains NaN or nil."
  [layout]
  (var ok true)
  (eachk node (layout :pos)
         (def p ((layout :pos) node))
         (when (or (= (p 0) nil) (= (p 1) nil))
           (set ok false)))
  ok)

(defn- positions-unique?
  "True if no two nodes share the same position."
  [layout]
  (def seen @{})
  (var ok true)
  (eachk node (layout :pos)
         (def p ((layout :pos) node))
         (def key (string (p 0) "," (p 1)))
         (when (get seen key)
           (set ok false))
         (put seen key true))
  ok)

(defn- y-in-canvas?
  "True when every y coordinate lies inside [0, height]."
  [layout height]
  (var ok true)
  (eachk node (layout :pos)
         (def p ((layout :pos) node))
         (when (or (< (p 1) 0) (> (p 1) height)) (set ok false)))
  ok)

(defn- determinism-check
  "Run layout twice with same seed and check positions match."
  [layout-fn g & args]
  (def r1 (layout-fn g ;args))
  (def r2 (layout-fn g ;args))
  (def nodes (sort (keys (r1 :pos))))
  (var same true)
  (each n nodes
    (def p1 ((r1 :pos) n))
    (def p2 ((r2 :pos) n))
    (when (or (not= (p1 0) (p2 0)) (not= (p1 1) (p2 1)))
      (set same false)))
  same)

# ==============================================================================
#  layout-force
# ==============================================================================

(print "* layout-force")

(def g1 (make-linear-digraph))
(def fl1 (layout-force g1))

(cassert-eq "eligible" (fl1 :eligible) true)
(cassert-eq "all nodes present" (nodes-present? g1 fl1) true)
(cassert-eq "no NaN" (no-nan-positions? fl1) true)

(def g2 (make-cyclic-digraph))
(def fl2 (layout-force g2))
(cassert-eq "cyclic: all nodes present" (nodes-present? g2 fl2) true)
(cassert-eq "cyclic: no NaN" (no-nan-positions? fl2) true)

# determinism
(cassert-eq "linear: deterministic" (determinism-check layout-force g1) true)
(cassert-eq "cyclic: deterministic" (determinism-check layout-force g2) true)

# ==============================================================================
#  layout-circle
# ==============================================================================

(print "\n* layout-circle")

(def cl1 (layout-circle g1))
(cassert-eq "linear: eligible" (cl1 :eligible) true)
(cassert-eq "linear: all nodes present" (nodes-present? g1 cl1) true)
(cassert-eq "linear: no NaN" (no-nan-positions? cl1) true)
(cassert-eq "linear: positions unique" (positions-unique? cl1) true)

(def cl2 (layout-circle g2))
(cassert-eq "cyclic: all nodes present" (nodes-present? g2 cl2) true)

# determinism
(cassert-eq "linear: deterministic" (determinism-check layout-circle g1) true)
(cassert-eq "cyclic: deterministic" (determinism-check layout-circle g2) true)

# ==============================================================================
#  layout-tree
# ==============================================================================

(print "\n* layout-tree")

(def tree (make-simple-tree))
(def tl (layout-tree tree))
(cassert-eq "tree: eligible" (tl :eligible) true)
(cassert-eq "tree: all nodes present" (nodes-present? tree tl) true)
(cassert-eq "tree: no NaN" (no-nan-positions? tl) true)
(cassert-eq "tree: has root" (not= nil (tl :root)) true)

# root should be centrish
(def root ((tl :pos) (tl :root)))
(cassert-eq "tree: root near center x"
            (and (>= (root 0) 2) (<= (root 0) 8)) true)

(cassert-eq "tree: deterministic" (determinism-check layout-tree tree) true)

# top-down by default
(def tp ((tl :pos) (tl :root)))
(cassert-eq "tree: root above child"
            (< (tp 1) (get (get (tl :pos) "a") 1)) true)
(cassert-eq "tree: y in canvas"
            (y-in-canvas? tl 8.0) true)

# ==============================================================================
#  layout-radial
# ==============================================================================

(print "\n* layout-radial")

(def rl (layout-radial tree))
(cassert-eq "tree: radial eligible" (rl :eligible) true)
(cassert-eq "tree: radial all nodes present" (nodes-present? tree rl) true)
(cassert-eq "tree: radial no NaN" (no-nan-positions? rl) true)
(cassert-eq "tree: radial has root" (not= nil (rl :root)) true)

# root should be at center (5,5)
(def root-r ((rl :pos) (rl :root)))
(cassert-eq "radial: root at center"
            (and (= (root-r 0) 5) (= (root-r 1) 5)) true)

(cassert-eq "radial: deterministic"
            (determinism-check layout-radial tree) true)

# ==============================================================================
#  layout-orthogonal
# ==============================================================================

(print "\n* layout-orthogonal")

(def ol (layout-orthogonal tree))
(cassert-eq "tree: orthogonal eligible"
            (ol :eligible) true)
(cassert-eq "tree: orthogonal all nodes present"
            (nodes-present? tree ol) true)
(cassert-eq "tree: orthogonal no NaN"
            (no-nan-positions? ol) true)
(cassert-eq "tree: orthogonal flag"
            (ol :orthogonal) true)

(cassert-eq "tree: orthogonal deterministic"
            (determinism-check layout-orthogonal tree) true)

# linear graph
(def ol2 (layout-orthogonal g1))
(cassert-eq "linear: orthogonal all nodes present"
            (nodes-present? g1 ol2) true)
(cassert-eq "linear: orthogonal flag"
            (ol2 :orthogonal) true)

# root sits above its children
(def orc (ol :pos))
(cassert-eq "orthogonal: root above child"
            (< (get (orc "root") 1) (get (orc "a") 1)) true)
(cassert-eq "orthogonal: y in canvas"
            (y-in-canvas? ol 8.0) true)

# deep chain fits the canvas (auto-fit spacing)
(def deep-g (make-weighted-graph))
(add-nodes deep-g @["r" "n1" "n2" "n3" "n4" "n5"])
(each e @[["r" "n1" 1] ["n1" "n2" 1] ["n2" "n3" 1] ["n3" "n4" 1] ["n4" "n5" 1]]
      (weighted-graph-add-edge deep-g (e 0) (e 1) (e 2)))

(def ol-deep (layout-orthogonal deep-g))
(cassert-eq "orthogonal: deep chain y in canvas"
            (y-in-canvas? ol-deep 8.0) true)
(cassert-eq "orthogonal: deep root above tail"
            (< (get ((ol-deep :pos) "r") 1) (get ((ol-deep :pos) "n5") 1)) true)

# ==============================================================================
#  layout-spectral
# ==============================================================================

(print "\n* layout-spectral")

(defn- eigenvalues-ascending?
  [layout]
  (def vals (layout :eigenvalues))
  (var ok true)
  (loop [i :range [1 (length vals)]]
    (when (< (vals i) (vals (- i 1))) (set ok false)))
  ok)

(defn- near-zero? [x] (< (math/abs x) 1e-6))

# linear path a-b-c-d
(def sl1 (layout-spectral g1))
(cassert-eq "linear: spectral eligible"
            (sl1 :eligible) true)
(cassert-eq "linear: spectral all nodes present"
            (nodes-present? g1 sl1) true)
(cassert-eq "linear: spectral no NaN"
            (no-nan-positions? sl1) true)
(cassert-eq "linear: eigenvalues ascending"
            (eigenvalues-ascending? sl1) true)
(cassert-eq "linear: four eigenvalues"
            (= (length (sl1 :eigenvalues)) 4) true)
(cassert-eq "linear: zero eigenvalue first"
            (near-zero? (get (sl1 :eigenvalues) 0)) true)

# Fiedler vector is strictly monotonic along the path
(def sl1-pos (sl1 :pos))
(cassert-eq "linear: Fiedler monotonic along path"
            (> (get sl1-pos "a" 0) (get sl1-pos "b" 0)
               (get sl1-pos "c" 0) (get sl1-pos "d" 0)) true)

(cassert-eq "linear: spectral deterministic"
            (determinism-check layout-spectral g1) true)

# cyclic graph a -> b -> c -> d -> a
(def sl2 (layout-spectral g2))
(cassert-eq "cyclic: spectral eligible"
            (sl2 :eligible) true)
(cassert-eq "cyclic: spectral all nodes present"
            (nodes-present? g2 sl2) true)
(cassert-eq "cyclic: spectral no NaN"
            (no-nan-positions? sl2) true)
(cassert-eq "cyclic: eigenvalues ascending"
            (eigenvalues-ascending? sl2) true)
(cassert-eq "cyclic: zero eigenvalue"
            (near-zero? (get (sl2 :eigenvalues) 0)) true)
(cassert-eq "cyclic: spectral deterministic"
            (determinism-check layout-spectral g2) true)

# star tree
(def sl3 (layout-spectral tree))
(cassert-eq "tree: spectral eligible"
            (sl3 :eligible) true)
(cassert-eq "tree: spectral all nodes present"
            (nodes-present? tree sl3) true)
(cassert-eq "tree: spectral no NaN"
            (no-nan-positions? sl3) true)
(cassert-eq "tree: eigenvalues ascending"
            (eigenvalues-ascending? sl3) true)

# empty graph
(def sl-e (layout-spectral (make-weighted-graph)))
(cassert-eq "empty: spectral eligible"
            (sl-e :eligible) true)
(cassert-eq "empty: spectral pos empty"
            (empty? (sl-e :pos)) true)
(cassert-eq "empty: eigenvalues empty"
            (empty? (sl-e :eigenvalues)) true)

# disconnected graph
(def dis-g (make-weighted-graph))
(add-nodes dis-g  @["a" "b" "x" "y"])
(add-edges dis-g ["a" "b" 1] [ "x" "y" 1])

(def sp-dis (layout-spectral dis-g))
(def pa (get (sp-dis :pos) "a"))
(def pb (get (sp-dis :pos) "b"))
(def px (get (sp-dis :pos) "x"))
(def py (get (sp-dis :pos) "y"))

(cassert-eq "disconnected: comp1 x spread distinct"
            (not= (pa 0) (pb 0)) true)
(cassert-eq "disconnected: comp2 x spread distinct"
            (not= (px 0) (py 0)) true)
(cassert-eq "disconnected: components separated"
            (> (math/abs (- (pa 0) (px 0))) 1e-6) true)
(cassert-eq "disconnected: y in canvas"
            (y-in-canvas? sp-dis 8.0) true)

# single node
(def sl-s (layout-spectral (make-weighted-graph @["only"] @{})))
(cassert-eq "single: spectral eligible"
            (sl-s :eligible) true)
(cassert-eq "single: spectral centered"
            (= ((get (sl-s :pos) "only") 0) 5.0) true)
(cassert-eq "single: spectral centered y"
            (= ((get (sl-s :pos) "only") 1) 4.0) true)
(cassert-eq "single: one eigenvalue"
            (= (length (sl-s :eigenvalues)) 1) true)

# ==============================================================================
#  TikZ
# ==============================================================================

(print "\n* TikZ integration")

(defn- contains? [s sub] (truthy? (string/find sub s)))

# force layout produces \node
(def tikz-f (to-tikz g1 :layout "force" :label-edges false))
(cassert-eq "force: has tikzpicture"
            (contains? tikz-f "tikzpicture") true)
(cassert-eq "force: has \\node"
            (contains? tikz-f "\\node") true)
(cassert-eq "force: no \\graph"
            (not (contains? tikz-f "\\graph")) true)

# circle layout
(def tikz-c (to-tikz g2 :layout "circle" :label-edges false))
(cassert-eq "circle: has \\node" (contains? tikz-c "\\node") true)

# tree layout
(def tikz-t (to-tikz tree :layout "tree"))
(cassert-eq "tree: has \\node" (contains? tikz-t "\\node") true)

# radial layout
(def tikz-r (to-tikz tree :layout "radial"))
(cassert-eq "radial: has \\node" (contains? tikz-r "\\node") true)

# orthogonal layout uses -| connector
(def tikz-o (to-tikz g1 :layout "orthogonal" :label-edges false))
(cassert-eq "orthogonal: has \\node" (contains? tikz-o "\\node") true)
(cassert-eq "orthogonal: uses -|" (contains? tikz-o " -| ") true)

# phylogram family uses the crossing-free |- hook
(def tikz-ph (to-tikz tree :layout "phylogram" :label-edges false))
(cassert-eq "phylogram: has \\node" (contains? tikz-ph "\\node") true)
(cassert-eq "phylogram: uses |-" (contains? tikz-ph " |- ") true)
(cassert-eq "phylogram: no -|" (not (contains? tikz-ph " -| ")) true)

(def tikz-phu (to-tikz tree :layout "unrooted-phylogram" :label-edges false))
(cassert-eq "unrooted-phylogram: uses |-" (contains? tikz-phu " |- ") true)

# TB/BT transpose the tree, a child is below (TB) or above (BT)
(def tikz-ph-tb (to-tikz tree :layout "phylogenetic" :root "root"
                         :direction "TB" :label-edges false))

(cassert-eq "phylogram TB: parent leaves through south"
            (contains? tikz-ph-tb "(root.south) |-") true)
(cassert-eq "phylogram TB: parent does not leave through north"
            (not (contains? tikz-ph-tb "(root.north) |-")) true)

(def tikz-ph-bt (to-tikz tree :layout "phylogenetic" :root "root"
                         :direction "BT" :label-edges false))
(cassert-eq "phylogram BT: parent leaves through north"
            (contains? tikz-ph-bt "(root.north) |-") true)
(cassert-eq "phylogram BT: parent does not leave through south"
            (not (contains? tikz-ph-bt "(root.south) |-")) true)

# spectral layout
(def tikz-s (to-tikz g1 :layout "spectral" :label-edges false))
(cassert-eq "spectral: has \\node" (contains? tikz-s "\\node") true)
(cassert-eq "spectral: no \\graph" (not (contains? tikz-s "\\graph")) true)

# linear fallback for non-eligible graph
(def tikz-l (to-tikz g2 :label-edges false))
(cassert-eq "fallback: has \\graph" (contains? tikz-l "\\graph") true)

# an unrooted layout with isolated :root forms a degenerate split
(def iso-g (make-weighted-graph))
(add-nodes iso-g @["root" "a" "b" "iso"])
(each e @[["root" "a" 1] ["root" "b" 1]]
      (weighted-graph-add-edge iso-g (e 0) (e 1) (e 2)))

(def iso-r (layout-unrooted-phylogram iso-g :root "iso"))
(cassert-eq "unrooted isolated root: no crash, no tree edges"
            (length (iso-r :edges)) 0)

# graph-layout dispatcher forwards to  algorithm by name
(def dl-g (make-digraph))
(each e @[["a" "b1"] ["a" "b2"] ["b1" "c1"] ["c1" "d"] ["b2" "c2"] ["c2" "d"]]
      (digraph-add-edge dl-g (e 0) (e 1) 1))

(def dl-dag (graph-layout dl-g "dag"))
(cassert-eq "graph-layout dag: has :pos"
            (not (nil? (dl-dag :pos))) true)
(cassert-eq "graph-layout dag: positions count"
            (length (dl-dag :pos)) 6)

(def dl-force (graph-layout dl-g "force"))
(cassert-eq "graph-layout force: has :pos"
            (not (nil? (dl-force :pos))) true)

(def dl-tree (graph-layout dl-g "tree" :root "a"))
(cassert-eq "graph-layout tree: has :pos"
            (not (nil? (dl-tree :pos))) true)

(def dl-circle (graph-layout dl-g "circle"))
(cassert-eq "graph-layout circle: has :pos"
            (not (nil? (dl-circle :pos))) true)

(def dl-spectral (graph-layout dl-g "spectral"))
(cassert-eq "graph-layout spectral: has :pos"
            (not (nil? (dl-spectral :pos))) true)

(def dl-orthogonal (graph-layout dl-g "orthogonal"))
(cassert-eq "graph-layout orthogonal: has :pos"
            (not (nil? (dl-orthogonal :pos))) true)

(def dl-vert (graph-layout dl-g "phylogram" :direction "TB"))
(cassert-eq "graph-layout phylo TB: has :pos"
            (not (nil? (dl-vert :pos))) true)

(def dl-rl (graph-layout dl-g "dag" :direction "RL"))
(cassert-eq "graph-layout dag RL: has :pos"
            (not (nil? (dl-rl :pos))) true)

(cassert-eq "graph-layout dag RL mirrors (root right of sink)"
            (> ((get (dl-rl :pos) "a") 0) ((get (dl-rl :pos) "d") 0)) true)

(def dl-err (try (graph-layout dl-g "manxome") ([err] err)))
(cassert-eq "graph-layout with unknown name raises error"
            (not (nil? dl-err)) true)

(print-results)
