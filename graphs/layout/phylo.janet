# -*- mode: janet; -*-
# graphs/layout/phylo: rooted and unrooted phylogram layouts for weighted trees

#  The phylogenetics family draws spanning trees whose edges weights are length
#
#   layout-phylogram                   — rooted rectangular phylogram
#   layout-unrooted-phylogram          — unrooted rectangular phylogram
#   layout-phylogenetic-tree           — rooted, tips aligned (dendrogram)
#   layout-unrooted-phylogenetic-tree  — unrooted, tips aligned

(import deft :prefix "")
(import ../types :prefix "")
(import ../util :prefix "")
(import ./common :prefix "")
(import ../record :prefix "r-")

(defn- _edge-weight
  "Weight of edge (u,v) from the adjacency, or nil when absent/non-numeric."
  [g u v]
  (def w (get-in (r-adjacency g) [u v]))
  (if (number? w) w nil))

(defn- _phylogeny-parents
  "Breadth-first rooted spanning forest over the undirected adjacency, with each
node's children sorted canonically for determinism. When :block is given, that
neighbour is never traversed, so the traversal covers half of an unrooted split."
  [g root &keys keys]
  (let [block (get keys :block)
        visited @{root true}
        children @{}]
    (var frontier @[root])
    (while (> (length frontier) 0)
      (def next-frontier @[])
      (each u frontier
        (put children u @[])
        (def nbrs (seq [[v _] :pairs (or (get (r-adjacency g) u) @{})
                       :when (and (not= u v) (not= v block) (not (get visited v)))]
                v))
        (sort-with nbrs node-cmp)
        (each v nbrs
          (put visited v true)
          (array/push (children u) v)
          (array/push next-frontier v)))
      (set frontier next-frontier))
    {:children children :visited visited}))

(defn- _phylo-weighted-dists
  "Shortest tree distances from start with parent tracking (Dijkstra on a tree)."
  [g start]
  (let [dist @{start 0.0}
        prev @{}]
    (var frontier @{start true})
    (while (> (length frontier) 0)
      (def next @{})
      (loop [[u _] :pairs frontier]
        (def du (dist u))
        (loop [[v _] :pairs (or (get (r-adjacency g) u) @{})]
          (when (not= u v)
            (def w (_edge-weight g u v))
            (def d (+ du (if w w 1.0)))
            (when (or (not (get dist v)) (< d (dist v)))
              (put dist v d)
              (put prev v u)
              (put next v true)))))
      (set frontier next))
    {:dist dist :prev prev}))

(defn- _phylo-diameter-path
  "Endpoints and the weighted diameter path of a tree (two-sided search)."
  [g start]
  (def p1 (_phylo-weighted-dists g start))
  (var a start)
  (var da (get (p1 :dist) start))
  (loop [[u d] :pairs (p1 :dist)]
    (when (> d da) (set a u) (set da d)))
  (def p2 (_phylo-weighted-dists g a))
  (var b a)
  (var db -1.0)
  (loop [[u d] :pairs (p2 :dist)]
    (when (> d db) (set b u) (set db d)))
  (def rev @[])
  (var cur b)
  (array/push rev cur)
  (while (and (not= cur a) (get (p2 :prev) cur))
    (set cur (get (p2 :prev) cur))
    (array/push rev cur))
  (def path (reverse rev))
  (var total 0.0)
  (for i 0 (- (length path) 1)
    (def w (_edge-weight g (path i) (path (+ i 1))))
    (+= total (if w w 1.0)))
  {:path path :total total})

(defn- _phylogeny-center
  "For unrooted layouts, two adjacent roots straddling the tree's centre."
  [g]
  (let [root (_tree-roots g)
        st (_phylogeny-parents g root)
        n2 (seq [[u kids] :pairs (st :children)
                        :when (= (length kids) 2)]
                u)]
    (if (pos? (length n2))
      (do
        (sort-with n2 node-cmp)
        (def u (n2 0))
        (def kids-u (get (st :children) u))
        @[u (if (> (length kids-u) 0) (kids-u 0) u)])
      (let [diam (_phylo-diameter-path g root)
            path (diam :path)
            half (/ (diam :total) 2)]
        (var acc 0.0)
        (var split nil)
        (for i 0 (- (length path) 1)
          (when split (break))
          (let [u (path i)
                v (path (+ i 1))
                w (or (_edge-weight g u v) 1.0)]
            (when (>= (+ acc w) half)
              (set split (if (= acc half) @[v u] @[u v])))
            (set acc (+ acc w))))
        (or split @[root root])))))

(defn- _phylogeny-rows
  "Leaf rows by depth-first descent so sibling subtrees never overlap.
Returns {:rows {leaf row} :nrow count} and the DFS leaf order."
  [root children]
  (let [rows @{}
        order @[]]
    (var row 0)
    (defn walk [u]
      (def kids (get children u @[]))
      (if (empty? kids)
        (do (put rows u row) (++ row) (array/push order u))
        (each c kids (walk c))))
    (walk root)
    {:rows rows :order order :nrow row}))

(defn- _phylo-layout
  ```Shared layout used for phylogram layouts. Builds a two-coordinate frame with
 depth along the growth axis, and leaf rows across.

   &keys
     :root       — ancestral root (default: heuristic) or :unrooted hub node
     :align-tips — align all leaves to the deepest tip
     :unrooted   — split the tree about its centre into two depth halves
     :vertical   — grow along y instead of x
     :mirror     — invert the depth axis (grow left/up)
     :vspacing   — gap between leaf rows (default 1.0)
     :hspacing   — on-paper length of one weight unit, used as the
                   "evolutionary unit length" (default 1.0)
```
  [g opts]
  (let [nodes (canonical-nodes (r-nodes g))
        n (length nodes)]
    (cond
      (<= n 0) @{:pos @{} :edges @[] :eligible true}
      (= n 1) @{:pos @{(nodes 0) @[0 0]} :edges @[] :eligible true :root (nodes 0)
                :orthogonal true}
      (let [vspacing (get opts :vspacing 1.0)
            hspacing (get opts :hspacing 1.0)
            align? (get opts :align-tips false)
            unrooted (get opts :unrooted false)
            vertical (get opts :vertical false)
            mirror (get opts :mirror false)
            vchildren @{}
            visited @{}
            side-b @{}]
        (var vroot nil)
        (var root-here nil)
        (def unrooted-pair
          (when unrooted
            (if (get opts :root)
              (let [rr (get opts :root)
                    st (_phylogeny-parents g rr)
                    ch (get (st :children) rr @[])]
                (if (> (length ch) 0) @[rr (ch 0)] @[rr rr]))
              (_phylogeny-center g))))
        (if (and unrooted-pair (not= (unrooted-pair 0) (unrooted-pair 1)))
          (do
            (def pair unrooted-pair)
            (def r1 (pair 0))
            (def r2 (pair 1))
            (def pa (_phylogeny-parents g r1 :block r2))
            (def pb (_phylogeny-parents g r2 :block r1))
            # virtual tree: root r1, first child r2, then r1's A-side children
            (def ch @[r2])
            (each c (get (pa :children) r1 @[]) (array/push ch c))
            (put vchildren r1 ch)
            (loop [[u k] :pairs (pa :children)]
              (unless (= u r1) (put vchildren u k)))
            (loop [[u k] :pairs (pb :children)]
              (put vchildren u k))
            (put vchildren r2 (get (pb :children) r2 @[]))
            # side-b: only the partner subtree (from pb, not vchildren)
            (put side-b r2 true)
            (defn mark-b [u]
              (each c (get (pb :children) u @[])
                (put side-b c true)
                (mark-b c)))
            (mark-b r2)
            (set vroot r1)
            (set root-here r1)
            (put visited r1 true)
            (put visited r2 true)
            (loop [[u _] :pairs (pa :visited)] (put visited u true))
            (loop [[u _] :pairs (pb :visited)] (put visited u true)))
          (do
            (def root (get opts :root (_tree-roots g)))
            (def tree (_phylogeny-parents g root))
            (loop [[u k] :pairs (tree :children)] (put vchildren u k))
            (set vroot root)
            (set root-here root)
            (loop [[u _] :pairs (tree :visited)] (put visited u true))))
        (let [rows (_phylogeny-rows vroot vchildren)
              nrow (rows :nrow)
              # depth: cumulative branch length from axis (sign per half)
              x @{}
              y @{}
              edges @[]]
          (put x vroot 0.0)
          (defn descend [u]
            (each c (get vchildren u @[])
              (def w (_edge-weight g u c))
              (def sgn (if unrooted (if (get side-b c) 1 -1) 1))
              (put x c (+ (get x u) (* (if w (* w hspacing) hspacing) sgn)))
              (array/push edges @[u c])
              (descend c))
            (def kids (get vchildren u @[]))
            (if (empty? kids)
              (put y u (* (get (rows :rows) u) vspacing))
              (do
                (put y u (/ (reduce (fn [acc c] (+ acc (get y c))) 0 kids)
                            (length kids))))))
          (descend vroot)
          # disconnected leftovers: stack remaining after the DFS leaves
          (when (< nrow n)
            (var rest-y (* nrow vspacing))
            (each u nodes
              (unless (get visited u)
                (put y u rest-y)
                (set rest-y (+ rest-y vspacing))
                (put x u 0.0))))
          # aligned tips: flush each leaf onto deepest tip column (of half)
          (when align?
            (def leaves (seq [u :in nodes
                             :when (empty? (get vchildren u @[]))]
                            u))
            (if unrooted
              (when (pos? (length leaves))
                (var right -1e18)
                (var left 1e18)
                (each u leaves
                  (if (get side-b u)
                    (set right (max right (get x u)))
                    (set left (min left (get x u)))))
                (each u leaves
                  (put x u (if (get side-b u) right left))))
              (do
                (var mx -1e18)
                (each u leaves (set mx (max mx (get x u))))
                (each u leaves (put x u mx)))))
          (let [pos @{}]
            (each u nodes
              (var px (get x u 0.0))
              (var py (get y u 0.0))
              (when mirror (set px (- 0 px)))
              (if vertical (put pos u @[py px]) (put pos u @[px py])))
            @{:pos pos :edges edges :eligible true
              :root root-here :orthogonal true}))))))

(defn- _phylo-opts
  "Option table for phylo wrappers."
  [keys align-tips unrooted]
  (merge (table ;keys) {:align-tips align-tips :unrooted unrooted}))


(define layout-phylogram
  ```Deterministic rooted rectangular phylogram layout.

  Draws a rooted tree where the root sits leftmost, branch lengths
  accumulate as the horizontal coordinate, and leaves stack into distinct
  vertical rows (sibling clades shouldn't overlap). Each internal node is
  centred vertically on its descendants. Edge weights are read as branch
  lengths.

  Works on any graph, most suitable for rooted weighted trees.

   &keys
     :root      — ancestral root node (default: heuristic)
     :vertical  — grow along the y axis instead of x
     :mirror    — invert the depth axis (grow left / up)
     :vspacing  — gap between leaf rows (default 1.0)
     :hspacing  — on-paper length of one weight unit (default 1.0)

   Returns {:pos {node [x y]} :edges [[parent child] ...]
            :eligible true :root node :orthogonal true}
```
  [g :weighted & keys] :table
  (_phylo-layout g (_phylo-opts keys false false)))


(define layout-unrooted-phylogram
  ```Unrooted rectangular phylogram layout.

  Draws a tree split about its centre (hub), where each half grows outwards
  on a common axis in it own depth direction. Branch lengths accumulate as
  the horizontal coordinate, and leaves stack into distinct vertical rows.
  Each internal node is centred vertically on its descendants. Edge weights
  are read as branch lengths.

   &keys
     :root      — selects the hub node (first child forms the split)
     :vertical  — grow along the y axis instead of x
     :mirror    — invert the depth axis (grow left / up)
     :vspacing  — gap between leaf rows (default 1.0)
     :hspacing  — on-paper length of one weight unit (default 1.0)

   Returns {:pos {node [x y]} :edges [[parent child] ...]
            :eligible true :root hub :orthogonal true}
```
  [g :weighted & keys] :table
  (_phylo-layout g (_phylo-opts keys false true)))


(define layout-phylogenetic-tree
  ```Deterministic phylogenetic tree layout (rooted, with leaves aligned).

  A cladogram/dendrogram where every leaf is aligned flush to the deepest
  tip column so all leaf taxa are vertically aligned. Internal nodes and
  branch lengths are presented as in layout-phylogram.

   &keys
     :root      — ancestral root node (default: heuristic)
     :vertical  — grow along the y axis instead of x
     :mirror    — invert the depth axis (grow left / up)
     :vspacing  — gap between leaf rows (default 1.0)
     :hspacing  — on-paper length of one weight unit (default 1.0)

   Returns {:pos {node [x y]} :edges [[parent child] ...]
            :eligible true :root node :orthogonal true}```
  [g :weighted & keys] :table
  (_phylo-layout g (_phylo-opts keys true false)))


(define layout-unrooted-phylogenetic-tree
  ```Unrooted phylogenetic tree layout (tips aligned).

   The cladogram style for unrooted tree.

   &keys: as layout-unrooted-phylogram.

   Returns {:pos {node [x y]} :edges [[parent child] ...]
            :eligible true :root hub :orthogonal true}```
  [g :weighted & keys] :table
  (_phylo-layout g (_phylo-opts keys true true)))


(define layout-phylogenetic
  ```Alias for layout-phylogram (rooted rectangular phylogram).

   Returns {:pos {node [x y]} :edges [[parent child] ...]
            :eligible true :root node :orthogonal true}```
  [g :weighted & keys] :table
  (_phylo-layout g (_phylo-opts keys false false)))
