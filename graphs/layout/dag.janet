# -*- mode: janet; -*-
# graphs/layout/dag:  Hierarchical DAG layout

#  Hierarchical DAG layout based on TALA/d2 layout algorithm.
#
#    -> network-simplex ranking (Gansner)      _rank
#    -> TALA eligibility gate                  _eligibility-reason
#    -> long-edge splitting                    _break-long-connections
#    -> crossing minimization (Gansner)        _minimize-hierarchy-crossings
#    -> global sifting (Matuszewski)           _global-sifting
#    -> placement with control points          _place-nodes-by-level

(import deft :prefix "")
(import ../types :prefix "")
(import ../util :prefix "")

(import ../record)
(import ../heap)
(import ../dag)
(import ../connectivity)

# ==============================================================================
#  Parameters
# ==============================================================================

# Geometry
(def _default-node-width 150)
(def _default-node-height 60)
(def _min-port-clearance 20.0)
(def _crossing-spacing 50.0)
(def _parent-spacing 300.0)
(def _sibling-spacing 60.0)

# Lengths are compared via tolerance rather than equality
(def _precision 0.0001)

# Limits
(def _min-hierarchy-levels 3)
(def _max-automatic-workflow-nodes 128)
(def _max-automatic-workflow-edges 256)
(def _max-engine-nodes 10000)
(def _max-engine-edges 50000)
(def _max-graph-size 30000)

# Sifting sweeps every node
(def _max-sift-iterations 10)


# ==============================================================================
#  Generic helpers
# ==============================================================================

(defn- _swap-arr
  "Swap two positions in a mutable array in place."
  [arr i j]
  (def t (arr i))
  (put arr i (arr j))
  (put arr j t))

(defn- _euclidean-distance
  "Euclidean distance with axis fast paths."
  [x1 y1 x2 y2]
  (cond
    (= x1 x2) (math/abs (- y1 y2))
    (= y1 y2) (math/abs (- x1 x2))
    (math/sqrt (+ (* (- x1 x2) (- x1 x2)) (* (- y1 y2) (- y1 y2))))))


# ==============================================================================
#  Reading the graph
# ==============================================================================

(defn- _count-sources-sinks
  "Source and sink count of a directed graph, as `@[sources sinks]`."
  [g]
  @[(length (dag/source-nodes g)) (length (dag/sink-nodes g))])

(defn- _incident-degrees
  "Structural degree per node."
  [g nodes-arr]
  (def result @{})
  (each n nodes-arr
    (put result n (structural-degree g n)))
  result)

# ==============================================================================
#  DAG-ification
# ==============================================================================

(defn- _bidirected-arc-count
  "How many arcs the DAG-ification builds before breaking and merging."
  [directed? entries]
  (* (if directed? 1 2) (length entries)))

# ==============================================================================
#  Network-simplex ranking (Gansner et al. 1993, section 2.3)
# ==============================================================================

(defn- _index-arcs
  "Arcs tagged with the canonical index of each endpoint."
  [arcs index]
  (map (fn [a]
         {:id (a :id)
          :from (a :from) :to (a :to)
          :from-idx (index (a :from)) :to-idx (index (a :to))
          :weight (a :weight)})
       arcs))

(defn- _compare-arcs
  "Canonical arc order: source, then target, then id."
  [a b]
  (cond
    (< (a :from-idx) (b :from-idx)) -1
    (> (a :from-idx) (b :from-idx)) 1
    (< (a :to-idx) (b :to-idx)) -1
    (> (a :to-idx) (b :to-idx)) 1
    (< (a :id) (b :id)) -1
    1))

(defn- _sort-arcs
  [arcs index]
  (sort-with (array/slice (_index-arcs arcs index)) _compare-arcs))

(defn- _outgoing-csr
  "CSR offsets over arc sources."
  [n arcs]
  (def start (array/new-filled (+ n 1) 0))
  (each a arcs
    (++ (start (+ (a :from-idx) 1))))
  (for i 0 n
    (+= (start (+ i 1)) (start i)))
  start)

(defn- _longest-path-levels
  "Longest-path levels via Kahn's algorithm with a min-heap ready queue."
  [n arcs out-start]
  (def indegree (array/new-filled n 0))
  (each a arcs (++ (indegree (a :to-idx))))
  (def ready (heap/make-binary-min-heap :compare (fn [a b] (< a b))))
  (each [i d] (pairs indegree)
    (when (= d 0) (heap/heap-push ready i)))
  (def levels (array/new-filled n 0))
  (var visited 0)
  (while (> (heap/heap-size ready) 0)
    (def u (heap/heap-pop ready))
    (++ visited)
    (for ei (out-start u) (out-start (+ u 1))
      (let [a (arcs ei)
            cand (+ (levels u) 1)
            to (a :to-idx)]
        (when (> cand (levels to)) (put levels to cand))
        (-- (indegree to))
        (when (= (indegree to) 0) (heap/heap-push ready to)))))
  (when (not= visited n) (error "input graph contains a directed cycle"))
  levels)

(defn- _simplex-balances
  "Net weight leaving each node; the total is checked to be zero."
  [n arcs]
  (def balance (array/new-filled n 0))
  (each a arcs
    (+= (balance (a :from-idx)) (a :weight))
    (-= (balance (a :to-idx)) (a :weight)))
  (when (not= (reduce + 0 balance) 0) (error "unbalanced rank supplies"))
  balance)

(defn- _pivot-order
  "Arc indices in ascending arc-id order."
  [arcs]
  (sort-with (array/slice (range (length arcs)))
             (fn [i j] (if (< ((arcs i) :id) ((arcs j) :id)) -1 1))))

(defn- _incidence-csr
  "Incidence offsets and arc-index list, filled in pivot order."
  [n arcs pivot-order]
  (def start (array/new-filled (+ n 1) 0))
  (each a arcs
    (++ (start (+ (a :from-idx) 1)))
    (++ (start (+ (a :to-idx) 1))))
  (for i 0 n
    (+= (start (+ i 1)) (start i)))
  (def edges (array/new-filled (* 2 (length arcs)) 0))
  (def pos (array/slice start))
  (each idx pivot-order
    (def a (arcs idx))
    (put edges (pos (a :from-idx)) idx)
    (++ (pos (a :from-idx)))
    (put edges (pos (a :to-idx)) idx)
    (++ (pos (a :to-idx))))
  @[start edges])

(defn- _arc-other
  "The endpoint of `a` other than `node`."
  [a node]
  (if (= (a :from-idx) node) (a :to-idx) (a :from-idx)))

(defn- _fill!
  "Set every element of a mutable array."
  [arr value]
  (for i 0 (length arr) (put arr i value)))

(defn- _rank-span
  [st a]
  (- ((st :levels) (a :to-idx)) ((st :levels) (a :from-idx))))

(defn- _rank-slack
  "Arc span in excess of one, rejecting an infeasible ranking."
  [st a]
  (def s (- (_rank-span st a) 1))
  (when (< s 0) (error "infeasible edge span"))
  s)

(defn- _rank-state
  "Mutable network-simplex state, updated in place by the stages below."
  [n arcs levels balance pivot-order]
  (def [inc-start inc-edges] (_incidence-csr n arcs pivot-order))
  (def in-tree (array/new-filled n false))
  (put in-tree 0 true)
  @{:n n
    :arcs arcs
    :levels levels
    :balance balance
    :pivot-order pivot-order
    :inc-start inc-start
    :inc-edges inc-edges
    :tree (array/new-filled (length arcs) false)
    :in-tree in-tree
    :queue @[0]
    :parent (array/new-filled n -2)
    :parent-edge (array/new-filled n 0)
    :cut-value (array/new-filled (length arcs) 0)
    :computed-balance (array/new-filled n 0)})

(defn- _tree-size
  [in-tree]
  (var size 0)
  (each present in-tree (when present (++ size)))
  size)

(defn- _grow-tight
  "Add every tight arc reachable from the queued tree boundary."
  [st]
  (def queue (st :queue))
  (def arcs (st :arcs))
  (def tree (st :tree))
  (def in-tree (st :in-tree))
  (def inc-start (st :inc-start))
  (def inc-edges (st :inc-edges))
  (var head 0)
  (while (< head (length queue))
    (def node (queue head))
    (++ head)
    (for i (inc-start node) (inc-start (+ node 1))
      (def idx (inc-edges i))
      (def a (arcs idx))
      (def other (_arc-other a node))
      (when (not (in-tree other))
        (when (= (_rank-slack st a) 0)
          (put tree idx true)
          (put in-tree other true)
          (array/push queue other)))))
  (array/clear queue))

(defn- _shift-to-boundary
  "Shift the tree toward the boundary by the smallest slack, then absorb
   every boundary arc the shift made tight."
  [st]
  (def arcs (st :arcs))
  (def tree (st :tree))
  (def in-tree (st :in-tree))
  (def levels (st :levels))
  (def queue (st :queue))
  (def boundary @[])
  (var best-slack 4611686018427387904)
  (each idx (st :pivot-order)
    (def a (arcs idx))
    (when (not= (in-tree (a :from-idx)) (in-tree (a :to-idx)))
      (def s (_rank-slack st a))
      (cond
        (< s best-slack)
        (do (set best-slack s) (array/clear boundary) (array/push boundary idx))
        (= s best-slack)
        (array/push boundary idx))))
  (when (= 0 (length boundary))
    (error "could not build a spanning feasible tree"))
  (def selected (arcs (boundary 0)))
  (def selected-tail (in-tree (selected :from-idx)))
  (def delta (if selected-tail best-slack (- 0 best-slack)))
  (each [node present] (pairs in-tree)
    (when present (put levels node (+ (levels node) delta))))
  (each idx boundary
    (def a (arcs idx))
    (when (not= (in-tree (a :from-idx)) (in-tree (a :to-idx)))
      (def tail (in-tree (a :from-idx)))
      (when (or (= best-slack 0) (= tail selected-tail))
        (when (= (_rank-slack st a) 0)
          (def outside (if tail (a :to-idx) (a :from-idx)))
          (put tree idx true)
          (put in-tree outside true)
          (array/push queue outside))))))

(defn- _feasible-tree
  "Grow a spanning tree of tight arcs, shifting levels as needed."
  [st]
  (_grow-tight st)
  (var tree-count (_tree-size (st :in-tree)))
  (while (< tree-count (st :n))
    (_shift-to-boundary st)
    (when (= 0 (length (st :queue)))
      (error "feasible-tree shift added no node"))
    (_grow-tight st)
    (set tree-count (_tree-size (st :in-tree)))))

(defn- _compute-cut-values
  "Root the tight tree at node 0 and compute every tree edge's cut value."
  [st]
  (def n (st :n))
  (def arcs (st :arcs))
  (def tree (st :tree))
  (def balance (st :balance))
  (def parent (st :parent))
  (def parent-edge (st :parent-edge))
  (def cut-value (st :cut-value))
  (def inc-start (st :inc-start))
  (def inc-edges (st :inc-edges))
  (var tree-edge-count 0)
  (each present tree (when present (++ tree-edge-count)))
  (when (not= tree-edge-count (- n 1))
    (error "basis has the wrong number of tree edges"))
  (_fill! parent -2)
  (put parent-edge 0 -1)
  (put parent 0 -1)
  (def order @[])
  (def stack @[0])
  (while (pos? (length stack))
    (def node (array/pop stack))
    (array/push order node)
    (for i (inc-start node) (inc-start (+ node 1))
      (def idx (inc-edges i))
      (when (and (tree idx) (not= idx (parent-edge node)))
        (def a (arcs idx))
        (def other (_arc-other a node))
        (when (not= (parent other) -2) (error "basis contains a cycle"))
        (put parent other node)
        (put parent-edge other idx)
        (array/push stack other))))
  (when (not= (length order) n) (error "basis is disconnected"))
  (def subtree (array/slice balance))
  (_fill! cut-value 0)
  (var i (- (length order) 1))
  (while (>= i 0)
    (def node (order i))
    (def eidx (parent-edge node))
    (when (>= eidx 0)
      (def a (arcs eidx))
      (def amount (subtree node))
      (put cut-value eidx
           (if (= (a :from-idx) node) amount (- 0 amount)))
      (+= (subtree (parent node)) amount))
    (-- i))
  (when (not= (subtree 0) 0) (error "tree balance is not zero")))

(defn- _head-component
  "Nodes on the far side of `leaving` in the rooted tight tree."
  [st leaving]
  (def n (st :n))
  (def arcs (st :arcs))
  (def tree (st :tree))
  (def inc-start (st :inc-start))
  (def inc-edges (st :inc-edges))
  (def head (array/new-filled n false))
  (def start (arcs leaving))
  (put head (start :to-idx) true)
  (def q @[(start :to-idx)])
  (var cursor 0)
  (while (< cursor (length q))
    (def node (q cursor))
    (++ cursor)
    (for i (inc-start node) (inc-start (+ node 1))
      (def idx (inc-edges i))
      (when (and (tree idx) (not= idx leaving))
        (def a (arcs idx))
        (def other (_arc-other a node))
        (when (not (head other))
          (put head other true)
          (array/push q other)))))
  (when (= (length q) n) (error "leaving edge does not cut the tree"))
  head)

(defn- _optimize
  "Pivot negative-cut tree edges toward optimality."
  [st]
  (def arcs (st :arcs))
  (def tree (st :tree))
  (def levels (st :levels))
  (def cut-value (st :cut-value))
  (while true
    (_compute-cut-values st)
    (var leaving nil)
    (each idx (st :pivot-order)
      (when (nil? leaving)
        (when (and (tree idx) (< (cut-value idx) 0))
          (set leaving idx))))
    (when (nil? leaving) (break))
    (def head (_head-component st leaving))
    (var min-slack nil)
    (var entering nil)
    (each idx (st :pivot-order)
      (def a (arcs idx))
      (when (and (not (tree idx))
                 (head (a :from-idx))
                 (not (head (a :to-idx))))
        (def s (_rank-slack st a))
        (when (or (nil? min-slack) (< s min-slack))
          (set min-slack s)
          (set entering idx))))
    (when (nil? entering)
      (error "negative cut tree edge has no entering edge"))
    (each [node present] (pairs head)
      (when present (+= (levels node) min-slack)))
    (when (not= (_rank-slack st (arcs entering)) 0)
      (error "entering edge is not tight after exchange"))
    (put tree leaving false)
    (put tree entering true)))

(defn- _rank-result
  "Check the final tree and build the ranking result."
  [st nodes]
  (def arcs (st :arcs))
  (def levels (st :levels))
  (def tree (st :tree))
  (def balance (st :balance))
  (def cut-value (st :cut-value))
  (def computed-balance (st :computed-balance))
  (_compute-cut-values st)
  (_fill! computed-balance 0)
  (var dual 0)
  (each [idx a] (pairs arcs)
    (var flow 0)
    (when (tree idx)
      (set flow (cut-value idx))
      (when (< flow 0) (error "final tree edge has negative cut value"))
      (when (not= (_rank-span st a) 1)
        (error "final tree edge is not tight")))
    (+= (computed-balance (a :from-idx)) flow)
    (-= (computed-balance (a :to-idx)) flow)
    (+= dual flow))
  (each [i want] (pairs balance)
    (when (not= (computed-balance i) want)
      (error "certificate balance mismatch")))
  (def min-level (reduce min 4611686018427387904 levels))
  (def max-level (reduce max (- 4611686018427387904) levels))
  (var primal 0)
  (each a arcs
    (def s (_rank-span st a))
    (when (< s 1) (error "infeasible edge span"))
    (+= primal (* (a :weight) s)))
  (when (not= primal dual) (error "optimality certificate failed"))
  (def levels-map @{})
  (each [i name] (pairs nodes)
    (put levels-map name (- (levels i) min-level)))
  {:levels levels-map
   :level-count (+ (- max-level min-level) 1)
   :primal primal
   :dual dual})

(defn- _rank
  "Minimize the weighted sum of edge spans over levels.

   Returns {:levels {name level} :level-count n :primal n :dual n}."
  [arcs nodes]
  (def nodes (canonical-nodes nodes))
  (def n (length nodes))
  (def index @{})
  (each [i name] (pairs nodes) (put index name i))
  (if (= 0 (length arcs))
    # no arcs: all nodes rest on a single level
    {:levels (merge @{} (seq [name :in nodes] {name 0}))
     :level-count 1
     :primal 0
     :dual 0}
    (let [sorted (_sort-arcs arcs index)
          out-start (_outgoing-csr n sorted)
          levels (_longest-path-levels n sorted out-start)
          balance (_simplex-balances n sorted)
          pivot-order (_pivot-order sorted)
          st (_rank-state n sorted levels balance pivot-order)]
      (_feasible-tree st)
      (_optimize st)
      (_rank-result st nodes))))


# ==============================================================================
#  Eligibility heuristics
# ==============================================================================

(defn- _is-1n1
  "Detects the 1 → N → 1 pattern, 3 levels, single source and sink, every
middle node connected above and below only.

`nb` is an undirected-neighbour accessor, [g node] -> node names."
  [g levels level-count nb]
  (if (not= level-count 3) false
    (do
      (def parts @[@[] @[] @[]])
      (each [name l] (pairs levels)
        (array/push (parts l) name))
      (def l0 (parts 0))
      (def l1 (parts 1))
      (def l2 (parts 2))
      (if (or (not= (length l0) 1) (not= (length l2) 1)) false
        (do
          (var ok true)
          (each node l1
            (var above false)
            (var below false)
            (each v (nb g node)
              (def vl (levels v))
              (cond
                (= vl 0) (set above true)
                (= vl 2) (set below true)
                (set ok false)))
            (when (or (not above) (not below)) (set ok false)))
          (when ok
            (each v (nb g (l0 0))
              (when (not= (levels v) 1) (set ok false))))
          ok)))))

(defn- _minimum-workflow-extent-fits
  "Upper-bound check used by the branched-workflow exception."
  [levels level-count horizontal? sizes]
  (def level-size (array/new-filled level-count 0))
  (each [name l] (pairs levels)
    (def sz (get sizes name @[_default-node-width _default-node-height]))
    (def size (if horizontal? (sz 0) (sz 1)))
    (when (> size (level-size l)) (put level-size l size)))
  (var extent (* (- level-count 1) (+ _crossing-spacing (* 2 _min-port-clearance))))
  (each size level-size (+= extent (math/ceil size)))
  (<= extent _max-graph-size))

(defn- _eligibility-reason
  "Returns nil if the ranked hierarchy is eligible, otherwise the first failing heuristic."
  [g rank-res directed? entries source-count sink-count nodes-arr horizontal?
   sizes]
  (def levels (rank-res :levels))
  (def level-count (rank-res :level-count))
  (def n (length nodes-arr))
  (def aspect (/ n (* level-count level-count)))
  (def dir @[0 0])
  (if directed?
    (each [u v] entries
      (if (> (levels v) (levels u)) (++ (dir 0)) (++ (dir 1))))
    (put dir 1 (length entries)))
  (var branched-workflow (and (> n level-count)
                              (= source-count 1)
                              (= sink-count 1)
                              (= (dir 1) 0)
                              (<= n _max-automatic-workflow-nodes)
                              (<= (+ (dir 0) (dir 1)) _max-automatic-workflow-edges)))
  (when (and (< aspect 0.5) branched-workflow
             (not (_minimum-workflow-extent-fits levels level-count horizontal? sizes)))
    (set branched-workflow false))
  (def max-edges (* 2 (math/ceil (math/sqrt n))))
  (def degs (_incident-degrees g nodes-arr))
  (def has-dense (truthy? (find (fn [node] (> (get degs node 0) max-edges)) nodes-arr)))
  (cond
    (< level-count _min-hierarchy-levels) "less-than-3-levels"
    (or (= source-count 0) (= sink-count 0)) "no-source-or-sink"
    (> aspect 2.0) "too-wide"
    (and (< aspect 0.5) (not branched-workflow)) "too-tall"
    (< (dir 0) (* 1.5 (dir 1))) "edges-balance"
    has-dense "dense-hub"
    (_is-1n1 g levels level-count (make-undirected-neighbours-fn g))
    "one-to-one-hierarchy"
    nil))

# ==============================================================================
#  Ordering: crossing minimization & global sifting
# ==============================================================================

# A placement node (pn) is a table: {:id name-or-neg-int, :node name/nil,
# :level, :rank, :aboves {id pn}, :belows {id pn}, :is-dummy, :is-chainning}.

(defn- _new-pn
  [id node-name level]
  @{:id id :node node-name :level level :rank 0
    :aboves @{} :belows @{} :is-dummy false :is-chainning false})

(defn- _pn-connect
  "below.aboves[above], above.belows[below]; ties by level sorted."
  [above below]
  (when (not= (above :level) (below :level))
    (put (below :aboves) (above :id) above)
    (put (above :belows) (below :id) below)))

(defn- _build-placement-nodes
  [nodes levels-map]
  (let [pns @[]
        pn-by-name @{}]
    (each name nodes
      (def pn (_new-pn name name (levels-map name)))
      (array/push pns pn)
      (put pn-by-name name pn))
    @[pns pn-by-name]))

(defn- _group-by-level
  [pns level-count]
  (def by-level @[])
  (for i 0 level-count (array/push by-level @[]))
  (each pn pns
    (array/push (by-level (pn :level)) pn))
  by-level)

(defn- _compute-level-ranks
  "Rank = array position (upstream computeLevelRanks, no containers)."
  [nodes]
  (each [i pn] (pairs nodes) (put pn :rank i)))

(defn- _cross-level-segments
  "Segments from each pn to its aboves/belows, as one flat array of
  [sx sy ex ey] quads (segment i at offsets 4i..4i+3)."
  [nodes aboves? belows?]
  (def segs @[])
  (each pn nodes
    (def sx (pn :rank))
    (def sy (pn :level))
    (when aboves?
      (each [_ c] (pairs (pn :aboves))
        (array/push segs sx) (array/push segs sy)
        (array/push segs (c :rank)) (array/push segs (c :level))))
    (when belows?
      (each [_ c] (pairs (pn :belows))
        (array/push segs sx) (array/push segs sy)
        (array/push segs (c :rank)) (array/push segs (c :level)))))
  segs)

(defn- _segments-cross
  "layoutgraph.SegmentsCross on flat segment coordinates."
  [u0x u0y u1x u1y v0x v0y v1x v1y]
  (let [denom (- (* (- u1y u0y) (- v1x v0x)) (* (- u1x u0x) (- v1y v0y)))
        s (/ (- (* (- v1x v0x) (- v0y u0y)) (* (- v1y v0y) (- v0x u0x))) denom)
        t (/ (- (* (- u1x u0x) (- v0y u0y)) (* (- u1y u0y) (- v0x u0x))) denom)]
    (and (not= denom 0) (>= s 0) (<= s 1) (>= t 0) (<= t 1))))

(defn- _count-crossings
  "Count edge crossings. Pairwise, bbox precheck, equal starts/ends skipped."
  [segs]
  (var crossings 0)
  (def n (/ (length segs) 4))
  (def min-xs (array/new-filled n 0)) (def max-xs (array/new-filled n 0))
  (def min-ys (array/new-filled n 0)) (def max-ys (array/new-filled n 0))
  (for i 0 n
    (def b (* 4 i))
    (def sx (get segs b))
    (def sy (get segs (+ b 1)))
    (def ex (get segs (+ b 2)))
    (def ey (get segs (+ b 3)))
    (put min-xs i (min sx ex)) (put max-xs i (max sx ex))
    (put min-ys i (min sy ey)) (put max-ys i (max sy ey)))
  (var max-min-x 0)
  (for i 0 n (when (> (min-xs i) max-min-x) (set max-min-x (min-xs i))))
  (def counts (array/new-filled (+ max-min-x 1) 0))
  (for i 0 n (++ (counts (min-xs i))))
  (var acc 0)
  (for b 0 (length counts)
    (def c (counts b))
    (put counts b acc)
    (set acc (+ acc c)))
  (def order (array/new-filled n 0))
  (for i 0 n
    (def b (min-xs i))
    (put order (counts b) i)
    (put counts b (+ 1 (counts b))))
  (for oi 0 (- n 1)
    (def i (order oi))
    (def bi (* 4 i))
    (def isx (get segs bi))
    (def isy (get segs (+ bi 1)))
    (def iex (get segs (+ bi 2)))
    (def iey (get segs (+ bi 3)))
    (def min-x (min-xs i)) (def max-x (max-xs i))
    (def min-y (min-ys i)) (def max-y (max-ys i))
    (var oj (inc oi))
    (while (< oj n)
      (def j (order oj))
      (when (> (min-xs j) max-x) (break))
      (def bj (* 4 j))
      (def jsx (get segs bj))
      (def jsy (get segs (+ bj 1)))
      (def jex (get segs (+ bj 2)))
      (def jey (get segs (+ bj 3)))
      (def x-overlap (and (> (max-xs j) min-x) (<= (min-xs j) max-x)))
      (def y-overlap (and (> (max-ys j) min-y) (<= (min-ys j) max-y)))
      (when (and x-overlap y-overlap
                 (not (or (and (= isx jsx) (= isy jsy))
                          (and (= iex jex) (= iey jey)))))
        (when (_segments-cross isx isy iex iey jsx jsy jex jey)
          (++ crossings)))
      (++ oj)))
  crossings)

(defn- _split-level-segments
  "_cross-level-segments for the both-directions case, as two flat arrays."
  [nodes]
  (var n-above 0)
  (var n-below 0)
  (each pn nodes
    (set n-above (+ n-above (length (pn :aboves))))
    (set n-below (+ n-below (length (pn :belows)))))
  (def above (array/new-filled (* 4 n-above) 0))
  (def below (array/new-filled (* 4 n-below) 0))
  (var ai 0)
  (var bi 0)
  (each pn nodes
    (def sx (pn :rank))
    (def sy (pn :level))
    (each [_ c] (pairs (pn :aboves))
      (put above ai sx) (put above (+ ai 1) sy)
      (put above (+ ai 2) (c :rank)) (put above (+ ai 3) (c :level))
      (set ai (+ ai 4)))
    (each [_ c] (pairs (pn :belows))
      (put below bi sx) (put below (+ bi 1) sy)
      (put below (+ bi 2) (c :rank)) (put below (+ bi 3) (c :level))
      (set bi (+ bi 4))))
  [above below])

(defn- _count-crossings-split
  "_count-crossings of a level's above/below segment groups, counted separately."
  [above-segs below-segs]
  (+ (_count-crossings above-segs)
     (_count-crossings below-segs)))

(defn- _add-lengths
  "Sum of segment Euclidean lengths (order-independent)."
  [segs]
  (var total 0.0)
  (var i 0)
  (def n (/ (length segs) 4))
  (while (< i n)
    (def b (* 4 i))
    (set total (+ total
                  (_euclidean-distance
                   (get segs b) (get segs (+ b 1))
                   (get segs (+ b 2)) (get segs (+ b 3)))))
    (++ i))
  total)

(defn- _add-lengths-split
  "_add-lengths over a level's two segment groups."
  [above-segs below-segs]
  (+ (_add-lengths above-segs)
     (_add-lengths below-segs)))

(defn- _sort-level-by-adjacency
  "Sort nodes by the average rank of connections toward the optimization direction."
  [nodes use-aboves]
  (defn avg [pn]
    (def conns (if use-aboves (pn :aboves) (pn :belows)))
    (if (empty? conns)
      0
      (/ (reduce + 0 (map (fn [[_ c]] (c :rank)) (pairs conns)))
         (length conns))))
  (sort-with nodes (fn [a b]
    (def aa (avg a))
    (def bb (avg b))
    (cond
      (< aa bb) -1
      (> aa bb) 1
      (< (a :rank) (b :rank)) -1
      (> (a :rank) (b :rank)) 1
      0))))

(defn- _measure-level
  "Crossings and total length of a level as @[crossings length]."
  [nodes]
  (def [sa sb] (_split-level-segments nodes))
  @[(_count-crossings-split sa sb) (_add-lengths-split sa sb)])

(defn- _best-index-by-swapping
  "Try swaps with the next 3 neighbors (circular).
Returns @[best-crossings best-length best-index] without changing positions."
  [nodes j by-level]
  (var best @[nil nil 0])
  (def level ((nodes j) :level))
  (loop [i :range [(+ j 1) (+ j 4)]]
    (def k (% i (length nodes)))
    (_swap-arr nodes j k)
    (_compute-level-ranks (by-level level))
    (def [crossings len] (_measure-level nodes))
    (_swap-arr nodes j k)
    (when (or (nil? (best 0))
              (< crossings (best 0))
              (and (= crossings (best 0))
                   (< len (- (best 1) _precision))))
      (set best @[crossings len k])))
  best)

(defn- _minimize-crossings
  "Gansner vertex ordering within a level."
  [nodes by-level top-down?]
  (when (pos? (length nodes))
    (def level ((nodes 0) :level))
    (_sort-level-by-adjacency nodes top-down?)
    (_compute-level-ranks (by-level level))
    (var measured (_measure-level nodes))
    (for j 0 (length nodes)
      (def candidate (_best-index-by-swapping nodes j by-level))
      (when (or (< (candidate 0) (measured 0))
                (and (= (candidate 0) (measured 0))
                     (< (candidate 1) (- (measured 1) _precision))))
        (set measured candidate)
        (_swap-arr nodes j (candidate 2)))
      (_compute-level-ranks (by-level level)))))

(defn- _minimize-hierarchy-crossings
  [by-level]
  (def widest (reduce max 0 (map length by-level)))
  (for iter 0 widest
    (for level 0 (length by-level)
      (when (pos? (length (by-level level)))
        (_minimize-crossings (by-level level) by-level (> level 0))))))

(defn- _band-crossings
  "Between-level crossing metric. For every level > 0, count crossings of
segments from its nodes to their aboves."
  [by-level]
  (reduce (fn [total level]
    (+ total (_count-crossings (_cross-level-segments (by-level level) true false))))
    0 (range 1 (length by-level))))

(defn- _improved
  [crossings best-crossings length best-length allow-equal]
  (if allow-equal
    (<= crossings best-crossings)
    (or (< crossings best-crossings)
        (and (= crossings best-crossings)
             (< length (- best-length _precision))))))

(defn- _sift
  "Matuszewski single-node sifting within its sibling level."
  [node siblings by-level allow-equal]
  (def level (node :level))
  (def sn (length siblings))
  (def initial (_measure-level siblings))
  (var best @[(initial 0) (initial 1) nil])
  (defn step [i j new-pos]
    (_swap-arr siblings i j)
    (_compute-level-ranks (by-level level))
    (def measured (_measure-level siblings))
    (when (_improved (measured 0) (best 0) (measured 1) (best 1) allow-equal)
      (put best 0 (measured 0))
      (put best 1 (measured 1))
      (put best 2 new-pos)))
  (when (and (pos? (best 0)) (> sn 1))
    (if (= node (siblings (- sn 1)))
      (do (put best 2 (- sn 1)))
      (do
        (var i 0)
        (while (< i (- sn 1))
          (when (= (siblings i) node) (break))
          (++ i))
        (put best 2 i)
        (var j (+ i 1))
        (while (< j sn)
          (step (- j 1) j j)
          (++ j))
        (when (not= (siblings (- sn 1)) node)
          (error "sifting: node did not reach the right end"))))
    (var idx (- sn 1))
    (while (> idx 0)
      (step (- idx 1) idx (- idx 1))
      (-- idx))
    (when (not= (siblings 0) node)
      (error "sifting: node did not reach the left end"))
    (var i 0)
    (while (and (< i sn) (not= i (best 2)))
      (_swap-arr siblings (+ i 1) i)
      (++ i))
    (_compute-level-ranks (by-level level))))

(defn- _global-sifting
  "Matuszewski global sifting. Repeatedly sweep every node until nothing moves."
  [by-level degrees max-iterations]
  (def queue (array/concat @[] ;by-level))
  (sort-with queue (fn [a b]
    (def da (if (a :is-dummy) 2 (get degrees (a :node) 0)))
    (def db (if (b :is-dummy) 2 (get degrees (b :node) 0)))
    (cond
      (> da db) -1
      (< da db) 1
      (< (a :level) (b :level)) -1
      (> (a :level) (b :level)) 1
      (< (a :rank) (b :rank)) -1
      (> (a :rank) (b :rank)) 1
      0)))
  (def node-siblings @{})
  (each pn queue
    (put node-siblings (pn :id) (by-level (pn :level))))

  (defn sweep [allow-equal]
    (var improved false)
    (each node (array/slice queue)
      (def old-rank (node :rank))
      (_sift node (node-siblings (node :id)) by-level allow-equal)
      (when (not= (node :rank) old-rank) (set improved true)))
    improved)

  (var allow-equal false)
  (for iter 0 max-iterations
    (if (sweep allow-equal)
      (set allow-equal false)
      (if allow-equal (break) (set allow-equal true)))))

# ==============================================================================
#  Long-edge splitting, placement, control points
# ==============================================================================

(defn- _break-long-connections
  "Split arcs spanning > 1 level with dummy chain nodes."
  [pns by-level]
  (def chains @{})
  (var dummy-counter 0)
  (each pn pns
    (def belows @[])
    (each [_ below] (pairs (pn :belows))
      (when (> (- (below :level) (pn :level)) 1)
        (array/push belows below)))
    (sort-with belows (fn [a b]
      (cond
        (< (a :rank) (b :rank)) -1
        (> (a :rank) (b :rank)) 1
        (< (a :level) (b :level)) -1
        1)))
    (each below belows
      (put (pn :belows) (below :id) nil)
      (put (below :aboves) (pn :id) nil)
      (var above pn)
      (def chain @[])
      (for level (+ (pn :level) 1) (below :level)
        (-- dummy-counter)
        (def dummy (_new-pn dummy-counter nil level))
        (put dummy :is-dummy true)
        (put dummy :is-chainning true)
        (put dummy :rank (length (by-level level)))
        (_pn-connect above dummy)
        (array/push chain dummy)
        (array/push (by-level level) dummy)
        (set above dummy))
      (_pn-connect above below)
      (when (pos? (length chain))
        (put chains (tuple (pn :node) (below :node)) chain))))
  chains)

(defn- _scaled-node-sizes
  "Complete {name -> [w h]} map."
  [nodes-arr node-sizes horizontal?]
  (def out @{})
  (each name nodes-arr
    (def sz (get node-sizes name @[_default-node-width _default-node-height]))
    (def w (if (number? (sz 0)) (sz 0) _default-node-width))
    (def h (if (number? (sz 1)) (sz 1) _default-node-height))
    (put out name (if horizontal? @[h w] @[w h])))
  out)

(defn- _assign-columns
  "Place one level's nodes left to right at `y-offset`.
Returns band height, i.e.  tallest node on the level."
  [level-nodes sizes y-offset]
  (var x-offset 0.0)
  (var band-height 0.0)
  (each pn level-nodes
    (def w (if (pn :is-dummy) 1.0 (get (sizes (pn :node)) 0)))
    (def h (if (pn :is-dummy) 1.0 (get (sizes (pn :node)) 1)))
    (put pn :tx x-offset)
    (put pn :ty y-offset)
    (put pn :w w)
    (put pn :h h)
    (when (> h band-height) (set band-height h))
    (if (pn :is-dummy)
      (+= x-offset _sibling-spacing)
      (+= x-offset (math/ceil w) _sibling-spacing)))
  band-height)

(defn- _centre-level
  "Centre a level vertically within its band."
  [level-nodes band-top band-height]
  (def centre-y (math/ceil (+ band-top (/ band-height 2))))
  (each pn level-nodes
    (def w (pn :w))
    (def h (pn :h))
    (put pn :cx (+ (pn :tx) (/ w 2)))
    (def cy-raw (+ (pn :ty) (/ h 2)))
    (put pn :cy (+ cy-raw (math/ceil (- centre-y cy-raw))))))

(defn- _place-nodes-by-level
  "Assign coordinates (centres) to every pn.
   Returns {:top-y .. :level-height ..}."
  [by-level sizes]
  (def n-levels (length by-level))
  (def top-y (array/new-filled n-levels 0))
  (def level-height (array/new-filled n-levels 0))
  (var y-offset 0.0)
  (for i 0 n-levels
    (def level-nodes (by-level i))
    (def band-height (_assign-columns level-nodes sizes y-offset))
    (put level-height i band-height)
    (put top-y i y-offset)
    (def segs (_cross-level-segments level-nodes false true))
    (def crossings (_count-crossings segs))
    (def spread (max (* crossings _crossing-spacing) _crossing-spacing))
    (def distance (math/round (min spread _parent-spacing)))
    (+= y-offset
        (math/ceil (+ band-height distance (* 2 _min-port-clearance)))))
  (for i 0 n-levels
    (_centre-level (by-level i) (top-y i) (level-height i)))
  {:top-y top-y :level-height level-height})

(defn- _transform
  "Direction transform from placement space (TB flow, y down) to output."
  [x y direction]
  (case direction
    :tb @[x y]
    :bt @[x (- y)]
    :lr @[y x]
    :rl @[(- y) (- x)]
    @[x y]))

(defn- _arc-control-points
  "Orthogonal staircase for a span>1 arc through its dummy chain."
  [from-pn to-pn chains top-y level-height]
  (let [cps @[]
        chain (get chains (tuple (from-pn :node) (to-pn :node)))]
    (when chain
      (var cur-x (from-pn :cx))
      (for k (from-pn :level) (to-pn :level)
        (def y (+ (top-y k) (level-height k)))
        (array/push cps @[cur-x y])
        (def next-x (if (< k (- (to-pn :level) 1))
                       ((chain (- k (from-pn :level))) :cx)
                       (to-pn :cx)))
        (array/push cps @[next-x y])
        (set cur-x next-x))
      (array/push cps @[cur-x (to-pn :cy)]))
    cps))

(defn- _node-positions
  "Centres in output orientation, plus each node's rank within its level."
  [pns direction]
  (def pos @{})
  (def node-index @{})
  (each pn pns
    (put pos (pn :node) (_transform (pn :cx) (pn :cy) direction))
    (put node-index (pn :node) (pn :rank)))
  @[pos node-index])

(defn- _build-control-points
  "Control points for every arc that was split into a dummy chain."
  [simple-arcs levels pn-by-name chains top-y level-height direction]
  (def control-points @{})
  (each a simple-arcs
    (def span (- (levels (a :to)) (levels (a :from))))
    (when (> span 1)
      (def cps (_arc-control-points
                 (pn-by-name (a :from)) (pn-by-name (a :to))
                 chains top-y level-height))
      (when (pos? (length cps))
        (put control-points (tuple (a :from) (a :to))
             (seq [p :in cps] (_transform (p 0) (p 1) direction))))))
  control-points)


# ==============================================================================
#  Layout entry point
# ==============================================================================


(defn- _dag-eligible-or-layout
  "Apply validity heuristics, then run ordering and placement."
  [g opts force direction horizontal? nodes entries source-count sink-count
   simple-arcs rank-res]
  (def node-sizes (get opts :node-sizes @{}))
  (def reason
    (when (not force)
      (_eligibility-reason g rank-res (structurally-directed? g) entries
                           source-count sink-count nodes horizontal?
                           (_scaled-node-sizes nodes node-sizes false))))
  (if reason
    {:eligible false :reason reason}
    (let [levels (rank-res :levels)
          level-count (rank-res :level-count)
          sizes (_scaled-node-sizes nodes node-sizes horizontal?)
          [pns pn-by-name] (_build-placement-nodes nodes levels)
          by-level (_group-by-level pns level-count)
          max-iterations (get opts :max-iterations _max-sift-iterations)
          re-rank (fn [] (each level-nodes by-level
                           (_compute-level-ranks level-nodes)))]
      #  connect arcs to placement nodes. each pn is connected above and below
      (each a simple-arcs
        (_pn-connect (pn-by-name (a :from)) (pn-by-name (a :to))))
      (re-rank)
      #  replace every arc spanning more than one level with a chain
      (def chains (_break-long-connections pns by-level))
      (re-rank)
      #  order each level, greedily, to reduce crossings
      (_minimize-hierarchy-crossings by-level)
      (re-rank)
      (def crossings-initial (_band-crossings by-level))
      #  sifting. improves on greedy order by moving single nodes
      (_global-sifting by-level (_incident-degrees g nodes) max-iterations)
      (re-rank)
      (def crossings-after (_band-crossings by-level))
      #  assign coordinates
      (def {:top-y top-y :level-height level-height}
        (_place-nodes-by-level by-level sizes))
      (def [pos node-index] (_node-positions pns direction))
      #  control points for the arcs that were split
      (def control-points
        (_build-control-points simple-arcs levels pn-by-name chains
                               top-y level-height direction))
      {:eligible true
       :pos pos
       :levels levels
       :node-index node-index
       :level-count level-count
       :control-points control-points
       :primal (rank-res :primal)
       :dual (rank-res :dual)
       :crossings-initial crossings-initial
       :crossings-after crossings-after})))

(defn- _dag-pipeline
  "DAG layout and rejection pipeline."
  [g opts force nodes]
  (def direction (keyword (string (get opts :direction "tb"))))
  (def max-edges (get opts :max-edges _max-engine-edges))
  (def horizontal? (or (= direction :lr) (= direction :rl)))
  (def is-directed (structurally-directed? g))
  (def entries (structural-edges-for g is-directed))
  (def arc-count (_bidirected-arc-count is-directed entries))
  (def [source-count sink-count] (if is-directed (_count-sources-sinks g) @[0 0]))
  (def rejection
    (some (fn [check] (when ((check 0)) (check 1)))
          @[[(fn [] (and (not force) (not is-directed))) "no-source-or-sink"]
            [(fn [] (> (length entries) max-edges)) "too-large"]
            [(fn [] (not (connectivity/graph-connected? g))) "disconnected"]
            [(fn [] (> arc-count max-edges)) "too-large"]]))
  (if rejection
    {:eligible false :reason rejection}
    (let [simple-arcs (try (dag/dagify g) ([e] nil))
          rank-res (if simple-arcs (try (_rank simple-arcs nodes) ([e] nil)) nil)]
      (if (nil? rank-res)
        {:eligible false :reason "rank-failed"}
        (_dag-eligible-or-layout g opts force direction horizontal? nodes
                                 entries source-count sink-count
                                 simple-arcs rank-res)))))


(define layout-dag
  ```Compute a hierarchical layout of a directed or weighted graph.

  &keys
    :direction — "tb" (default), "bt", "lr", "rl"

    :force — skip the eligibility heuristics and layout the graph

    :node-sizes — {node [width height]} in dots (default 150x60)

    :max-nodes, :max-edges — engine limits (defaults 10000, 50000)

    :max-iterations — global-sifting sweeps (default 10)

  Returns
        {:eligible true
         :pos {node [x y]}
         :levels {node level}
         :node-index {node rank}
         :level-count n
         :control-points {(tuple a b) [[x y] ...]}
         :primal n
         :dual n
         :crossings-initial n
         :crossings-after n}

  or {:eligible false :reason "..."} when the graph can't be laid out.
```
  [g :weighted & keys]
  (def opts (table ;keys))
  (def force (truthy? (get opts :force false)))
  (def nodes (canonical-nodes (record/nodes g)))
  # simple validity checks
  (def rejected
    (or
      (when (<= (length nodes) 1) "empty-or-singleton")
      (when (> (length nodes)
               (get opts :max-nodes _max-engine-nodes)) "too-large")))
  (if rejected
    {:eligible false :reason rejected}
    (_dag-pipeline g opts force nodes)))
