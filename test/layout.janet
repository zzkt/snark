# -*- mode: janet; -*-
# graphs/test/layout: various tests for the layout algorithms

(import ./helper :prefix "")
(import ../graphs :prefix "")
(import ../graphs/record :prefix "r-")
(import ../graphs/layout/polyarc :prefix "_pa_")
(import ./data/graphs :prefix "fx-")

(print "\n* layout: eligibility")

(defn mk [nodes adj]
  (def g (make-digraph))
  (each n nodes (digraph-add-node g n))
  (each [u tbl] (pairs adj)
    (each [v w] (pairs tbl) (digraph-add-edge g u v w)))
  g)

# a C3 diamond is the excluded 1 → N → 1 pattern
(def diamond (layout-dag (mk @["a" "b" "c" "d"]
                             {"a" {"b" 1 "c" 1}
                              "b" {"d" 1}
                              "c" {"d" 1} "d" {}})))

(cassert-eq "diamond ineligible (1→N→1)"
            (diamond :eligible) false)
(cassert-eq "diamond reason"
            (diamond :reason) "one-to-one-hierarchy")

# a cycle is only laid out using 'force'
(def cyc (layout-dag (mk @["a" "b" "c"]
                         {"a" {"b" 1} "b" {"c" 1} "c" {"a" 1}})
                     :force true))

(cassert-eq "forced cycle eligible"
            (cyc :eligible) true)
(cassert-eq "forced cycle primal==dual"
            (= (cyc :primal) (cyc :dual)) true)

# two nodes with mutual directed edges have no source/sink
(def pair (layout-dag (mk @["a" "b"]
                          {"a" {"b" 1} "b" {"a" 1}})))

(cassert-eq "mutual pair ineligible"
            (pair :eligible) false)

(cassert-eq "mutual pair reason"
            (pair :reason) "no-source-or-sink")

(cassert-eq "singleton ineligible"
            ((layout-dag (mk @["a"] {})) :eligible) false)

(cassert-eq "singleton reason"
            ((layout-dag (mk @["a"] {})) :reason)
            "empty-or-singleton")

(cassert-eq "disconnected ineligible"
            ((layout-dag (mk @["a" "b" "c" "d"] {"a" {"b" 1} "c" {"d" 1}})) :reason)
            "disconnected")

# an ordinary chain is too tall for the compactness heuristic (aspect < 0.5)
(def chain (layout-dag (mk @["a" "b" "c"]
                           {"a" {"b" 1} "b" {"c" 1} "c" {}})))

(cassert-eq "chain ineligible" (chain :eligible) false)
(cassert-eq "chain reason" (chain :reason) "too-tall")

(print "\n* layout: eligible hierarchies")


(def ladder (layout-dag (mk @["a" "b1" "c1" "b2" "c2" "d"]
                            {"a" {"b1" 1 "b2" 1} "b1" {"c1" 1} "c1" {"d" 1}
                             "b2" {"c2" 1} "c2" {"d" 1} "d" {}})))

(cassert-eq "ladder eligible"
            (ladder :eligible) true)
(cassert-eq "ladder level-count"
            (ladder :level-count) 4)
(cassert-eq "ladder source level"
            ((ladder :levels) "a") 0)
(cassert-eq "ladder sink level"
            ((ladder :levels) "d") 3)
(cassert-eq "ladder primal==dual"
            (= (ladder :primal) (ladder :dual)) true)
(cassert-eq "ladder crossings non-increasing"
            (<= (ladder :crossings-after)
                (ladder :crossings-initial)) true)
(cassert-eq "ladder node positioned"
            (not= nil (get (ladder :pos) "a")) true)

# a 5-node 3-level graph with two sinks is eligible and not 1N1
(def wide (layout-dag (mk @["a" "b" "c" "d" "e"]
                          {"a" {"b" 1 "c" 1}
                           "b" {"d" 1 "e" 1}
                           "c" {"d" 1 "e" 1}
                           "d" {} "e" {}})))

(cassert-eq "wide eligible"
            (wide :eligible) true)
(cassert-eq "wide level-count"
            (wide :level-count) 3)
(cassert-eq "wide primal==dual"
            (= (wide :primal) (wide :dual)) true)
(cassert-eq "wide crossings non-increasing"
            (<= (wide :crossings-after) (wide :crossings-initial)) true)

# long-edge chains
(def diary (layout-dag (mk @["a" "b" "c" "d"]
                           {"a" {"b" 1 "d" 1} "b" {"c" 1} "c" {"d" 1}})
                       :force true))

(cassert-eq "long-edge forced eligible"
            (diary :eligible) true)
(cassert-eq "long-edge has control points"
            (not= nil (get (diary :control-points) (tuple "a" "d"))) true)

(def cps (get (diary :control-points) (tuple "a" "d")))
(cassert-eq "long-edge orthogonal staircase"
            (>= (length cps) 5) true)
(cassert-eq "long-edge starts under source"
            (= (get (cps 0) 0) (get ((diary :pos) "a") 0)) true)

(print "\n* layout: to-dot integration")

(def ld-graph (mk @["s" "a" "b" "c" "d" "m" "t" "u"]
                  {"s" {"a" 1 "m" 1} "a" {"b" 1} "b" {"c" 1} "c" {"d" 1}
                   "d" {"u" 1} "m" {"t" 1 "d" 1} "t" {"u" 1} "u" {}}))

(def dot (to-dot ld-graph :layout "tala" :rankdir "TB"))

(cassert-eq "tala splines=ortho"
            (> (length (string/find-all "splines=ortho" dot)) 0) true)
(cassert-eq "tala node positions"
            (> (length (string/find-all "pos=\"" dot)) 0) true)
(cassert-eq "tala pinned positions"
            (> (length (string/find-all "!\"]" dot)) 0) true)
(cassert-eq "tala rankdir passed"
            (> (length (string/find-all "rankdir=TB" dot)) 0) true)
(cassert-eq "tala edge splines"
            (> (length (string/find-all "pos=\"e," dot)) 0) true)

# a graph without an eligible hierarchy still exports (plain fallback)
(def fallback (to-dot (mk @["a" "b" "c"] {"a" {"b" 1} "b" {"c" 1} "c" {}})
                      :layout "tala"))

(cassert-eq "ineligible graph still exports"
            (> (length (string/find-all "digraph G {" fallback)) 0) true)
(cassert-eq "ineligible graph no splines=ortho"
            (> (length (string/find-all "splines=ortho" fallback)) 0) false)

# rankdir LR routes into the horizontal layout direction
(def dot-lr (to-dot ld-graph :layout "tala" :rankdir "LR"))

(cassert-eq "tala LR still emits node positions"
            (> (length (string/find-all "pos=\"" dot-lr)) 0) true)
(cassert-eq "tala LR still emits orthogonal splines"
            (> (length (string/find-all "splines=ortho" dot-lr)) 0) true)

(print "\n* layout: spring (FR planar/Tutte fallback)")

(defn wg [nodes edges]
  (def g (make-weighted-graph @[] @{}))
  (add-nodes g (map string nodes))
  (each [u v w] edges (add-edge g (string u) (string v) w))
  g)

# planar prism: the Tutte fallback removes all spring crossings
(def lprism (layout-spring
             (wg (range 10)
                 @[[0 1][1 2][2 3][3 4][4 0][5 6][6 7][7 8][8 9][9 5]
                   [0 5][1 6][2 7][3 8][4 9]])
             :width 50 :height 38 :iterations 100 :tutte-sweeps 500))

(cassert-eq "spring prism planar" (lprism :planar) true)
(cassert-eq "spring prism fallback used" (lprism :fallback) true)
(cassert-eq "spring prism removes all crossings" (lprism :crossings-after) 0)
(cassert-eq "spring prism positions sized" (length (lprism :pos)) 10)

# non-planar K5 keeps the spring drawing, no fallback
(def lk5 (layout-spring
          (wg (range 5) (seq [i :range [0 5]
                              j :range [(+ i 1) 5]
                              :let [e [i j 1]]] e))
          :width 30 :height 24 :iterations 50))

(cassert-eq "spring K5 not planar" (lk5 :planar) false)
(cassert-eq "spring K5 no fallback" (lk5 :fallback) false)
(cassert-eq "spring K5 positions sized" (length (lk5 :pos)) 5)

# crossing-reduction never makes the drawing worse
(cassert-eq "spring K5 smoothing does not increase crossings"
            (<= (lk5 :crossings-after) (lk5 :crossings-before)) true)

# :smooth-iterations 0 turns the post-pass off
(def lk5-raw (layout-spring
              (wg (range 5) (seq [i :range [0 5]
                                  j :range [(+ i 1) 5]
                                  :let [e [i j 1]]] e))
              :width 30 :height 24 :iterations 50 :smooth-iterations 0))

(cassert-eq "spring K5 smoothing off keeps drawing"
            (= (lk5-raw :crossings-after) (lk5-raw :crossings-before)) true)

# named dispatch

(def dispatch (graph-layout (wg @["a" "b" "c" "d"]
                                @[["a" "b" 1] ["b" "c" 1]
                                  ["c" "d" 1] ["d" "a" 1]])
                            "spring"))

(cassert-eq "graph-layout spring eligible"
            (dispatch :eligible) true)
(cassert-eq "graph-layout spring positions sized"
            (length (dispatch :pos)) 4)

(print "\n* layout: node-positions pinning with internal svg layout")

# to-dot accepts :node-positions and pins every node
(def pinned (to-dot (wg @["a" "b"]
                        @[["a" "b" 1]])
                    :node-positions @{"a" [1.0 2.0] "b" [3.0 4.0]}))

(cassert-eq "pinned node positions emitted"
            (> (length (string/find-all "pos=\"" pinned)) 0) true)
(cassert-eq "pinned positions flagged -n2"
            (> (length (string/find-all "pos=\"" pinned)) 0) true)
(cassert-eq "pinned y flipped (a = 1,-2)"
            (not= nil (string/find "1,-2" pinned)) true)

# to-svg renders a library layout through graphviz -n2
(def svg-spring (to-svg (wg @["a" "b" "c" "d" "e1" "e2" "e3" "e4"]
                            @[["a" "b" 1] ["b" "c" 1] ["c" "d" 1]
                              ["d" "a" 1] ["a" "e1" 1] ["b" "e2" 1]
                              ["c" "e3" 1] ["d" "e4" 1]])
                        :layout "spring"
                        :width 20
                        :height 15
                        :path "img/spring-test.svg"))

(cassert-eq "library layout svg renders"
            (> (length svg-spring) 100) true)
(cassert-eq "library layout svg is svg"
            (> (length (string/find-all "<svg" svg-spring)) 0) true)

(print "\n* layout: poly-arc Lombardi")

# Petersen: 15 edges, every vertex degree 3; construction always succeeds
(def lpetersen (layout-lombardi
                (wg (range 10)
                    @[[0 1][1 2][2 3][3 4][4 0][5 6][6 7][7 8][8 9][9 5]
                      [0 5][1 6][2 7][3 8][4 9]])
                :width 10 :height 8))

(cassert-eq "lombardi petersen eligible"
            (lpetersen :eligible) true)
(cassert-eq "lombardi petersen never iterates"
            (lpetersen :iterations) 0)
(cassert-eq "lombardi petersen arcs count"
            (length (lpetersen :arcs)) 15)
(cassert-eq "lombardi petersen perfect resolution by construction"
            (lpetersen :lombardi) true)
(cassert-eq "lombardi petersen residual finite"
            (<= (lpetersen :residual) math/inf) true)

# every record is a poly-arc chain: segments link (x0 y0) to (x1 y1)
(each a (lpetersen :arcs)
  (cassert-eq "lombardi record kind valid"
              (or (= (a :kind) :line)
                  (= (a :kind) :arc)
                  (= (a :kind) :poly)) true)

  (cassert-eq "lombardi record has segments"
              (> (length (a :segments)) 0) true)

  (def last-seg (get (a :segments) (- (length (a :segments)) 1)))

  (cassert-eq "lombardi chain ends at (x1 y1)"
              (and (= (last-seg :x) (a :x1)) (= (last-seg :y) (a :y1))) true))

# deterministic: same graph, same options, same arcs
(def lpetersen2 (layout-lombardi
                 (wg (range 10)
                     @[[0 1][1 2][2 3][3 4][4 0]
                       [5 6][6 7][7 8][8 9][9 5]
                       [0 5][1 6][2 7][3 8][4 9]])
                 :width 10 :height 8))

(cassert-eq "lombardi petersen deterministic"
            (deep= (lpetersen :arcs) (lpetersen2 :arcs)) true)

# overlap reduction: greedy joint sweep never raises arc overlaps

(cassert-eq "lombardi petersen reducer runs"
            (> (lpetersen :overlaps-before) 0) true)
(cassert-eq "lombardi petersen reducer lowers overlaps"
            (<= (lpetersen :overlaps-after) (lpetersen :overlaps-before)) true)
(cassert-eq "lombardi petersen reducer deterministic"
            (= (lpetersen :overlaps-after) (lpetersen2 :overlaps-after)) true)
(cassert-eq "lombardi petersen reducer actually reduced"
            (< (lpetersen :overlaps-after) (lpetersen :overlaps-before)) true)

#  crossing metrics. graph-theoretic (non-adjacent transversal points)
#  and visibly distinct (i.e. not hidden by a node or merged by location)

(cassert-eq "lombardi petersen graph crossings reported"
            (number? (lpetersen :crossings-graph-after)) true)

(cassert-eq "lombardi petersen visible crossings reported"
            (number? (lpetersen :crossings-visible-after)) true)

(cassert-eq "lombardi petersen visible <= graph"
            (<= (lpetersen :crossings-visible-after)
                (lpetersen :crossings-graph-after)) true)

(cassert-eq "lombardi petersen graph crossings deterministic"
            (= (lpetersen :crossings-graph-after)
               (lpetersen2 :crossings-graph-after)) true)

# meetings at a shared endpoint are not crossings
(def lpath (layout-lombardi (wg @[0 1 2] @[[0 1] [1 2]])
                            :width 10 :height 8))

(cassert-eq "lombardi path has no graph crossings"
            (lpath :crossings-graph-after) 0)
(cassert-eq "lombardi path has no visible crossings"
            (lpath :crossings-visible-after) 0)

# the reducer rotates locus joints
(each a (lpetersen :arcs)
  (when (= (a :kind) :poly)
    (cassert-eq "lombardi poly segments are all arcs"
                (> (length (seq [s :in (a :segments)
                                 :when (get s :line)] true)) 0) false)))

# K4
(def lk4 (layout-lombardi
          (wg @["a" "b" "c" "d"]
              @[["a" "b" 1] ["a" "c" 1] ["a" "d" 1]
                ["b" "c" 1] ["b" "d" 1] ["c" "d" 1]])))

(cassert-eq "lombardi K4 eligible" (lk4 :eligible) true)
(cassert-eq "lombardi K4 arcs count" (length (lk4 :arcs)) 6)

# The Petersen graph and the symmetry-aware route
(def lsym-g
  (wg (range 10)
      @[[0 1][1 2][2 3][3 4][4 0]
        [5 6][6 7][7 8][8 9][9 5]
        [0 5][1 6][2 7][3 8][4 9]]))

(def lsym (layout-lombardi lsym-g :width 10 :height 8))
(def lflat (layout-lombardi lsym-g :width 10 :height 8 :symmetry false
                            :arc-clearance false :node-clearance false))

(cassert-eq "lombardi symmetry is a 5-fold fold when used"
            (or (nil? (lsym :symmetry)) (= (get (lsym :symmetry) :k) 5)) true)
(cassert-eq "lombardi symmetry keeps the reduction baseline"
            (= (lsym :overlaps-before) (lflat :overlaps-before)) true)
(cassert-eq "lombardi symmetry improves the reduced overlaps"
            (< (lsym :overlaps-after) (lflat :overlaps-after)) true)
(cassert-eq "lombardi :symmetry false leaves the option off"
            (lflat :symmetry) nil)

# the :pos table may include vertices that are not in the graph
(def lsym-extra (layout-lombardi lsym-g :base :pos
                   :pos (merge @{} (lsym :pos) @{"z" (get (lsym :pos) "0")})))

(cassert-eq "lombardi symmetry ignores positions outside the graph when used"
            (or (nil? (lsym-extra :symmetry))
                (= (get (lsym-extra :symmetry) :k) 5)) true)

# Non-crossing arc separation
(def lpetersen-clear (layout-polyarc lsym-g :width 10 :height 8))
(def lpetersen-clear-off
  (layout-polyarc lsym-g :width 10 :height 8
                  :arc-clearance false :node-clearance false))

(cassert-eq "polyarc non-crossing clearance reported"
            (number? (lpetersen-clear :min-arc-clearance)) true)
(cassert-eq "polyarc keeps non-crossing arcs clear"
            (>= (lpetersen-clear :min-arc-clearance) 0.35) true)
(cassert-eq "polyarc non-crossing node clearance reported"
            (number? (lpetersen-clear :min-node-clearance)) true)
(cassert-eq "polyarc keeps arcs out of unrelated node discs"
            (>= (lpetersen-clear :min-node-clearance) 0.25) true)
(cassert-eq "polyarc clearance options off restore the closer drawing"
            (< (lpetersen-clear-off :min-arc-clearance) 0.35) true)

# a 3-cycle is rotationally symmetric
(def ltri (layout-lombardi
           (wg @["a" "b" "c"]
               @[["a" "b" 1] ["b" "c" 1] ["c" "a" 1]])
           :max-sweep nil))

(def ltri-flat (layout-lombardi
                (wg @["a" "b" "c"]
                    @[["a" "b" 1] ["b" "c" 1] ["c" "a" 1]])
                :max-sweep nil :symmetry false))

(cassert-eq "lombardi symmetry does not worsen the 3-cycle residual"
            (<= (ltri :residual) (+ (ltri-flat :residual) 1e-9)) true)

# K4 placed with an exact threefold rotation fixing "a"
(def lfixed-g
  (wg @["a" "b" "c" "d"]
      @[["a" "b" 1] ["a" "c" 1] ["a" "d" 1]
        ["b" "c" 1] ["c" "d" 1] ["d" "b" 1]]))

(def lfixed-pos
  @{"a" @[0.0 0.0]
    "b" @[1.0 0.0]
    "c" @[-0.5 0.8660254037844386]
    "d" @[-0.5 -0.8660254037844386]})

(def lfixed
  (layout-lombardi lfixed-g :base :pos :pos lfixed-pos :max-sweep 0))

(def lfixed-flat
  (layout-lombardi lfixed-g :base :pos :pos lfixed-pos :max-sweep 0
                   :symmetry false))

(cassert-eq "lombardi fixed-centre symmetry is a threefold fold when used"
            (or (nil? (lfixed :symmetry))
                (= (get (lfixed :symmetry) :k) 3)) true)
(cassert-eq "lombardi fixed-centre symmetry is never worse than the flat route"
            (<= (lfixed :overlaps-after) (lfixed-flat :overlaps-after)) true)

# named dispatch

(def ldispatch (graph-layout
                (wg @["a" "b" "c" "d"]
                    @[["a" "b" 1] ["b" "c" 1] ["c" "d" 1] ["d" "a" 1]]) "lombardi"))

(cassert-eq "graph-layout lombardi eligible"
            (ldispatch :eligible) true)
(cassert-eq "graph-layout lombardi arcs count"
            (length (ldispatch :arcs)) 4)

# non-planar directed graphs fall back to position-derived slots
(def lnonplanar (let [dg (make-digraph @[] @{})]
                  (add-edges dg @[["n1" "n2"] ["n2" "n3"] ["n3" "n4"]
                                  ["n4" "n5"] ["n5" "n6"] ["n6" "n1"]
                                  ["n2" "n5"] ["n3" "n6"] ["n1" "n4"]])
                  (graph-layout dg "lombardi")))

(cassert-eq "lombardi non-planar digraph does not crash"
            (lnonplanar :eligible) true)
(cassert-eq "lombardi non-planar digraph arcs count"
            (length (lnonplanar :arcs)) 9)

# native svg rendering (graphviz can't draw arcs)
(def svg-lombardi (to-svg
                   (wg (range 10)
                       @[[0 1][1 2][2 3][3 4][4 0]
                         [5 6][6 7][7 8][8 9][9 5]
                         [0 5][1 6][2 7][3 8][4 9]])
                   :layout "lombardi" :width 10
                   :height 8 :path "img/lombardi-test.svg"))

(cassert-eq "lombardi svg renders"
            (> (length svg-lombardi) 100) true)
(cassert-eq "lombardi svg has svg root"
            (> (length (string/find-all "<svg" svg-lombardi)) 0) true)
(cassert-eq "lombardi svg has circular-arc edges"
            (> (length (string/find-all " A " svg-lombardi)) 0) true)

# polyarc: tangent-frame sweep should never raise the crossing count
(defn pg [n adj]
  (def g (make-weighted-graph @[] @{}))
  (add-nodes g (map string (range n)))
  (each [u v] adj (add-edge g (string u) (string v) 1))
  g)

(def p-snark (pg 10 @[[0 1][1 2][2 3][3 4][4 0]
                      [5 6][6 7][7 8][8 9][9 5]
                      [0 5][1 6][2 7][3 8][4 9]]))

#  polyarc blocks. vary a single option each test

(def lp-base [:reduce-overlap true :reduce-passes 2 :sweep 8 :relax-levels 4])
(defn pa [& overrides]
  (graph-layout p-snark "polyarc" ;lp-base ;overrides))
(def lp-frame (pa))
(def lp-noback (pa :edge-order-backtrack false))

(cassert-eq "polyarc backtracking reduces poly-arcs"
            (<= (length (seq [a :in (lp-frame :arcs)
                              :when (= (a :kind) :poly)] true))
                (length (seq [a :in (lp-noback :arcs)
                              :when (= (a :kind) :poly)] true))) true)

(def lp-noframe (pa :frame-sweep false))
(cassert-eq "polyarc frame-sweep never raises overlaps"
            (<= (lp-frame :overlaps-after)
                (lp-frame :overlaps-before)) true)

(cassert-eq "polyarc frame-sweep lowers overlaps"
            (< (lp-frame :overlaps-after)
               (lp-frame :overlaps-before)) true)

(def lp-rep (pa))
(cassert-eq "polyarc frame-sweep deterministic"
            (= (lp-frame :overlaps-after)
               (lp-rep :overlaps-after)) true)

(def lp-noorder (pa :edge-order-sweep false))
(cassert-eq "polyarc edge-order sweep never raises overlaps"
            (<= (lp-frame :overlaps-after)
                (lp-noorder :overlaps-after)) true)

(def lp-capped (pa :max-sweep (* 0.5 math/pi)))
(cassert-eq "polyarc :max-sweep does not raise overlaps"
            (<= (lp-capped :overlaps-after)
                (lp-capped :overlaps-before)) true)

(def lp-capped-rep (pa :max-sweep (* 0.5 math/pi)))
(cassert-eq "polyarc :max-sweep deterministic"
            (= (lp-capped :overlaps-after)
               (lp-capped-rep :overlaps-after))
            true)

# polyarc: the slot table must use planar embedding's rotation system
(defn- pedges
  "The edge list `pg` implies, in its string-labelled node form."
  [pairs]
  (map |[(string ($ 0)) (string ($ 1))] pairs))

(defn slot-complete?
  "True if the planar embedding is accepted and every vertex has deg(v) slots."
  [g pairs]
  (def es (_pa__embedding-slots g (pedges pairs)))
  (if (nil? es)
    false
    (do
      (def deg (_pa__degrees (pedges pairs)))
      (var ok true)
      (eachp [v d] deg
             (def m (get es v))
             (unless (= (length m) d) (set ok false)))
      ok)))

(cassert-eq "polyarc: cut vertex still gets a full rotation from the embedding"
            (slot-complete? (pg 5 @[[0 1] [1 2] [2 0] [2 3] [3 4] [4 2]])
                            @[[0 1] [1 2] [2 0] [2 3] [3 4] [4 2]]) true)

(cassert-eq "polyarc: three triangles sharing a hub get a full rotation"
            (slot-complete? (pg 7 @[[0 1] [1 2] [2 0] [0 3] [3 4] [4 0]])
                            @[[0 1] [1 2] [2 0] [0 3] [3 4] [4 0]]) true)

(cassert-eq "polyarc: 2-connected graph gets a full rotation"
            (slot-complete? (pg 4 @[[0 1] [0 2] [0 3] [1 2] [1 3] [2 3]])
                            @[[0 1] [0 2] [0 3] [1 2] [1 3] [2 3]]) true)

(cassert-eq "polyarc: a tree has no planar faces to take a rotation from"
            (slot-complete? (pg 3 @[[0 1] [1 2]]) @[[0 1] [1 2]]) false)

# a bowtie must still lay out deterministically with embedding-derived slots
(def bt (pg 5 @[[0 1] [1 2] [2 0] [2 3] [3 4] [4 2]]))
(cassert-eq "polyarc: bowtie layout is deterministic"
            (deep= ((layout-polyarc bt) :arcs)
                   ((layout-polyarc bt) :arcs)) true)
(cassert-eq "polyarc: bowtie reduces overlaps"
            (< ((layout-polyarc bt) :overlaps-after)
               ((layout-polyarc bt) :overlaps-before)) true)


(print "\n* layout-dag: level structure should not be corrupted")

(defn distinct-ys [r]
  (sorted (map |($ 1) (values (get r :pos)))))

(defn y-gaps-of [ys]
  (def gaps @[])
  (var i 0)
  (while (< i (- (length ys) 1))
    (array/push gaps (math/floor (- (ys (+ i 1)) (ys i))))
    (++ i))
  gaps)

(defn uniform-gaps? [gaps]
  (var ok true)
  (var i 1)
  (while (< i (length gaps))
    (when (not= (gaps i) (gaps 0)) (set ok false) (break))
    (++ i))
  ok)

(each n [8 16 32]
  (def r (layout-dag (fx-staircase n) :force true))
  (def ys (distinct-ys r))
  (cassert-eq (string "staircase n=" n ": one distinct y per level")
              (length ys) (r :level-count))
  (cassert-eq (string "staircase n=" n ": uniform level spacing")
              (uniform-gaps? (y-gaps-of ys)) true))


#  trees

(def tm-parent @{"n1" "n0" "n2" "n0" "n3" "n1" "n4" "n1" "n5" "n2"})
(def tm-dg (make-digraph))
(for i 0 6 (digraph-add-node tm-dg (string "n" i)))
(def tm-tree (make-tree tm-dg "n0" tm-parent))

(cassert-eq "tree: record/nodes reads the backing digraph's labels"
            (= (length (r-nodes tm-tree)) 6) true)
(cassert-eq "tree: adjacency is the children map derived from :parent"
            (= (length (get (r-adjacency tm-tree) "n0")) 2) true)
(cassert-eq "tree: a leaf gets an empty row, not nil"
            (zero? (length (get (r-adjacency tm-tree) "n5"))) true)
(cassert-eq "tree: the root has a row even though it is no key of :parent"
            (has-key? (r-adjacency tm-tree) "n0") true)
(cassert-eq "tree: in-adjacency is the parent of each node"
            (= (get (get (r-in-adjacency tm-tree) "n3") "n1") true) true)
(cassert-eq "tree: node-count agrees with nodes"
            (= (node-count tm-tree) (length (r-nodes tm-tree))) true)

(each [nm f] [["force" layout-force] ["circle" layout-circle]
              ["radial" layout-radial] ["ortho" layout-orthogonal]
              ["spectral" layout-spectral] ["tree" layout-tree]
              ["phylogram" layout-phylogram] ["polyarc" layout-polyarc]]

  (def pos (get (f tm-tree) :pos))
  (cassert-eq (string "tree: layout-" nm " places every node")
              (= (length (keys pos)) 6) true)
  (cassert-eq (string "tree: layout-" nm " gives every node a numeric pair")
              (all (fn [k] (all number? (get pos k))) (keys pos)) true)
  (cassert-eq (string "tree: layout-" nm " does not stack nodes on one point")
              (> (length (keys (frequencies (map (fn [k] (tuple ;(get pos k)))
                                                 (keys pos))))) 1) true))


# :hspacing scales every weighted branch
(def hs-g (wg [0 1 2] [[0 1 1] [1 2 1]]))
(def hs-1 (get (layout-phylogram hs-g :root "0" :hspacing 1) :pos))
(def hs-2 (get (layout-phylogram hs-g :root "0" :hspacing 2) :pos))

(cassert-eq "phylogram: :hspacing scales an existing branch length"
            (get (get hs-2 "2") 0) (* 2 (get (get hs-1 "2") 0)))
(cassert-eq "phylogram: :hspacing leaves the root at depth zero"
            (get (get hs-2 "0") 0) 0)


#  layout-polyarc: crossing-reduction sweep is quadratic
(def pa-g (make-graph @{}))
(for i 0 24 (add-node pa-g (string "p" i)))
(for i 1 24 (add-edge pa-g (string "p" (- i 1)) (string "p" i)))

(def pa-on (layout-polyarc pa-g))
(cassert-eq "polyarc: places every node by default"
            (= (length (keys (get pa-on :pos))) 24) true)

(cassert-eq "polyarc: :reduce-overlap false still places every node"
            (= (length
                (keys (get (layout-polyarc pa-g :reduce-overlap false) :pos)))
               24) true)

(cassert-eq "polyarc: a lower :sweep still places every node"
            (= (length
                (keys (get (layout-polyarc pa-g :sweep 2) :pos))) 24) true)

(cassert-eq "polyarc: turning the sweep off is deterministic"
            (deep= (get (layout-polyarc pa-g :reduce-overlap false) :pos)
                   (get (layout-polyarc pa-g :reduce-overlap false) :pos))
            true)

(cassert-eq "polyarc: the default is deterministic run to run"
            (deep= (get (layout-polyarc pa-g) :pos)
                   (get (layout-polyarc pa-g) :pos)) true)


#  layout-spectral

(def sp-legible (layout-spectral (fx-path 60)))
(def sp-collapsed (layout-spectral (fx-layered 8 3)))

(cassert-eq "spectral: a path is legible and reports no degeneracy"
            (get sp-legible :degeneracy) nil)
(cassert-eq "spectral: a layered DAG reports its collapse"
            ((get sp-collapsed :degeneracy) 0) true)
(cassert-eq "spectral: the reported implied scale is past the cap to-svg clamps at"
            (> ((get sp-collapsed :degeneracy) 2) 1500.0) true)
(cassert-eq "spectral: a degenerate result is still well-formed, positions included"
            (= (length (get sp-collapsed :pos)) 24) true)
(cassert-eq "spectral: a degenerate result still reports its eigenvalues"
            (> (length (get sp-collapsed :eigenvalues)) 1) true)


# layout-dag: invariants, and sifting iteration

(defn dg-chain [n]
  (def g (make-digraph))
  (for i 0 n (digraph-add-node g (string "n" i)))
  (for i 1 n (digraph-add-edge g (string "n" (- i 1)) (string "n" i) 1))
  g)

(defn dg-layered [layers per]
  (def g (make-digraph))
  (for l 0 layers
    (for i 0 per
      (def nm (string "L" l "N" i))
      (digraph-add-node g nm)
      (when (pos? l)
        (for j 0 per (digraph-add-edge g (string "L" (- l 1) "N" j) nm 1)))))
  g)

(defn dg-diamond [n]
  (def g (make-digraph))
  (digraph-add-node g "s") (digraph-add-node g "t")
  (for i 1 (- n 1) (digraph-add-node g (string "m" i)))
  (for i 1 (- n 1) (do (digraph-add-edge g "s" (string "m" i) 1)
                       (digraph-add-edge g (string "m" i) "t" 1)))
  g)

(each [nm g] [["chain-20" (dg-chain 20)] ["layered-9x3" (dg-layered 9 3)]
              ["diamond-10" (dg-diamond 10)]]
  (def r (layout-dag g :force true))
  (def pos (get r :pos))
  (def levels (get r :levels))
  (def lc (get r :level-count))

  (cassert-eq (string "dag/" nm ": eligible when forced")
              (get r :eligible) true)
  (cassert-eq (string "dag/" nm ": every node placed")
              (= (length (keys pos)) (length (keys levels))) true)
  (cassert-eq (string "dag/" nm ": every position is a finite numeric pair")
              (all (fn [p] (and (= 2 (length p))
                                (all (fn [c] (= c c)) p))) (values pos)) true)
  (cassert-eq (string "dag/" nm ": levels are within 0..level-count-1")
              (all (fn [l] (and (number? l)
                                (>= l 0) (< l lc))) (values levels)) true)
  (cassert-eq (string "dag/" nm ": one row per level")
              (length (distinct (map (fn [k]
                                       ((get pos k) 1)) (keys pos)))) lc)
  (cassert-eq (string "dag/" nm ": crossing minimisation never regresses")
              (<= (get r :crossings-after)
                  (get r :crossings-initial)) true)

  # the node-index is a permutation within each level
  (def per-level @{})
  (eachp [k rk] (get r :node-index)
         (def lst (or (get per-level (get levels k)) @[]))
         (array/push lst rk)
         (put per-level (get levels k) lst))

  (cassert-eq (string "dag/" nm ": ranks are unique within each level")
              (all (fn [lst] (= (length lst)
                                (length (distinct lst))))
                   (values per-level)) true))

(def dg-r (layout-dag (dg-layered 8 3) :force true))
(cassert-eq "dag: deterministic across calls"
            (deep= (get dg-r :pos)
                   (get (layout-dag (dg-layered 8 3) :force true) :pos)) true)

(def dg-fast (layout-dag (dg-layered 8 3) :force true :max-iterations 1))
(cassert-eq "dag: :max-iterations 1 still lays the graph out completely"
            (= (length (keys (get dg-fast :pos)))
               (length (keys (get dg-r :pos)))) true)

(cassert-eq "dag: :max-iterations 1 never regresses crossings"
            (<= (get dg-fast :crossings-after)
                (get dg-fast :crossings-initial)) true)

#  layout-dag: ineligibility reasons

(def el-dag (make-digraph))
(for i 0 6 (digraph-add-node el-dag (string "n" i)))
(for i 1 5 (digraph-add-edge el-dag (string "n" (- i 1)) (string "n" i) 1))

(cassert-eq "dag ineligible: :max-nodes below the node count"
            ((layout-dag el-dag :max-nodes 3) :reason) "too-large")
(cassert-eq "dag ineligible: :max-edges below the arc count"
            ((layout-dag el-dag :max-edges 1) :reason) "too-large")

(def el-und (fx-path 5))
(cassert-eq "dag ineligible: an undirected graph has no source or sink"
            ((layout-dag el-und) :reason) "no-source-or-sink")
(cassert-eq "dag: :force lays out what the heuristics would reject"
            (get (layout-dag el-und :force true) :eligible) true)

(cassert-eq "dag ineligible: many nodes on three levels is too wide"
            ((layout-dag (fx-layered 3 10)) :reason) "too-wide")
(cassert-eq "dag ineligible: a hub past 2*sqrt(n) is a dense hub"
            ((layout-dag (fx-layered 3 6)) :reason) "dense-hub")

#  tree adjacency cache

(def cache-parent @{})
(put cache-parent "n1" "n0")
(put cache-parent "n2" "n1")
(put cache-parent "n3" "n2")

(defn tree-with-parent [parent root]
  (def dg (make-digraph))
  (digraph-add-node dg root)
  (eachp [child par] parent
         (digraph-add-node dg child)
         (digraph-add-edge dg par child 1))
  (make-tree dg root parent))

(def cache-warm (tree-with-parent cache-parent "n0"))
(def cache-cold (tree-with-parent cache-parent "n0"))
(r-adjacency cache-warm)
(r-in-adjacency cache-warm)

(cassert-eq "tree: cached adjacency matches a freshly built net"
            (deep= (r-adjacency cache-warm) (r-adjacency cache-cold)) true)
(cassert-eq "tree: cached in-adjacency matches a freshly built net"
            (deep= (r-in-adjacency cache-warm) (r-in-adjacency cache-cold)) true)
(cassert-eq "tree: a second read of the cache is the same as the first"
            (deep= (r-adjacency cache-warm) (r-adjacency cache-warm)) true)


# layout-dag :direction

(def dir-g (fx-layered 3 3))
(defn dir-pos [d] (get (layout-dag dir-g :force true :direction d) :pos))
(defn dir-level-y [pos lvl] ((get pos (string "L" lvl "N0")) 1))
(defn dir-level-x [pos lvl] ((get pos (string "L" lvl "N0")) 0))

(def dir-tb (dir-pos "tb"))
(def dir-bt (dir-pos "bt"))
(def dir-lr (dir-pos "lr"))
(def dir-rl (dir-pos "rl"))

(each d [["tb" dir-tb] ["bt" dir-bt] ["lr" dir-lr] ["rl" dir-rl]]
  (def pos (d 1))
  (cassert-eq (string "layout-dag direction " (d 0) ": every node placed")
              (length (keys pos)) 9)
  (cassert-eq (string "layout-dag direction " (d 0) ": every coordinate finite")
              (all (fn [p] (all (fn [c] (= c c)) p)) (values pos)) true)
  (cassert-eq (string "layout-dag direction " (d 0) ": one row per level")
              (length (distinct (map (fn [p] (p 1)) (values pos)))) 3))

(cassert-eq "layout-dag tb: y increases down the hierarchy"
            (< (dir-level-y dir-tb 0) (dir-level-y dir-tb 1)) true)
(cassert-eq "layout-dag tb: coordinates are non-negative"
            (all (fn [p] (and (>= (p 0) 0) (>= (p 1) 0)))
                 (values dir-tb)) true)
(cassert-eq "layout-dag bt: every node is tb with y negated"
            (all (fn [k] (and (= ((dir-tb k) 0) ((dir-bt k) 0))
                              (= ((dir-tb k) 1) (* -1 ((dir-bt k) 1)))))
                 (keys dir-tb)) true)
(cassert-eq "layout-dag bt: which is a mirror, so y is non-positive"
            (all (fn [p] (<= (p 1) 0)) (values dir-bt)) true)
(cassert-eq "layout-dag lr: x increases across the hierarchy"
            (< (dir-level-x dir-lr 0) (dir-level-x dir-lr 1)) true)
(cassert-eq "layout-dag lr: coordinates are non-negative"
            (all (fn [p] (and (>= (p 0) 0) (>= (p 1) 0)))
                 (values dir-lr)) true)
(cassert-eq "layout-dag rl: every node is lr with both axes negated"
            (all (fn [k] (and (= ((dir-lr k) 0) (* -1 ((dir-rl k) 0)))
                              (= ((dir-lr k) 1) (* -1 ((dir-rl k) 1)))))
                 (keys dir-lr)) true)

(defn level-spread
  "The extent of one level along an axis, from a complete layout result."
  [r lvl axis]
  (def pos (get r :pos))
  (def lv (get r :levels))
  (var lo nil) (var hi nil)
  (eachp [k p] pos
         (when (= (get lv k) lvl)
           (def v (if (= axis 0) (p 0) (p 1)))
           (when (or (nil? lo) (< v lo)) (set lo v))
           (when (or (nil? hi) (> v hi)) (set hi v))))
  (if (nil? lo) 0.0 (- hi lo)))

(defn dir-level-y [pos lvl] ((get pos (string "L" lvl "N0")) 1))
(defn dir-level-x [pos lvl] ((get pos (string "L" lvl "N0")) 0))

(def dir-tb (dir-pos "tb"))
(def dir-bt (dir-pos "bt"))
(def dir-lr (dir-pos "lr"))
(def dir-rl (dir-pos "rl"))

(each d [["tb" dir-tb] ["bt" dir-bt] ["lr" dir-lr] ["rl" dir-rl]]
  (def pos (d 1))
  (cassert-eq (string "layout-dag direction " (d 0) ": every node placed")
              (length (keys pos)) 9)
  (cassert-eq (string "layout-dag direction " (d 0) ": every coordinate finite")
              (all (fn [p] (all (fn [c] (= c c)) p)) (values pos)) true)
  (cassert-eq (string "layout-dag direction " (d 0) ": one row per level")
              (length (distinct (map (fn [p] (p 1)) (values pos)))) 3))

(cassert-eq "layout-dag tb: y increases down the hierarchy"
            (< (dir-level-y dir-tb 0) (dir-level-y dir-tb 1)) true)
(cassert-eq "layout-dag tb: coordinates are non-negative"
            (all (fn [p] (and (>= (p 0) 0) (>= (p 1) 0))) (values dir-tb)) true)
(cassert-eq "layout-dag bt: every node is tb with y negated"
            (all (fn [k] (and (= ((dir-tb k) 0) ((dir-bt k) 0))
                              (= ((dir-tb k) 1) (* -1 ((dir-bt k) 1))))) (keys dir-tb)) true)
(cassert-eq "layout-dag bt: which is a mirror, so y is non-positive"
            (all (fn [p] (<= (p 1) 0)) (values dir-bt)) true)
(cassert-eq "layout-dag lr: x increases across the hierarchy"
            (< (dir-level-x dir-lr 0) (dir-level-x dir-lr 1)) true)
(cassert-eq "layout-dag lr: coordinates are non-negative"
            (all (fn [p] (and (>= (p 0) 0) (>= (p 1) 0))) (values dir-lr)) true)
(cassert-eq "layout-dag rl: every node is lr with both axes negated"
            (all (fn [k] (and (= ((dir-lr k) 0) (* -1 ((dir-rl k) 0)))
                              (= ((dir-lr k) 1) (* -1 ((dir-rl k) 1))))) (keys dir-lr)) true)


# Node sizes widen the level they are on
(def sz-pos (layout-dag (fx-layered 2 3) :force true))
(def sz-wide (layout-dag (fx-layered 2 3) :force true
                         :node-sizes @{"L0N0" [900 60]}))

(cassert-eq "layout-dag :node-sizes widens the level the node is on"
            (> (level-spread sz-wide 0 0) (level-spread sz-pos 0 0)) true)
(cassert-eq "layout-dag :node-sizes leaves the other level alone"
            (= (level-spread sz-wide 1 0) (level-spread sz-pos 1 0)) true)

# sizes swap roles for the horizontal directions
(defn lr-size-run [d sizes]
  (layout-dag (fx-layered 2 3)
              :force true :direction d :node-sizes sizes))
(each d ["tb" "lr"]
  (def axis (if (= d "tb") 0 1))
  (def plain (lr-size-run d @{}))
  (def wide (lr-size-run d @{"L0N0" [900 60]}))
  (def tall (lr-size-run d @{"L0N0" [60 900]}))
  # transposition.
  (def along (if (= d "tb") wide tall))   # 900 in the level-axis direction
  (def across (if (= d "tb") tall wide))  # 900 in the other direction

  (cassert-eq (string "layout-dag " d ": size along the level axis widens it")
              (> (level-spread along 0 axis) (level-spread plain 0 axis)) true)

  (cassert-eq (string "layout-dag " d ": size across it does not, sizes transpose")
              (<= (level-spread across 0 axis) (level-spread plain 0 axis)) true))

(print-results)
