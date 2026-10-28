# -*- mode: janet; -*-
# graphs/test/export: to-dot

(import ./helper :prefix "")
(import ../graphs :prefix "")
(import ../graphs/export/common :prefix "ec-")
(import ../graphs/util :prefix "u-")

(defn- has?
  [hay needle]
  (not= nil (string/find needle hay)))

(defn- misses?
  [hay needle]
  (nil? (string/find needle hay)))

(print "\n* export: to-dot")

(def dg (make-digraph))
(digraph-add-edge dg "a" "b" 1)
(digraph-add-edge dg "b" "c" 2)

(def dot (to-dot dg :label-edges true))
(cassert-eq "digraph header" (string/find "digraph G {" dot) 0)
(cassert-eq "digraph uses ->" (has? dot "->") true)
(cassert-eq "digraph has no --" (misses? dot "--") true)
(cassert-eq "edge weight label" (has? dot "label=\"1\"") true)
(cassert-eq "default rankdir LR" (has? dot "rankdir=LR") true)
(cassert-eq "nodes emitted" (has? dot "\"a\";") true)

(cassert-eq ":name" (has? (to-dot dg :name "M") "digraph M {") true)
(cassert-eq ":rankdir" (has? (to-dot dg :rankdir "TB") "rankdir=TB") true)
(cassert-eq ":label-edges off"
            (misses? (to-dot dg :label-edges false) "label=") true)
(cassert-eq ":node-opts"
            (has? (to-dot dg :node-opts @{:shape "box"})
                  "shape=\"box\"") true)
(cassert-eq ":edge-opts"
            (has? (to-dot dg :edge-opts @{:color "#333"})
                  "color=\"#333\"") true)
(cassert-eq ":graph-opts"
            (has? (to-dot dg :graph-opts @{:bgcolor "pink"})
                  "bgcolor=pink") true)

(def wg (make-weighted-graph))
(weighted-graph-add-edge wg "a" "b" 1.5)
(cassert-eq "weighted undirected uses --"
            (has? (to-dot wg) "--") true)
(cassert-eq "weighted has no arrows"
            (misses? (to-dot wg) "->") true)
(cassert-eq "empty digraph"
            (string/find "digraph G {" (to-dot (make-digraph))) 0)

(def sym (make-digraph))
(add-nodes sym "a" "b")
(digraph-add-edge sym "a" "b" 1)
(digraph-add-edge sym "b" "a" 1)
(cassert-eq "symmetric digraph auto-detects as undirected"
            (string/find "digraph" (to-dot sym)) nil)
(cassert-eq ":directed? true forces a digraph"
            (string/find "digraph" (to-dot sym :directed? true)) 0)
(cassert-eq ":directed? true uses arrows"
            (has? (to-dot sym :directed? true) "->") true)
(cassert-eq ":directed? false forces undirected"
            (string/find "digraph" (to-dot sym :directed? false)) nil)
(cassert-eq ":directed? survives a round trip"
            ((graph-from-dot (to-dot sym :directed? true)) :edge-count) 2)

(def sl-d (make-digraph))
(add-edge sl-d "a" "a" 5)
(cassert-eq "directed self loop"
            (has? (to-dot sl-d :directed? true) "\"a\" -> \"a\"") true)

(def sl-g (make-graph @{}))
(add-edge sl-g "a" "a")
(cassert-eq "undirected self loop"
            (has? (to-dot sl-g) "\"a\" -- \"a\"") true)

(def mg (make-multigraph @{}))
(add-edges mg ["a" "b"] ["a" "b"] ["a" "b"])
(cassert-eq "parallel edges stay separate"
            (length (string/find-all "\"a\" -- \"b\"" (to-dot mg))) 3)

(print "\n* export: per-element colour")

(def K4
  (do (def g (make-graph @{}))
      (each [u v] [["a" "b"] ["a" "c"] ["a" "d"]
                   ["b" "c"] ["b" "d"] ["c" "d"]]
        (add-edge g u v))
      g))

(def nc (to-dot K4 :node-colors @{"a" "#ff0000" "b" "#00ff00"}))
(cassert-eq "node-colors: the named node carries its colour"
            (has? nc "\"a\" [fillcolor=\"#ff0000\"]") true)
(cassert-eq "node-colors: a second named node carries its own"
            (has? nc "\"b\" [fillcolor=\"#00ff00\"]") true)
(cassert-eq "node-colors: an unlisted node is untouched"
            (has? nc "\"c\";") true)
(cassert-eq "node-colors: the drawing-wide default is unchanged"
            (has? nc "node [shape=\"ellipse\"") true)

(def nc-str-key (to-dot K4 :node-colors @{"1" "#ff0000"}))
(cassert-eq "node-colors: keys compare by value, so \"1\" misses node 1"
            (misses? nc-str-key "fillcolor=\"#ff0000\"") true)

(def nc-pos
  (to-dot K4 :node-colors @{"a" "#ff0000"}
             :node-positions @{"a" [10.0 20.0]}))
(cassert-eq "node-colors: pos and fillcolor share one attribute list"
            (has? nc-pos "pos=\"10,-20!\",fillcolor") true)

(def ec (to-dot K4 :label-edges false
                  :edge-opts @{:color "#000000"}
                  :edge-colors @{(tuple "a" "b") "#ff0000"}))
(cassert-eq "edge-colors: the named edge carries its colour"
            (has? ec "\"a\" -- \"b\" [color=\"#ff0000\"") true)
(cassert-eq "edge-colors: an unlisted edge keeps the default"
            (has? ec "\"a\" -- \"c\" [color=\"#000000\"") true)

(cassert-eq "an empty :node-colors is identical to none"
            (= (to-dot K4 :node-colors @{}) (to-dot K4)) true)
(cassert-eq "an empty :edge-colors is identical to none"
            (= (to-dot K4 :edge-colors @{}) (to-dot K4)) true)

(def nasty (to-dot K4 :node-colors @{"a" "#ff \"00 00\""}))
(cassert-eq "node-colors: a quote inside a colour is escaped"
            (has? nasty "fillcolor=\"#ff \\\"00 00\\\"\"") true)
(cassert-eq "node-colors: the escaped colour keeps the bracket intact"
            (has? nasty "fillcolor=\"#ff \\\"00 00\\\"\"];") true)

# Integer nodes: native order (2 < 10) disagrees with string order ("10" < "2").
(def INTG
  (do (def g (make-graph @{}))
      (each [u v] [[2 10] [10 3] [3 2] [3 4] [4 2]] (add-edge g u v))
      g))

(def vz (vizing-edge-colour INTG))
(def vzdot (to-dot INTG :label-edges false
                    :edge-opts @{:color "#000000"}
                    :edge-colors (get vz :colouring)))
(def vz-edge-lines
  (filter |(string/find " -- " $) (string/split "\n" vzdot)))

(cassert-eq "edge-colors: a vizing colouring styles every edge"
            (length (filter |(string/find "color=\"#000000\"" $)
                            vz-edge-lines)) 0)
(cassert-eq "edge-colors: the colouring reaches all 5 edges"
            (length vz-edge-lines) 5)
(cassert-eq "edge-colors: the edge 3-10 is found by string order"
            (has? vzdot "\"3\" -- \"10\" [color=\"") true)
(cassert-eq "edge-colors: a native-ordered key misses it"
            (misses? vzdot "\"3\" -- \"10\" [color=\"#000000\"") true)

(def DG (make-digraph))
(digraph-add-edge DG "a" "b" 1)
(digraph-add-edge DG "b" "a" 1)
(digraph-add-edge DG "b" "c" 2)

(def dgdot (to-dot DG :label-edges false :directed? true
                   :edge-opts @{:color "#000000"}
                   :edge-colors @{(tuple "a" "b") "#ff0000"}))

(cassert-eq "edge-colors: a directed arc uses its natural orientation"
            (has? dgdot "\"a\" -> \"b\" [color=\"#ff0000\"") true)
(cassert-eq "edge-colors: the reverse arc is a different arc"
            (misses? dgdot "\"b\" -> \"a\" [color=\"#ff0000\"") true)

(cassert-eq "canonical key uses the string ordering"
            (u-canonical-edge-key 3 10) (tuple 10 3))
(cassert-eq "ec-edge-colour-key agrees for an undirected edge"
            (ec-edge-colour-key false 3 10) (tuple 10 3))
(cassert-eq "ec-edge-colour-key keeps directed orientations"
            (ec-edge-colour-key true 3 10) (tuple 3 10))

(def mg3 (make-multidigraph))
(add-nodes mg3 "a" "b" "c")
(add-edges mg3 ["a" "b"] ["a" "b"] ["b" "c"])

(cassert-eq "multidigraph: parallel edges are kept"
            (length (string/find-all " -> " (to-dot mg3))) 3)

(cassert-eq "multidigraph: the edge is emitted"
            (not= nil (string/find
                       "\"a\" -> \"b\""
                       (do (def md1 (make-multidigraph))
                           (add-nodes md1 "a" "b")
                           (add-edge md1 "a" "b")
                           (to-dot md1)))) true)

(print-results)
