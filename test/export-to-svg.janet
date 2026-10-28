# -*- mode: janet; -*-
# graphs/test/export: to-svg

(import ./helper :prefix "")
(import ../graphs :prefix "")
(import ../graphs/export :prefix "ex-")

(defn- has?
  [hay needle]
  (not= nil (string/find needle hay)))

(def graphviz?
  (try (do (os/execute ["dot" "-V"]
                       :p {:err (file/open "/dev/null" :w)})
           true)
       ([_] false)))

(print "\n* export: to-svg")

(when (not graphviz?)
  (print "  - graphviz dot not found; skipping graphviz tests"))

(when graphviz?
  (def g (make-digraph @[] @{}))
  (digraph-add-edge g "a" "b" 1)
  (def out (to-svg g :path "/tmp/_snark-test-svg.svg" :layout "dot"))

  (cassert-eq "svg root element" (has? out "<svg") true)
  (cassert-eq "svg has nodes" (has? out "<title>a</title>") true)
  (cassert-eq "svg has edges" (has? out "edge") true)

  (def kept (to-svg g :path "/tmp/_snark-test-svg2.svg" :layout "neato"
                    :dot-path "/tmp/_snark-test-kept.dot"))

  (cassert-eq "svg neato renders"
              (has? kept "<svg") true)

  (cassert-eq "dot file written"
              (not= nil (os/stat "/tmp/_snark-test-kept.dot")) true))

(print "\n* export: escaping")

(when graphviz?
  (def tricky (make-digraph))
  (add-nodes tricky "a&b" "x<y")
  (digraph-add-edge tricky "a&b" "x<y" 1)
  (def sv (to-svg tricky))
  (cassert-eq "svg escapes ampersand" (has? sv "a&amp;b") true)
  (cassert-eq "svg escapes less-than" (has? sv "x&lt;y") true))

(def esc-g (make-digraph))
(add-nodes esc-g "a<b&c" "plain")
(digraph-add-edge esc-g "a<b&c" "plain" 1)
(def esc-svg
  (to-svg esc-g :layout "lombardi" :label-nodes true
          :path "/tmp/snark-test-esc.svg"
          :dot-path "/tmp/snark-test-esc.dot"))

(cassert-eq "native renderer XML-escapes a node label"
            (has? esc-svg "a&lt;b&amp;c") true)

(cassert-eq "the raw label does not leak into the svg"
            (has? esc-svg ">a<b&c<") false)

(def inside-svg
  (to-svg esc-g :layout "lombardi" :label-nodes true
          :label-position :inside
          :path "/tmp/snark-test-inside.svg"
          :dot-path "/tmp/snark-test-inside.dot"))

(cassert-eq ":label-position :inside centres the text"
            (has? inside-svg "dominant-baseline=\"central\"") true)

(defn- svg-font-size
  [s]
  (def i (string/find "font-size=\"" s))
  (scan-number (string/slice s (+ 11 i) (+ 15 i))))

(cassert-eq "an inside label defaults to a font that fits the node"
            (< (svg-font-size inside-svg)
               (svg-font-size esc-svg)) true)

(def long-g (make-digraph))
(add-nodes long-g "a-very-long-label" "b")
(digraph-add-edge long-g "a-very-very-very-very-long-label" "b" 1)

(def long-svg
  (to-svg long-g :layout "lombardi" :label-nodes true
          :label-position :inside
          :path "/tmp/snark-test-long.svg"
          :dot-path "/tmp/snark-test-long.dot"))

(cassert-eq "the fitted label size shrinks with the longest label"
            (< (svg-font-size long-svg) (svg-font-size inside-svg)) true)

(def pos-g (make-digraph))
(add-nodes pos-g "a" "b")
(add-edge pos-g "a" "b" 1)

(to-svg pos-g :layout "circle"
        :node-positions @{"a" @[11 22] "b" @[33 44]}
        :path "/tmp/snark-test-pos.svg"
        :dot-path "/tmp/snark-test-pos.dot")

(def pos-dot (slurp "/tmp/snark-test-pos.dot"))

(cassert-eq "a caller's node positions survive a library layout"
            (has? pos-dot "pos=\"11,-22!\"") true)

(cassert-eq "both nodes are pinned by the caller"
            (has? pos-dot "pos=\"33,-44!\"") true)

(to-svg pos-g :layout "circle"
        :path "/tmp/snark-test-pos2.svg"
        :dot-path "/tmp/snark-test-pos2.dot")

(cassert-eq "the layout positions are used when the caller gives none"
            (has? (slurp "/tmp/snark-test-pos2.dot") "pos=") true)

(def pad-g (make-digraph))
(add-nodes pad-g "a" "b")
(add-edge pad-g "a" "b" 1)
(def pad-svg
  (to-svg pad-g :layout "lombardi"
          :path "/tmp/snark-test-pad.svg" :node-radius 0.2))

(defn- svg-attr
  [s attr]
  (scan-number
    (first (string/split "\""
                         (last (string/split (string attr "=\"") s))))))

(def pad-w (svg-attr pad-svg "width"))
(def pad-h (svg-attr pad-svg "height"))
(def pad-circles
  (map (fn [piece]
         @[(svg-attr piece "cx")
           (svg-attr piece "cy")
           (svg-attr piece "r")])
       (slice (string/split "<circle" pad-svg) 1)))

(cassert-eq "the native renderer draws both nodes"
            (length pad-circles) 2)

(eachp [i [cx cy r]] pad-circles
  (cassert-eq (string "node " i " is not clipped by the canvas edge")
              (and (> (- cx r) 0) (< (+ cx r) pad-w)
                   (> (- cy r) 0) (< (+ cy r) pad-h))
              true))

(def sp-g (make-digraph))
(add-nodes sp-g (range 0 60))
(each i (range 0 59) (digraph-add-edge sp-g i (+ i 1) 1))

(def sp-fixed
  (to-svg sp-g :layout "circle" :position-scale 77
          :path "/tmp/snark-sp-fixed.svg"
          :dot-path "/tmp/snark-sp-fixed.dot"))
(cassert-eq "an explicit :position-scale is used verbatim"
            (has? (slurp "/tmp/snark-sp-fixed.dot") "pos=\"1234") false)

(to-svg sp-g :layout "circle"
        :path "/tmp/snark-sp-auto.svg"
        :dot-path "/tmp/snark-sp-auto.dot")

(cassert-eq "a derived scale is produced"
            (has? (slurp "/tmp/snark-sp-auto.dot") "pos=") true)

(cassert-eq ":position-scale-max is honoured"
            (do (to-svg sp-g :layout "circle" :position-scale-max 60
                        :path "/tmp/snark-sp-cap.svg"
                        :dot-path "/tmp/snark-sp-cap.dot")
                (has? (slurp "/tmp/snark-sp-cap.dot") "pos="))
            true)

(def sp-tiny (make-digraph))
(add-nodes sp-tiny "a" "b")
(add-edge sp-tiny "a" "b" 1)
(cassert-eq "a two-node graph still renders"
            (= (type (to-svg sp-tiny :layout "circle"
                             :path "/tmp/snark-sp-tiny.svg"
                             :dot-path "/tmp/snark-sp-tiny.dot"))
               :string)
            true)

(when graphviz?
  (def mc-g (make-digraph))
  (add-nodes mc-g "a" "b" "c")
  (add-edges mc-g ["a" "b" 1] ["b" "c" 1])

  (cassert-eq "an ordinary render returns a string"
              (= (type (to-svg mc-g :layout "neato"
                               :path "/tmp/snark-mc.svg"
                               :dot-path "/tmp/snark-mc.dot"))
                 :string) true)

  (cassert-eq ":max-canvas leaves a normal render alone"
              (= (type (to-svg mc-g :layout "neato" :max-canvas 20000
                               :path "/tmp/snark-mc2.svg"
                               :dot-path "/tmp/snark-mc2.dot"))
                 :string) true)

  (cassert-eq "a canvas over :max-canvas is refused"
              (not= nil
                    (try (do (to-svg mc-g :layout "neato" :max-canvas 1
                                     :path "/tmp/snark-mc3.svg"
                                     :dot-path "/tmp/snark-mc3.dot")
                             nil)
                         ([e] e))) true)

  (cassert-eq ":max-canvas 0 disables the check"
              (= (type (to-svg mc-g :layout "neato" :max-canvas 0
                               :path "/tmp/snark-mc4.svg"
                               :dot-path "/tmp/snark-mc4.dot"))
                 :string) true))

(when graphviz?
  (def warn-g (make-digraph))
  (add-nodes warn-g "w" "x" "y")
  (add-edge warn-g "w" "x" 1)
  (add-edge warn-g "x" "y" 1)

  (cassert-eq ":position-scale-warn is accepted"
              (= (type (to-svg warn-g :layout "neato"
                               :position-scale-warn false
                               :path "/tmp/snark-warn.svg"
                               :dot-path "/tmp/snark-warn.dot"))
                 :string) true)

  (cassert-eq "a capped scale still renders rather than failing"
              (= (type (to-svg warn-g :layout "neato"
                               :position-scale-max 1
                               :position-scale-warn false
                               :path "/tmp/snark-warn3.svg"
                               :dot-path "/tmp/snark-warn3.dot"))
                 :string) true))

(def cs-cap (ex-capped-scale-warning 1500))
(cassert-eq "warning: names the cap that was hit"
            (has? cs-cap "limit of 1500") true)
(cassert-eq "warning: names an alternative layout"
            (has? cs-cap "force, circle or radial") true)
(cassert-eq "warning: says how to accept it anyway"
            (has? cs-cap "raise :position-scale-max") true)
(cassert-eq "warning: says how to silence it"
            (has? cs-cap ":position-scale-warn false") true)
(cassert-eq "warning: reports the cap it is given"
            (has? (ex-capped-scale-warning 42) "limit of 42") true)

(def sc-pos @{"a" [0.0 0.0] "b" [1.0 0.0]
              "c" [0.5 0.9] "d" [0.4 1.0]
              "e" [0.55 1.1] "f" [0.5 1.2]
              "g" [0.5 1.3] "h" [0.5 1.4]})

(defn- sc
  [spacing floor cap]
  (ex-auto-position-scale sc-pos spacing floor cap))

(def sc-base (sc 60.0 40.0 1500.0))
(cassert-eq "position scale: the default is not capped"
            (sc-base 1) false)
(cassert-eq "position scale: :node-spacing scales proportionally"
            (let [[s2 _] (sc 120.0 40.0 1500.0)]
              (= (math/floor (* 2 (sc-base 0)))
                 (math/floor s2))) true)
(cassert-eq "position scale: :position-scale-floor raises a small result"
            (let [[s2 capped?] (sc 60.0 500.0 1500.0)]
              (and (= (math/floor s2) 500) (not capped?))) true)
(cassert-eq "position scale: :position-scale-max lowers a large result"
            (let [[s2 capped?] (sc 60.0 40.0 10.0)]
              (and (= (math/floor s2) 10) capped?)) true)
(cassert-eq "position scale: a single point falls back to the floor"
            (let [[s2 capped?]
                  (ex-auto-position-scale @{"a" [0.0 0.0]} 60.0 40.0 1500.0)]
              (and (= (math/floor s2) 40) (not capped?))) true)
(cassert-eq "position scale: coincident points do not divide by zero"
            (let [[s2 capped?]
                  (ex-auto-position-scale @{"a" [1.0 1.0] "b" [1.0 1.0]}
                                          60.0 40.0 1500.0)]
              (and (number? s2) (not capped?))) true)
(cassert-eq "position scale: an empty layout falls back to the floor"
            (let [[s2 capped?]
                  (ex-auto-position-scale @{} 60.0 40.0 1500.0)]
              (and (= (math/floor s2) 40) (not capped?))) true)

(def degen-petersen
  (do (def g (make-graph @{}))
      (add-nodes g (range 10))
      (add-edges g @[[0 1] [1 2] [2 3] [3 4] [4 0]
                     [5 6] [6 7] [7 8] [8 9] [9 5]
                     [0 5] [1 6] [2 7] [3 8] [4 9]])
      g))

(def degen-spec (layout-spectral degen-petersen))

(cassert-eq "spectral on Petersen reports a degenerate embedding"
            (not= nil (degen-spec :degeneracy)) true)

(cassert-eq "spectral on Petersen collapses 10 nodes onto 5 positions"
            (let [uniq @{}]
              (each p (values (degen-spec :pos))
                (put uniq (string (math/round (* 1e6 (p 0))) ";"
                                  (math/round (* 1e6 (p 1)))) true))
              (length (keys uniq))) 5)

(def degen-err
  (try
    (do (to-svg degen-petersen :layout "lombardi" :base :spectral
                :width 20 :height 20
                :path "/tmp/snark-test-degen.svg")
        nil)
    ([e] e)))

(cassert-eq "a degenerate embedding is refused"
            (not= nil degen-err) true)
(cassert-eq "the degenerate refusal names the placement at fault"
            (has? degen-err "spectral") true)
(cassert-eq "the degenerate refusal says what to do instead"
            (has? degen-err ":layout force") true)
(cassert-eq "the degenerate refusal left no file behind"
            (nil? (os/stat "/tmp/snark-test-degen.svg")) true)

(def degen-ok
  (try (do (to-svg degen-petersen :layout "lombardi" :base :spectral
                   :allow-coincident-nodes true
                   :width 20 :height 20
                   :path "/tmp/snark-test-degen-ok.svg")
           true)
       ([_] false)))

(cassert-eq ":allow-coincident-nodes draws it anyway"
            degen-ok true)

(cassert-eq "a sound embedding still draws"
            (try (do (to-svg degen-petersen :layout "lombardi"
                             :width 20 :height 20
                             :path "/tmp/snark-test-sound.svg")
                     true) ([_] false)) true)

(print "\n* export: per-element colour")

(def K4
  (do (def g (make-graph @{}))
      (each [u v] [["a" "b"] ["a" "c"] ["a" "d"]
                   ["b" "c"] ["b" "d"] ["c" "d"]]
        (add-edge g u v))
      g))

(when graphviz?
  (def svg-nc (to-svg K4 :path "/tmp/_snark-test-nc.svg"
                      :node-colors @{"a" "#ff0000"}))
  (cassert-eq ":node-colors reaches the graphviz file"
              (has? svg-nc "#ff0000") true)

  (def svg-ec (to-svg K4 :path "/tmp/_snark-test-ec.svg"
                      :label-edges false
                      :edge-colors @{(tuple "a" "b") "#0000ff"}))

  (cassert-eq ":edge-colors reaches the graphviz file"
              (has? svg-ec "#0000ff") true)

  (cassert-eq "empty colour tables leave the drawing unchanged"
              (= (to-svg K4 :path "/tmp/_snark-test-plain.svg"
                         :label-edges false)
                 (to-svg K4 :path "/tmp/_snark-test-plain2.svg"
                         :label-edges false
                         :node-colors @{} :edge-colors @{}))
              true))

(def lg (to-svg K4 :path "/tmp/_snark-test-lombardi.svg"
                :layout "lombardi"
                :node-colors @{"a" "#ff0000"}
                :edge-colors @{(tuple "a" "b") "#0000ff"}))

(cassert-eq "lombardi: :node-colors reaches the file"
            (has? lg "#ff0000") true)

(cassert-eq "lombardi: :edge-colors reaches the file"
            (has? lg "#0000ff") true)

(cassert-eq "lombardi: empty colour tables leave the drawing unchanged"
            (= (to-svg K4 :path "/tmp/_snark-test-lg2.svg"
                       :layout "lombardi")
               (to-svg K4 :path "/tmp/_snark-test-lg3.svg"
                       :layout "lombardi"
                       :node-colors @{} :edge-colors @{}))
            true)

(def lg-nasty
  (to-svg K4 :path "/tmp/_snark-test-lgn.svg" :layout "lombardi"
          :node-colors @{"a" "#ff \"00 00\""}))

(cassert-eq "lombardi: a quote inside a colour is escaped"
            (has? lg-nasty "&quot;") true)
(cassert-eq "lombardi: the escaped colour keeps the attribute intact"
            (has? lg-nasty "fill=\"#ff &quot;00 00&quot;\"") true)
(cassert-eq "lombardi: the raw quote does not reach the document"
            (has? lg-nasty "fill=\"#ff \"") false)

(print "\n* export: drawing-wide colour and pinned positions")

(defn- svg-colours
  "The hex colours in an SVG document, sorted."
  [svg]
  (def seen @{})
  (each line (string/split "\n" svg)
    (def at (string/find "\"#" line))
    (when at
      (def stop (string/find "\""
                             (string/slice line (+ at 1)
                                           (length line))))
      (put seen (string/slice line (+ at 1)
                              (+ at 1 stop)) true)))
  (sorted (keys seen)))

(when graphviz?
  (def colour-gv
    (to-svg K4 :path "/tmp/_snark-test-col.svg" :label-edges false
            :node-color "#ff0000" :edge-color "#00ff00"))

  (cassert-eq ":node-color reaches the graphviz rendering"
              (arr-contains (svg-colours colour-gv) "#ff0000") true)
  (cassert-eq ":edge-color reaches the graphviz rendering"
              (arr-contains (svg-colours colour-gv) "#00ff00") true)
  (cassert-eq ":node-color leaves to-dot's default behind"
              (arr-contains (svg-colours colour-gv) "#fff3cd") false)

  (cassert-eq ":node-opts :fillcolor beats :node-color"
              (arr-contains
                (svg-colours
                  (to-svg K4 :path "/tmp/_snark-test-col2.svg"
                          :label-edges false
                          :node-color "#ff0000"
                          :node-opts @{:fillcolor "#00ff00"}))
                "#00ff00") true)

  (cassert-eq ":node-color does not then override it"
              (arr-contains
                (svg-colours
                  (to-svg K4 :path "/tmp/_snark-test-col3.svg"
                          :label-edges false
                          :node-color "#ff0000"
                          :node-opts @{:fillcolor "#00ff00"}))
                "#ff0000") false)

  (def caller-opts @{:shape "circle"})

  (to-svg K4 :path "/tmp/_snark-test-col4.svg" :label-edges false
          :node-color "#ff0000" :node-opts caller-opts)

  (cassert-eq "the caller's :node-opts table is not modified"
              (length (keys caller-opts)) 1)

  (cassert-eq "with neither option the drawing is unchanged"
              (= (to-svg K4 :path "/tmp/_snark-test-col6.svg"
                         :label-edges false)
                 (to-svg K4 :path "/tmp/_snark-test-col7.svg"
                         :label-edges false
                         :node-opts @{} :edge-opts @{}))
              true))

(def colour-lg
  (to-svg K4 :path "/tmp/_snark-test-col5.svg" :layout "lombardi"
          :node-color "#ff0000" :edge-color "#00ff00"))

(cassert-eq "lombardi: :node-color and :edge-color are honoured"
            (and (arr-contains (svg-colours colour-lg) "#ff0000")
                 (arr-contains (svg-colours colour-lg) "#00ff00"))
            true)

(def ALL-PINS
  @{"a" [10.0 10.0] "b" [200.0 20.0]
    "c" [100.0 200.0] "d" [300.0 20.0]})

(defn- canvas-size
  [svg]
  (def at (string/find "viewBox=\"" svg))
  (def rest (string/slice svg (+ at 9) (length svg)))
  (def parts
    (string/split " " (string/slice rest 0 (string/find "\"" rest))))
  (scan-number (parts 2)))

(when graphviz?
  (def starts
    (to-svg K4 :path "/tmp/_snark-test-starts.svg"
            :label-edges false :node-positions ALL-PINS))
  (def pinned
    (to-svg K4 :path "/tmp/_snark-test-pin.svg"
            :label-edges false :node-positions ALL-PINS
            :fixed-positions true))

  (cassert-eq "without :fixed-positions the engine lays it out its own way"
              (not= (canvas-size starts) (canvas-size pinned)) true)

  (cassert-eq ":fixed-positions holds the drawing at the pinned size"
              (< (canvas-size pinned) 600.0) true)

  (cassert-eq ":fixed-positions is the same under a named engine"
              (= (canvas-size
                   (to-svg K4 :path "/tmp/_snark-test-pin2.svg"
                           :label-edges false :layout "circo"
                           :node-positions ALL-PINS
                           :fixed-positions true))
                 (canvas-size
                   (to-svg K4 :path "/tmp/_snark-test-pin3.svg"
                           :label-edges false :node-positions ALL-PINS
                           :fixed-positions true))) true)

  (cassert-eq ":position-scale does not rescale the caller's positions"
              (canvas-size
                (to-svg K4 :path "/tmp/_snark-test-pin4.svg"
                        :label-edges false :node-positions ALL-PINS
                        :fixed-positions true :position-scale 400))
              (canvas-size pinned))

  (def partial-err
    (try (to-svg K4 :path "/tmp/_snark-test-partial.svg"
                 :label-edges false
                 :node-positions @{"a" [10.0 10.0] "b" [20.0 20.0]}
                 :fixed-positions true)
         ([e] e)))

  (cassert-eq "a partial table with :fixed-positions is refused"
              (not= nil partial-err) true)
  (cassert-eq "the refusal says how many nodes are unpinned"
              (has? partial-err "2 of 4") true)
  (cassert-eq "the refusal names an unpinned node"
              (has? partial-err "Unpinned: c, d") true)
  (cassert-eq "the refusal says what to do instead"
              (has? partial-err "drop :fixed-positions") true)
  (cassert-eq "the partial refusal left no file behind"
              (nil? (os/stat "/tmp/_snark-test-partial.svg")) true)

  (def wrong-type
    (try (to-svg K4 :path "/tmp/_snark-test-wrongtype.svg"
                 :label-edges false :fixed-positions true
                 :node-positions @{"1" [1.0 1.0] "b" [2.0 2.0]
                                   "c" [3.0 3.0] "d" [4.0 4.0]})
         ([e] e)))

  (cassert-eq "keys of the wrong type count as no pin at all"
              (has? wrong-type "pins 3 of 4 nodes") true)
  (cassert-eq "the refusal says why they did not count"
              (has? wrong-type "name no node at all") true)
  (cassert-eq "the refusal states the key rule"
              (has? wrong-type "does not match the integer") true)

  (cassert-eq "a partial table without :fixed-positions is not an error"
              (try (do (to-svg K4 :path "/tmp/_snark-test-partial0.svg"
                               :label-edges false
                               :node-positions @{"a" [10.0 10.0]
                                                 "b" [20.0 20.0]})
                       true) ([_] false)) true))

(def partial-ok
  (try (do (to-svg K4 :path "/tmp/_snark-test-partial2.svg"
                   :label-edges false :layout "circle"
                   :node-positions @{"a" [10.0 10.0]})
           true)
       ([_] false)))

(cassert-eq "a partial :node-positions is fine with a library layout"
            partial-ok true)

(def big-lg
  (try (to-svg K4 :path "/tmp/_snark-test-big.svg" :layout "lombardi"
                :scale 100000.0)
       ([e] e)))

(cassert-eq "lombardi: :max-canvas is enforced there too"
            (not= nil big-lg) true)

(cassert-eq "lombardi: the refusal names the layout at fault"
            (has? big-lg "lombardi") true)

(cassert-eq "lombardi: the refusal left no file behind"
            (nil? (os/stat "/tmp/_snark-test-big.svg")) true)

(cassert-eq "lombardi: :max-canvas 0 disables the check"
            (try (do (to-svg K4 :path "/tmp/_snark-test-big2.svg"
                             :layout "lombardi"
                             :scale 100000.0 :max-canvas 0)
                     true)
                 ([_] false))
            true)

(each f ["/tmp/_snark-test-svg.svg" "/tmp/_snark-test-svg2.svg" "/tmp/_snark-test-kept.dot" "/tmp/snark-test-esc.svg" "/tmp/snark-test-esc.dot" "/tmp/snark-test-inside.svg" "/tmp/snark-test-inside.dot" "/tmp/snark-test-long.svg" "/tmp/snark-test-long.dot" "/tmp/snark-test-pos.svg" "/tmp/snark-test-pos.dot" "/tmp/snark-test-pos2.svg" "/tmp/snark-test-pos2.dot" "/tmp/snark-test-pad.svg" "/tmp/snark-sp-fixed.svg" "/tmp/snark-sp-fixed.dot" "/tmp/snark-sp-auto.svg" "/tmp/snark-sp-auto.dot" "/tmp/snark-sp-cap.svg" "/tmp/snark-sp-cap.dot" "/tmp/snark-sp-tiny.svg" "/tmp/snark-sp-tiny.dot" "/tmp/snark-mc.svg" "/tmp/snark-mc.dot" "/tmp/snark-mc2.svg" "/tmp/snark-mc2.dot" "/tmp/snark-mc3.svg" "/tmp/snark-mc3.dot" "/tmp/snark-mc4.svg" "/tmp/snark-mc4.dot" "/tmp/snark-warn.svg" "/tmp/snark-warn.dot" "/tmp/snark-warn3.svg" "/tmp/snark-warn3.dot" "/tmp/snark-test-degen.svg" "/tmp/snark-test-degen-ok.svg" "/tmp/snark-test-sound.svg" "/tmp/_snark-test-nc.svg" "/tmp/_snark-test-ec.svg" "/tmp/_snark-test-plain.svg" "/tmp/_snark-test-plain2.svg" "/tmp/_snark-test-lombardi.svg" "/tmp/_snark-test-lg2.svg" "/tmp/_snark-test-lg3.svg" "/tmp/_snark-test-lgn.svg" "/tmp/_snark-test-col.svg" "/tmp/_snark-test-col2.svg" "/tmp/_snark-test-col3.svg" "/tmp/_snark-test-col4.svg" "/tmp/_snark-test-col5.svg" "/tmp/_snark-test-col6.svg" "/tmp/_snark-test-col7.svg" "/tmp/_snark-test-starts.svg" "/tmp/_snark-test-pin.svg" "/tmp/_snark-test-pin2.svg" "/tmp/_snark-test-pin3.svg" "/tmp/_snark-test-pin4.svg" "/tmp/_snark-test-partial.svg" "/tmp/_snark-test-partial0.svg" "/tmp/_snark-test-partial2.svg" "/tmp/_snark-test-wrongtype.svg" "/tmp/_snark-test-big.svg" "/tmp/_snark-test-big2.svg"]
  (when (os/stat f) (os/rm f)))

(print-results)
