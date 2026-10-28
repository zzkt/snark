# -*- mode: janet; -*-
# graphs/test/export: to-cytoscape

(import ./helper :prefix "")
(import ../graphs :prefix "")

(defn- has?
  [hay needle]
  (not= nil (string/find needle hay)))

(defn- misses?
  [hay needle]
  (nil? (string/find needle hay)))

(print "\n* export: to-cytoscape")

(def dg (make-digraph @[] @{}))
(digraph-add-edge dg "a" "b" 1)
(digraph-add-edge dg "b" "c" 2)

(def html (to-cytoscape dg))
(cassert-eq "html doctype" (string/find "<!DOCTYPE html>" html) 0)
(cassert-eq "title tag" (has? html "<title>Graph</title>") true)
(cassert-eq "elements json" (has? html "elements: [") true)
(cassert-eq "node element" (has? html "\"id\":\"a\"") true)
(cassert-eq "edge element" (has? html "\"source\":\"a\"") true)
(cassert-eq "edge weight" (has? html "\"weight\":1") true)
(cassert-eq "directed arrows in style"
            (has? html "target-arrow-shape") true)
(cassert-eq "container div" (has? html "id=\"cy\"") true)

(cassert-eq ":title"
            (has? (to-cytoscape dg :title "My Graph")
                  "<title>My Graph</title>") true)
(cassert-eq ":height"
            (has? (to-cytoscape dg :height "800px")
                  "height: 800px") true)
(cassert-eq ":layout"
            (has? (to-cytoscape dg :layout "dagre")
                  "layout: { name: 'dagre' }") true)
(cassert-eq ":directed? false omits arrows"
            (misses? (to-cytoscape dg :directed? false)
                     "target-arrow-shape") true)
(cassert-eq ":label-edges false hides the weight label"
            (misses? (to-cytoscape dg :label-edges false)
                     "data(weight)") true)

(def fcose
  "https://unpkg.com/cytoscape-fcose@2.2.0/cytoscape-fcose.js")
(cassert-eq ":scripts appended"
            (has? (to-cytoscape dg :scripts @[fcose]) fcose) true)

(def sbgn-style (to-cytoscape dg :style "cytoscapeSbgnStylesheet(cytoscape)"))
(cassert-eq "string style verbatim"
            (has? sbgn-style
                  "style: cytoscapeSbgnStylesheet(cytoscape)") true)
(cassert-eq "table style as json"
            (has? (to-cytoscape dg :style @[{:selector "node"}])
                  "\"selector\":\"node\"") true)

(def enriched
  (to-cytoscape dg
                :node-data @{"a" @{:class "macromolecule"}}
                :edge-data @{(tuple "a" "b")
                             @{:class "catalysis" :cardinality 0}}))
(cassert-eq "node-data merged"
            (has? enriched "\"class\":\"macromolecule\"") true)
(cassert-eq "edge-data merged"
            (has? enriched "\"class\":\"catalysis\"") true)
(cassert-eq "edge-data cardinality"
            (has? enriched "\"cardinality\":0") true)

(def positioned
  (to-cytoscape dg :layout "preset"
                :node-positions @{"a" @[100 200] "b" @[300.5 400.25]}))
(cassert-eq "preset layout"
            (has? positioned "layout: { name: 'preset' }") true)
(cassert-eq "positions emitted"
            (has? positioned "\"position\":{\"x\":100") true)
(cassert-eq "positions keep the fractional y"
            (has? positioned "\"y\":400.25") true)
(cassert-eq "no positions without the option"
            (misses? (to-cytoscape dg) "\"position\":") true)

(def hostile-title (to-cytoscape dg :title "<b>alert</b>"))
(cassert-eq "title escaped"
            (has? hostile-title
                  "<title>&lt;b&gt;alert&lt;/b&gt;</title>") true)
(cassert-eq "title raw tag not emitted"
            (misses? hostile-title "<title><b>alert</b></title>") true)

(def breakout
  (to-cytoscape dg
                :node-data @{"a" @{:label "</script><b>pwnd</b>"}}))
(cassert-eq "json </script> hardened"
            (has? breakout "<\\/script>") true)
(cassert-eq "no raw tag from data"
            (misses? breakout "<b>pwnd</b>") true)

(def empty-html (to-cytoscape (make-digraph @[] @{})))
(cassert-eq "empty graph page"
            (string/find "<!DOCTYPE html>" empty-html) 0)
(cassert-eq "empty elements json"
            (has? empty-html "elements: []") true)

# The graph's own edge-data is read by default.
(def anno (make-digraph @[] @{}))
(add-nodes anno "a" "b")
(add-edge anno "a" "b" 1)
(set-node-data anno "a" @{:class "Source"})
(set-edge-data anno "a" "b" @{:class "catalysis"})
(def anno-cy (to-cytoscape anno))
(cassert-eq "cytoscape: node-data from the graph is merged"
            (has? anno-cy "\"class\":\"Source\"") true)
(cassert-eq "cytoscape: edge-data from the graph is merged"
            (has? anno-cy "\"class\":\"catalysis\"") true)
(cassert-eq "cytoscape: an unannotated node carries no label junk"
            (has? anno-cy "\"id\":\"b\"}") true)
(cassert-eq "cytoscape: :node-data @{} suppresses it"
            (misses? (to-cytoscape anno :node-data @{}) "Source") true)
(cassert-eq "cytoscape: :edge-data @{} suppresses it"
            (misses? (to-cytoscape anno :edge-data @{}) "catalysis") true)
(cassert-eq "cytoscape: an explicit option still wins"
            (has? (to-cytoscape anno
                               :node-data @{"a" @{:tag "explicit"}})
                  "explicit") true)

(def mg3 (make-multidigraph @{} @{}))
(add-nodes mg3 "a" "b" "c")
(add-edges mg3 ["a" "b"] ["a" "b"] ["b" "c"])
(cassert-eq "to-cytoscape folds multiplicity into a weight"
            (length (string/find-all "\"weight\":2" (to-cytoscape mg3))) 1)
(cassert-eq "to-cytoscape writes one element per distinct pair"
            (length (string/find-all "\"source\"" (to-cytoscape mg3))) 2)
(cassert-eq "multidigraph: to-cytoscape emits the edge"
            (not= nil (string/find "source"
                                   (do (def md3 (make-multidigraph @{} @{}))
                                       (add-nodes md3 "a" "b")
                                       (add-edge md3 "a" "b")
                                       (to-cytoscape md3))))
            true)

(print-results)
