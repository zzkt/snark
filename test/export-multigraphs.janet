# -*- mode: janet; -*-
# graphs/test/export: multigraphs to-dot, to-gexf, to-cytoscape

(import ./helper :prefix "")
(import ../graphs :prefix "")
(import ../graphs/export/common :prefix "ec-")
(import ../graphs/util :prefix "u-")

# multiplicity: three parallel edges must stay three edges
(def mg (make-multigraph))
(add-edges mg ["a" "b"] ["a" "b"] ["a" "b"])

(cassert-eq "multigraph of three parallel edges stays three in to-gexf"
            (length (string/find-all "<edge " (to-gexf mg))) 3)
(cassert-eq "multigraph parallel edges each carry unit weight"
            (length (string/find-all "weight=\"1.0000\"" (to-gexf mg))) 3)
(cassert-eq "multigraph does not report the count as a weight"
            (length (string/find-all "weight=\"3" (to-gexf mg))) 0)
(cassert-eq "multigraph parallel edges stay separate in to-dot"
            (length (string/find-all "\"a\" -- \"b\"" (to-dot mg))) 3)

#  to-tikz on multigraphs

(def tk-mg (make-multigraph))
(add-nodes tk-mg "a" "b")
(add-edge tk-mg "a" "b")

(cassert-eq "to-tikz: a multigraph with one edge exports"
            (= (type (to-tikz tk-mg)) :string) true)
(cassert-eq "to-tikz: a multigraph is written undirected"
            (not= nil (string/find "a --" (to-tikz tk-mg))) true)

(def tk-par (make-multigraph))
(add-nodes tk-par "a" "b")
(add-edges tk-par ["a" "b"] ["a" "b"] ["a" "b"])

(cassert-eq "to-tikz: parallel edges do not crash the chain walk"
            (= (type (to-tikz tk-par)) :string) true)
(cassert-eq "to-tikz: both endpoints still present"
            (not= nil (string/find "b;" (to-tikz tk-par))) true)

(def tk-md (make-multidigraph))
(add-nodes tk-md "a" "b")
(add-edge tk-md "a" "b")
(cassert-eq "to-tikz: a multidigraph exports"
            (= (type (to-tikz tk-md)) :string) true)
(cassert-eq "to-tikz: a multidigraph is written directed"
            (not= nil (string/find "a ->" (to-tikz tk-md))) true)

(def tk-mdp (make-multidigraph))
(add-nodes tk-mdp "a" "b")
(add-edge tk-mdp "a" "b")
(add-edge tk-mdp "a" "b")
(cassert-eq "to-tikz: a multidigraph with parallel edges exports"
            (= (type (to-tikz tk-mdp)) :string) true)

#  :multidigraph via the exporters

(cassert-eq "multidigraph: a :multidigraph really has no :adj"
            (do (def md0 (make-multidigraph))
                (add-nodes md0 "a" "b") (add-edge md0 "a" "b")
                (nil? (get md0 :adj))) true)
(cassert-eq "multidigraph: to-dot emits the edge"
            (do (def md1 (make-multidigraph))
                (add-nodes md1 "a" "b")  (add-edge md1 "a" "b")
                (not= nil (string/find "\"a\" -> \"b\"" (to-dot md1)))) true)
(cassert-eq "multidigraph: to-gexf emits the edge"
            (do (def md2 (make-multidigraph))
                (add-nodes md2 "a" "b")  (add-edge md2 "a" "b")
                (= (length (string/find-all "<edge " (to-gexf md2))) 1)) true)
(cassert-eq "multidigraph: to-cytoscape emits the edge"
            (do (def md3 (make-multidigraph))
                (add-nodes md3 "a" "b") (add-edge md3 "a" "b")
                (not= nil (string/find "source" (to-cytoscape md3)))) true)
(cassert-eq "multidigraph: to-tikz emits the edge"
            (do (def md4 (make-multidigraph))
                (add-nodes md4 "a" "b")  (add-edge md4 "a" "b")
                (not= nil (string/find "a ->" (to-tikz md4)))) true)


#  each-edge and multigraph? asserted at the seam

(defn ee-edges [g directed?]
  (def out @[])
  (ec-each-edge g directed? (fn [u v w] (array/push out [u v w])))
  out)

(each spec [["graph" (make-graph @{})]
            ["digraph" (make-digraph @[] @{})]
            ["multigraph" (make-multigraph @{})]
            ["multidigraph" (make-multidigraph @{} @{})]
            ["weighted-graph" (make-weighted-graph @[] @{})]]
  (def g (spec 1))
  (add-node g "a") (add-node g "b")
  (add-edge g "a" "b")
  (cassert-eq (string "multigraph?: a " (spec 0) " is one")
              (ec-multigraph? g)
              (or (= (string (spec 0)) "multigraph")
                  (= (string (spec 0)) "multidigraph")))
  (cassert-eq (string "each-edge: a " (spec 0) " reports its one edge once")
              (length (ee-edges g (u-structurally-directed? g))) 1))

# multiplicity is expanded into repeated unit edges
(def ee-m (make-multigraph))
(add-nodes ee-m "a" "b")
(add-edges ee-m ["a" "b"] ["a" "b"] ["a" "b"])
(cassert-eq "each-edge: three parallel edges report three times"
            (length (ee-edges ee-m (u-structurally-directed? ee-m))) 3)
(cassert-eq "each-edge: and each is weight 1, not the count"
            (all (fn [e] (= (e 2) 1))
                 (ee-edges ee-m (u-structurally-directed? ee-m))) true)


# what do the exporters do with a multigraph's multiplicity?

(defn- edge-count-of [g]
  (var n 0)
  (ec-each-edge g true (fn [u v w] (++ n)))
  n)

(defn- count-in [hay needle]
  (var n 0)
  (var start 0)
  (while true
    (def at (string/find needle hay start))
    (if (nil? at) (break) (do (++ n) (set start (+ at 1)))))
  n)

(def mg3 (make-multidigraph @{} @{}))
(add-nodes mg3 "a" "b" "c")
(add-edges mg3 ["a" "b"] ["a" "b"] ["b" "c"])
(cassert-eq "the graph has three edges"
            (edge-count-of mg3) 3)

(cassert-eq "to-dot keeps both parallel edges"
            (count-in (to-dot mg3) " -> ") 3)

(cassert-eq "to-tikz collapses parallel edges into one step"
            (count-in (to-tikz mg3) "->[") 2)

(cassert-eq "to-cytoscape folds multiplicity into a weight"
            (count-in (to-cytoscape mg3) "\"weight\":2") 1)

(cassert-eq "to-cytoscape writes one element per distinct pair"
            (count-in (to-cytoscape mg3) "\"source\"") 2)


(print-results)
