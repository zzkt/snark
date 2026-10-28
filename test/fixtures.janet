# -*- mode: janet; -*-
# graphs/test/fixtures: data/graphs and data/order -> fixtures used for other tests

(import ./helper :prefix "")
(import ../graphs :prefix "")
(import ./data/graphs :prefix "fx-")
(import ./data/order :prefix "")

(defn edges-of
  "Every edge of g, each unordered pair reported once, as a sorted array."
  [g]
  (def seen @{})
  (each u (keys (g :adj))
    (each v (keys (get (g :adj) u))
      (put seen (norm-edge [u v]) true)))
  (sort (keys seen)))

(defn edge-count [g]
  (length (edges-of g)))

# name, graph, nodes, undirected edges.
(each spec [["path 4" (fx-path 4) 4 3]
            ["path 4, alpha labels" (fx-path 4 fx-alpha->name) 4 3]
            ["path 4, integer labels" (fx-path 4 fx-idx->int) 4 3]
            ["chain 4" (fx-chain 4) 4 3]
            ["cycle 4" (fx-cycle 4) 4 4]
            ["star 3" (fx-star 3) 4 3]
            ["complete 4" (fx-complete 4) 4 6]
            ["staircase 6" (fx-staircase 6) 6 9]
            ["layered 3x2" (fx-layered 3 2) 6 8]
            ["diamond 5" (fx-diamond 5) 5 6]
            ["tree 2" (fx-tree 2) 13 12]]
  (def g (spec 1))

  (cassert-eq (string "fixture " (spec 0) ": node count")
    (length (g :nodes)) (spec 2))

  (cassert-eq (string "fixture " (spec 0) ": edge count")
    (edge-count g) (spec 3))

  (cassert-eq (string "fixture " (spec 0) ": deterministic")
    (deep= (g :nodes) ((spec 1) :nodes)) true))

# exact edge sets
(cassert-eq "fixture path 4: edges"
  (string/join (edges-of (fx-path 4)) ",") "0/1,1/2,2/3")

(cassert-eq "fixture complete 4: every pair"
  (string/join (edges-of (fx-complete 4)) ",") "0/1,0/2,0/3,1/2,1/3,2/3")

(cassert-eq "fixture staircase 6: each node reaches the next two"
  (string/join (edges-of (fx-staircase 6)) ",") "0/1,0/2,1/2,1/3,2/3,2/4,3/4,3/5,4/5")

(cassert-eq "fixture layerd 3x2: every node joins the level above"
  (string/join (edges-of (fx-layered 3 2)) ",")
  "L0N0/L1N0,L0N0/L1N1,L0N1/L1N0,L0N1/L1N1,L1N0/L2N0,L1N0/L2N1,L1N1/L2N0,L1N1/L2N1")

(cassert-eq "fixture cycle 4: closes the ring"
  (string/join (edges-of (fx-cycle 4)) ",") "0/1,0/3,1/2,2/3")

(cassert-eq "fixture star 3: hub joined to every leaf"
  (string/join (edges-of (fx-star 3)) ",") "0/1,0/2,0/3")

(cassert-eq "fixture diamond 5: one source, one sink, three middles"
  (string/join (edges-of (fx-diamond 5)) ",") "m1/s,m1/t,m2/s,m2/t,m3/s,m3/t")

# labels used by callers
(cassert-eq "fixture path 4: default labels are the indices"
  (string/join (map string ((fx-path 4) :nodes)) ",") "0,1,2,3")

(cassert-eq "fixture path 4: a label function overrides them"
  (string/join (map string ((fx-path 4 fx-alpha->name) :nodes)) ",") "a,b,c,d")

(cassert-eq "fixture layered 3x2: L<level>N<index>"
  (string/join (map string ((fx-layered 3 2) :nodes)) ",")
  "L0N0,L0N1,L1N0,L1N1,L2N0,L2N1")

(cassert-eq "fixture diamond 5: s, m1..m3, t"
  (sorted-str (map string ((fx-diamond 5) :nodes))) "m1,m2,m3,s,t")

# from-pairs passes labels and pairs through untouched
(def fromp (fx-from-pairs @["a" "b" "c"] [["a" "b"] ["b" "c"]]))

(cassert-eq "fixture from-pairs: labels verbatim"
  (string/join (map string (fromp :nodes)) ",") "a,b,c")

(cassert-eq "fixture from-pairs: only the pairs given"
  (string/join (edges-of fromp) ",") "a/b,b/c")

# mutators are type-dispatched. one call, three graph types
(each spec [["undirected" (make-graph @{})]
            ["digraph" (make-digraph @[] @{})]
            ["weighted-graph" (make-weighted-graph @[] @{})]]
  (def g (add-edges (add-nodes (spec 1) 3) [["0" "1"] ["1" "2"]]))
  (cassert-eq (string "fixture add-nodes!/add-edges! on a " (spec 0) ": edges")
    (string/join (edges-of g) ",") "0/1,1/2"))

# seeded builders are deterministic
(cassert-eq "fixture lcg-dag: deterministic for a seed"
  (deep= ((fx-lcg-dag 8 20 7) :adj) ((fx-lcg-dag 8 20 7) :adj)) true)

(cassert-eq "fixture lcg-dag: a different seed gives a different graph"
  (deep= ((fx-lcg-dag 8 20 7) :adj) ((fx-lcg-dag 8 20 8) :adj)) false)

(cassert-eq "fixture lcg-graph: deterministic for a seed"
  (deep= ((fx-lcg-graph 8 20 7) :adj) ((fx-lcg-graph 8 20 7) :adj)) true)

# the two string forms
(cassert-eq "order: set-str keeps the order it is given, sorted-str does not"
  (string (set-str @["b" "a"]) " then " (sorted-str @["b" "a"])) "b,a then a,b")

(cassert-eq "order: norm-edge is orientation-independent"
  (and (= (norm-edge ["b" "a"]) "a/b") (= (norm-edge ["a" "b"]) "a/b")) true)

(cassert-eq "order: set-eq ignores order, and checks membership"
  (and (set-eq @["a" "b"] @["b" "a"]) (not (set-eq @["a" "b"] @["a" "c"]))) true)

(cassert-eq "order: edge-set-str normalises orientation but does not dedup"
  (edge-set-str [["1" "0"] ["2" "1"]]) "0/1,1/2")

(cassert-eq "order: biconn-str groups, and sorts inside and across"
  (biconn-str [[["1" "0"] ["0" "2"]] [["3" "4"]]]) "0/1|0/2;3/4")

(cassert-eq "order: joined-str sorts and uses the separator given"
  (joined-str ["c" "a" "b"] "/") "a/b/c")


(print-results)
