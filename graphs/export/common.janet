# -*- mode: janet; -*-
# graphs/export/common: shared export

(import deft :prefix "")
(import ../util :prefix "")
(import ../record :prefix "rec-")

(defn multigraph?
  "True when the graph type stores an edge multiplicity rather than a weight."
  [g] :boolean
  (typecase-strict g
    :multigraph true
    :multidigraph true
    (fn [_] false)))

(defn out-adjacency
  "The out-adjacency table: :adj for most types, :out for :multidigraph."
  [g] (rec-adjacency g))


(defn each-edge
  "Iterate over edges, reporting each edge of the graph exactly once."
  [g directed? f]
  (let [multi? (multigraph? g)
        adj (out-adjacency g)]
    (def dir? (or directed? (get g :parent)))
    (loop [[u tbl] :pairs adj]
      (loop [[v w] :pairs tbl]
        (when (or dir? (<= u v))
          (if multi?
            (loop [_ :range [0 (or w 1)]] (f u v 1))
            (f u v w)))))))

(defn q
  "Wrap a string in double quotes for DOT/XML safety."
  [s] (string "\"" (string/replace-all "\"" "\\\"" (string s)) "\""))


(defn edge-colour-key
  ```Return the key used to lookup a per-edge colour for the edge u-v.

Undirected use `canonical-edge-key`, directed uses the natural ordered pair,
since (u v) and (v u) are distinct arcs.```
  [directed? u v]
  (if directed? (tuple u v) (canonical-edge-key u v)))

(defn xq
  "XML-escape a string and wrap it in double quotes (element text or an attribute)"
  [s] (string "\"" (xml-escape s) "\""))
