# -*- mode: janet; -*-
# test/data/order: canonical string forms for order-independent results.

(defn norm-edge
  "An edge pair as \"x/y\" with the smaller label first."
  [e]
  (def x (e 0))
  (def y (e 1))
  (if (< (string x) (string y)) (string x "/" y) (string y "/" x)))

(defn set-str
  "The elements as a comma-joined string, in the order given."
  [xs]
  (string/join (map string (or xs @[])) ","))

(defn sorted-str
  "The elements sorted, then comma-joined."
  [xs]
  (string/join (sort (map string (or xs @[]))) ","))

(defn edge-set-str
  "A set of edges as sorted, orientation-normalised \"x/y\" pairs."
  [edges]
  (string/join (sort (map norm-edge edges)) ","))

(defn biconn-str
  "A list of biconnected components, each as a set of edges, sorted as semicolon-separated groups."
  [groups]
  (string/join
    (sort (map (fn [g] (string/join (sort (map norm-edge g)) "|")) groups))
    ";"))

(defn joined-str
  "The elements sorted, joined with `sep`. For a vertex set that is keyed by a string."
  [xs sep]
  (string/join (sort (map string (or xs @[]))) sep))
