# -*- mode: janet; -*-
# graphs/export/gexf: GEXF (Gephi) XML export

(import deft :prefix "")
(import ../record :prefix "rec-")
(import ../types :prefix "")
(import ../util :prefix "")
(import ./common :prefix "")

# ==============================================================================
#  GEXF export
# ==============================================================================

(defn- gexf-edge-type
  [directed?]
  (if directed? "directed" "undirected"))

(defn- gexf-color-attrs
  "Return viz:color XML from a table containing :r, :g, :b keys, or nil."
  [opts]
  (let [r (get opts :r)
        g (get opts :g)
        b (get opts :b)]
    (when (and r g b)
      (string "        <viz:color r=" (q (string r)) " g=" (q (string g))
              " b=" (q (string b)) "/>\n"))))


(defn- gexf-size-attrs
  "Return viz:size XML from a table containing :size key, or nil."
  [opts]
  (def s (get opts :size))
  (when s
    (string "        <viz:size value=" (q (string s)) "/>\n")))

(defn- gexf-position-attrs
  "Return viz:position XML from a table containing :x, :y keys, or nil."
  [opts]
  (let [x (get opts :x)
        y (get opts :y)
        z (get opts :z)]
    (when (and x y)
      (def buf @"")
      (buffer/push-string buf "        <viz:position x="
                          (q (string x)) " y=" (q (string y)))
      (when z (buffer/push-string buf " z=" (q (string z))))
      (buffer/push-string buf "/>\n")
      (string buf))))

(defn- gexf-attvalues
  ```Return `<attvalues>` XML for a table of `fields`, or nil.```
  [fields]
  (when (and (table? fields) (not (empty? fields)))
    (var buf @"")
    (buffer/push-string buf "        <attvalues>\n")
    (loop [[k v] :pairs fields]
      (buffer/push-string buf
                          (string "          <attvalue for=" (xq (string k))
                                  " value=" (xq (string v)) "/>\n")))
    (buffer/push-string buf "        </attvalues>\n")
    (string buf)))


(define to-gexf
  ```Export a :digraph or :weighted-graph to GEXF XML format string.

 &keys
   :name       — graph label (default "G")
   :precision  — decimal places for weights (default 4)
   :node-opts  — table of default node viz options
                 e.g. @{:r 100 :g 150 :b 200 :size 12}
                 Accepts :r, :g, :b (0-255), :size (float), :x, :y, :z (float)
   :edge-opts  — table of default edge viz options
                 e.g. @{:r 200 :g 100 :b 100 :thickness 2.0}
                 Accepts :r, :g, :b (0-255), :thickness (float)
   :graph-opts — table of graph-level attributes
                 e.g. @{:mode "static" :idtype "string"}
                 Accepts :mode, :idtype, :defaultedgetype
   :node-data  — node id -> fields, written as <attvalues>. Defaults to the
                 graph's own node-data. Pass @{} to suppress.
   :edge-data  — (tuple source target) -> fields. Defaults to the graph's
                 own edge-data. Undirected keys are canonical.
                 All parallel edges of a multigraph share one key.

Returns a string suitable for writing to a .gexf file.
```
  [g :weighted & keys] :string
   (def opts (table ;keys))
  (def graph-name (get opts :name "G"))
  (def prec (get opts :precision 4))
  (def node-opts (get opts :node-opts @{}))
  (def edge-opts (get opts :edge-opts @{}))
  (def graph-opts (get opts :graph-opts @{}))
  (def node-data (if (has-key? opts :node-data)
                   (get opts :node-data)
                   (get g :node-data)))
  (def edge-data (if (has-key? opts :edge-data)
                   (get opts :edge-data)
                   (get g :edge-data)))
  (def gexf-mode (get graph-opts :mode "static"))
  (def gexf-idtype (get graph-opts :idtype))
  (def directed? (structurally-directed? g))
  (def gexf-edgetype (get graph-opts
                          :defaultedgetype (gexf-edge-type directed?)))
  (def seen-nodes (table))
  (def fmt (string "%." prec "f"))
  (def buf @"")
  (buffer/push-string buf "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
  (buffer/push-string buf "<gexf xmlns=\"http://gexf.net/1.3\"\n")
  (buffer/push-string buf "      xmlns:viz=\"http://gexf.net/1.3/viz\"\n")
  (buffer/push-string buf "      version=\"1.3\">\n")
  (buffer/push-string buf "  <meta>\n")
  (buffer/push-string buf
                      (string "    <description>"
                              (xml-escape graph-name) "</description>\n"))
  (buffer/push-string buf "  </meta>\n")
  (buffer/push-string buf (string "  <graph mode=" (q gexf-mode)
                                  " defaultedgetype=" (q gexf-edgetype)))
  (when gexf-idtype
    (buffer/push-string buf (string " idtype=" (q gexf-idtype))))
  (buffer/push-string buf ">\n")
  (buffer/push-string buf "    <nodes>\n")
  (each u (rec-nodes g)
    (unless (get seen-nodes u)
      (put seen-nodes u true)
      (buffer/push-string
       buf (string "      <node id=" (xq u) " label=" (xq u) ">\n"))
      (buffer/push-string
       buf (string (or (gexf-size-attrs node-opts)
                       "        <viz:size value=\"10\"/>\n")))
      (buffer/push-string
       buf (string (or (gexf-color-attrs node-opts)
                       "        <viz:color r=\"153\" g=\"153\" b=\"255\"/>\n")))
      (buffer/push-string
       buf (string (or (gexf-position-attrs node-opts) "")))
      (buffer/push-string
       buf (string (or (gexf-attvalues (get node-data u nil)) "")))
      (buffer/push-string buf "      </node>\n")))
  (buffer/push-string buf "    </nodes>\n")
  (buffer/push-string buf "    <edges>\n")
  (var edge-id 0)
  (each-edge g directed?
             (fn [u v w]
               (def wn (if (number? w) w 1))
               (do
                 (buffer/push-string
                  buf (string "      <edge id=" (q (string edge-id))
                              " source=" (xq u) " target=" (xq v)))
                 (def av (gexf-attvalues (get edge-data (tuple u v) nil)))
                 (if av
                   (do
                     (buffer/push-string buf ">\n")
                     (buffer/push-string buf av)
                     (buffer/push-string buf "      </edge>\n"))
                   (buffer/push-string
                    buf (string " weight=" (q (string/format fmt wn)) "/>\n")))
                 (++ edge-id))))
  (buffer/push-string buf "    </edges>\n")
  (buffer/push-string buf "  </graph>\n")
  (buffer/push-string buf "</gexf>\n")
  (string buf))
