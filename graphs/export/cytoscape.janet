# -*- mode: janet; -*-
# graphs/export/cytoscape: cytoscape.js HTML export

(import deft :prefix "")
(import ../record :prefix "rec-")
(import spork/json :prefix "json/")
(import ../types :prefix "")
(import ../util :prefix "")
(import ./common :prefix "")

# ==============================================================================
#  URL to use for cytoscape.js
# ==============================================================================

(def *cytoscape-url*
  "https://cdn.jsdelivr.net/npm/cytoscape@3.30.2/dist/cytoscape.min.js")


# ==============================================================================
#  HTML export
# ==============================================================================

(defn- escape-script [s]
  (string/replace-all "</" "<\\/" s))

(defn- html-escape
  "Escape &, <, > for text embedding in HTML."
  [s]
  (string/replace-all ">" "&gt;"
    (string/replace-all "<" "&lt;"
      (string/replace-all "&" "&amp;" (string s)))))


(defn- cytoscape-elements
 ```Convert a graph `g` to cytoscape.js elements.

Each element is a {data {...}} table. nodes have :id, edges have :source,
:target, and an optional numeric :weight.

Directed graphs emit each stored edge once; undirected graphs dedupe with u < v.
node-data is keyed by node id, edge-data by (tuple source target). node-positions
maps a node id to a [x y] array, used as the element's cytoscape position.
```
  [g directed? &opt node-extra edge-extra node-positions]
  (let [nodes (if node-extra node-extra @{})
        edges (if edge-extra edge-extra @{})
        positions (if node-positions node-positions @{})
        result @[]
        buf @""]
    (defn emit-id [x]
      (def s (string x))
      (buffer/push-string buf (string (string/format "%08x" (length s)) s)))
    # nodes
    (each u (rec-nodes g)
      (buffer/push-string buf "N")
      (emit-id u))
    # edges.
    (loop [[u tbl] :pairs (out-adjacency g)]
      (loop [[v w] :pairs tbl]
        (when (or directed? (< u v))
          (buffer/push-string buf "E")
          (emit-id u)
          (emit-id v)
          (if (number? w)
            (emit-id w)
            (buffer/push-string buf "00000000")))))
    (def stream (string buf))
    # PEG patterns: lenprefix, number, and capture
    (def hex8p ~(repeat 8 (set "0123456789abcdef")))
    (def lenpat ~(number ,hex8p 16))
    (def join-caps (fn [& caps] (string ;caps)))
    (def idpat ~(cmt (lenprefix ,lenpat (capture 1)) ,join-caps))
    (def emit-cb (fn [& xs]
      (if (= (length xs) 1)
        (do
          (def d @{:id (xs 0)})
          (def nextra (get nodes (xs 0)))
          (when (table? nextra) (merge-into d nextra))
          (def p (get positions (xs 0)))
          (if p
            (array/push result {:data d :position @{:x (p 0) :y (p 1)}})
            (array/push result {:data d})))
        (do
          (def d @{:source (xs 0) :target (xs 1)})
          (def eextra (get edges (tuple (xs 0) (xs 1))))
          (when (table? eextra) (merge-into d eextra))
          (def ws (xs 2))
          (when (pos? (length ws))
            (put d :weight (scan-number ws)))
          (array/push result {:data d})))
      true))
    (def elpat
      ~(cmt (choice (sequence "N" ,idpat)
                    (sequence "E" ,idpat ,idpat ,idpat)) ,emit-cb))
    (def gpat ~(* (any ,elpat) -1))
    (peg/match gpat stream)
    result))

(defn- cytoscape-style
  "Default cytoscape.js style sheet for nodes and edges."
  [directed? label-edges]
  (def style
    @[@{:selector "node"
        :style @{:background-color "#61bffc"
                 :color "#333333"
                 :label "data(id)"
                 :text-valign "center"
                 :text-halign "center"
                 :width 32
                 :height 32}}
      @{:selector "edge"
        :style @{:line-color "#9ca3af"
                 :curve-style "bezier"
                 :width 2}}])
  (when directed?
    (put (get-in style [1 :style]) :target-arrow-shape "triangle")
    (put (get-in style [1 :style]) :target-arrow-color "#9ca3af"))
  (when label-edges
    (put (get-in style [1 :style]) :label "data(weight)"))
  style)


(define to-cytoscape
  ```Export a graph `g` to a self-contained HTML page using cytoscape.js.

The graph is serialized to a length-prefixed byte stream and parsed into
cytoscape elements. The HTML page only requires cytoscape.js as dependency.

 &keys
  :title — page <title> (default "Graph")
  :layout — cytoscape layout name, e.g. "cose", "fcose", "dagre" (default "cose")
            Layouts other than "cose" require extra scripts installed.
            see: examples/cytoscape-export.org.

  :height — container CSS height (default "600px")

  :directed? — force directed edge arrows (default: auto-detected)

  :label-edges — show edge weights as edge labels (default true)

  :cytoscape-url — override the cytoscape.js <script src> (defaults to 3.30.2)

  :scripts — array of extra <script src> URLs for cytoscape extensions
             (e.g. fcose, dagre, sbgn-stylesheet) which are loaded after the
             core cytoscape.js script

  :style — override the inline style. Either a table/array (as JSON) or a
           string used verbatim as a JS expression.

  :node-data — table of node id -> extra data fields merged into the node.
               Defaults to the graph's own node-data, pass @{} to suppress
               e.g. @{"n1" @{:label "foo"}}

  :edge-data — table of (tuple source target) -> extra data fields merged.
               Defaults to the graph's own edge-data, pass @{} to suppress.
               e.g. @{(tuple "a" "b") @{:class "production"}}

  :node-positions — table of node id -> [x y] array of cytoscape coordinates
                    emitted as each node element's top-level position.
                    Use :layout "preset" to use fixed coordinates.


Returns an HTML string suitable for writing to a .html file.
```
  [g :weighted & keys] :string
  (def opts (table ;keys))
  (def title (get opts :title "Graph"))
  (def layout (get opts :layout "cose"))
  (def height (get opts :height "600px"))
  (def directed? (get opts :directed? (structurally-directed? g)))
  (def label-edges (get opts :label-edges true))
  (def cy-url
    (get opts :cytoscape-url *cytoscape-url*))
  (def scripts (get opts :scripts @[]))
  (def style-override (get opts :style))
  (def style-json
    (cond
      (string? style-override) style-override
      (buffer? style-override) (string style-override)
      (nil? style-override)
      (escape-script (json/encode (cytoscape-style directed? label-edges)))
      (escape-script (json/encode style-override))))
  (def node-data (if (has-key? opts :node-data)
                    (get opts :node-data)
                    (get g :node-data)))
  (def edge-data (if (has-key? opts :edge-data)
                    (get opts :edge-data)
                    (get g :edge-data)))
  (def node-positions (get opts :node-positions @{}))
  (def json-elements
    (json/encode (cytoscape-elements g directed? node-data edge-data node-positions)))
  # keep `</script>` out of the JSON string
  (def safe-elements (escape-script json-elements))
  (def layout-js (string/replace-all "'" "\\'" layout))
  (def buf @"")
  (buffer/push-string buf "<!DOCTYPE html>\n")
  (buffer/push-string buf "<html lang=\"en\">\n<head>\n")
  (buffer/push-string buf "  <meta charset=\"utf-8\">\n")
  (buffer/push-string buf (string "  <title>" (html-escape title) "</title>\n"))
  (buffer/push-string buf (string "  <script src=\"" cy-url "\"></script>\n"))
  (each s scripts
    (buffer/push-string buf (string "  <script src=\"" (html-escape s) "\"></script>\n")))
  (buffer/push-string buf
    (string "  <style>#cy { width: 100%; height: " height "; display: block; }</style>\n"))
  (buffer/push-string buf "</head>\n<body>\n")
  (buffer/push-string buf "  <div id=\"cy\"></div>\n")
  (buffer/push-string buf "  <script>\n")
  (buffer/push-string buf "    var cy = cytoscape({\n")
  (buffer/push-string buf "      container: document.getElementById('cy'),\n")
  (buffer/push-string buf (string "      elements: " safe-elements ",\n"))
  (buffer/push-string buf (string "      style: " style-json ",\n"))
  (buffer/push-string buf (string "      layout: { name: '" layout-js "' }\n"))
  (buffer/push-string buf "    });\n")
  (buffer/push-string buf "  </script>\n")
  (buffer/push-string buf "</body>\n</html>\n")
  (string buf))
