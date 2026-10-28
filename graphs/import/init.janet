# -*- mode: janet; -*-
# graphs/import: various graph importers

(import ./adjacency :prefix "ia_")
(import ./dot :prefix "id_")
(import ./gexf :prefix "ig_")
(import ./hog :prefix "ih_")

(def graph-from-adjacency-list ia_graph-from-adjacency-list)
(def graph-from-adjacency-matrix ia_graph-from-adjacency-matrix)
(def graph-from-dot id_graph-from-dot)
(def graph-from-gexf ig_graph-from-gexf)
(def graph-from-hog-json ih_graph-from-hog-json)
(def import-dot id_import-dot)
(def import-gexf ig_import-gexf)
(def import-hog ih_import-hog)


(defn import-file
  ```Import a graph from `path`, choosing the reader from its extension.

Recognises `.dot` and `.gexf`, and raises an error on anything else rather than
returning an empty graph. Any extra `args` are forwarded to the reader,
`:weighted`, `:directed`, `:labels` and `:multigraph` all work as expected with
`import-dot` and `import-gexf`.
```
  [path & args]
  (if (string/has-suffix? ".dot" path)
    (import-dot path ;args)
    (if (string/has-suffix? ".gexf" path)
      (import-gexf path ;args)
      (error (string "import-file: unsupported graph import format for " path)))))
