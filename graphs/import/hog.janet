# -*- mode: janet; -*-
# graphs/import/hog: House of Graphs JSON importer

(import spork/json :prefix "json/")
(import ./adjacency :prefix "a_")
(import deft :prefix "")

(defn- fetch-json
  ```Fetch JSON data from `url` using curl.```
  [url]
  (def tmp (string "/tmp/snark-hog-" (os/time) "-" (math/random) ".json"))
  (def out (file/open tmp :w))
  (def rc (os/execute ["curl" "-fsSL" url] :p {:out out}))
  (file/close out)
  (when (not= rc 0)
    (try (os/rm tmp) ([_] nil))
    (error (string "House of Graphs request failed: " url)))
  (def result (try
                (json/decode (slurp tmp) true true)
                ([e] (error (string "House of Graphs JSON error: " e)))))
  (try (os/rm tmp) ([_] nil))
  result)


(defn- graph-from-hog-data
  "Build a graph, or return a raw matrix, from House of Graphs data."
  [data opts]
  (let [adjacency (get data :adjacencyList)
        matrix (get data :adjacencyMatrix)
        labels (get opts :labels)]
    (if (get opts :matrix false)
      (do
        (unless matrix (error "House of Graphs response has no adjacencyMatrix"))
        matrix)
      (do
        (unless adjacency (error "House of Graphs response has no adjacencyList"))
        (a_graph-from-adjacency-list
         adjacency
         :directed (get opts :directed false)
         :weighted (get opts :weighted false)
         :multigraph (get opts :multigraph false)
         :labels labels)))))


(defn graph-from-hog-json
  ```Import a House of Graphs graph from JSON text.

Generates a graph from the JSON body of a House of Graphs API response.

Options
  :directed, :weighted, :multigraph, and :labels are passed to the converter.
  :matrix true returns the decoded adjacency matrix instead of a graph.

Returns a graph or adjacency matrix.
```
  [text & args]
  (let [opts (table ;args)
        data (try
               (json/decode text true true)
               ([e] (error (string "House of Graphs JSON error: " e))))]
    (graph-from-hog-data data opts)))


(define import-hog
  ```Import a House of Graphs graph by numeric id.

Fetches a graph from https://houseofgraphs.org/api/graphs/<id> with curl.
To import from an existing JSON file, use `graph-from-hog-json`.

Options
  :directed, :weighted, :multigraph, and :labels are passed to the converter.
  :matrix true returns the decoded adjacency matrix instead of a graph.

Returns a graph or adjacency matrix.
```
  [id :integer & args] :any-graph
  (let [opts (table ;args)
        data (fetch-json (string "https://houseofgraphs.org/api/graphs/" id))]
    (graph-from-hog-data data opts)))
