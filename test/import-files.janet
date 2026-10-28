# -*- mode: janet; -*-
# graphs/test/import-files: dot and GEXF import from files

(import ./helper :prefix "")
(import ../graphs/import :prefix "")
(import ../graphs/util :prefix "")

#  fixtures

(def DOT-TEXT
  ```digraph G {
  a -> b [weight=2];
  b -> c;
}```)

(def GEXF-TEXT
  ```<?xml version="1.0" encoding="UTF-8"?>
<gexf xmlns="http://www.gexf.net/1.2draft" version="1.2">
  <graph defaultedgetype="directed">
    <nodes>
      <node id="a" label="A"/>
      <node id="b" label="B"/>
      <node id="c" label="C"/>
    </nodes>
    <edges>
      <edge id="0" source="a" target="b" weight="2"/>
      <edge id="1" source="b" target="c"/>
    </edges>
  </graph>
</gexf>```)

(def dir (string "/tmp/snark-import-files-" (os/time) "-" (math/random)))
(os/mkdir dir)

(defn write-fixture [name text]
  (def path (string dir "/" name))
  (spit path text)
  path)

(def dot-path (write-fixture "g.dot" DOT-TEXT))
(def gexf-path (write-fixture "g.gexf" GEXF-TEXT))
(def bogus-path (write-fixture "g.txt" "not a graph format"))

(defn cleanup! []
  (each f (os/dir dir) (try (os/rm (string dir "/" f)) ([_] nil)))
  (try (os/rm dir) ([_] nil)))

(defn adj-of [g u]
  (string/join (sort (map string (neighbours g u))) ","))

(defn edge-strings [g]
  (def out @[])
  (each u (nodes g)
    (each v (neighbours g u)
      (array/push out (string u ">" v))))
  (string/join (sort out) " "))

(defn weight-of [g u v] (get (edge-weights g u) v))

#  import-dot

(print "\n* import-dot")

(cassert-eq "import-dot: reads the file"
  (length (nodes (import-dot dot-path))) 3)
(cassert-eq "import-dot: agrees with graph-from-dot on the same text"
  (edge-strings (import-dot dot-path))
  (edge-strings (graph-from-dot DOT-TEXT)))
(cassert-eq "import-dot: both edges are present"
  (edge-strings (import-dot dot-path)) "a>b b>c")
(cassert-eq "import-dot: takes direction from the digraph header"
  (string (adj-of (import-dot dot-path) "a") "|" (adj-of (import-dot dot-path) "b"))
  "b|c")
(cassert-eq "import-dot: :directed false overrides the header"
  (string (adj-of (import-dot dot-path :directed false) "a") "|"
          (adj-of (import-dot dot-path :directed false) "b"))
  "b|a,c")
(cassert-eq "import-dot: without :weighted the edge weighs 1"
  (weight-of (import-dot dot-path) "a" "b") 1.0)
(cassert-eq "import-dot: :weighted reaches the parser"
  (weight-of (import-dot dot-path :weighted true) "a" "b") 2.0)


#  import-gexf

(print "\n* import-gexf")

(cassert-eq "import-gexf: reads the file"
  (length (nodes (import-gexf gexf-path))) 3)
(cassert-eq "import-gexf: agrees with graph-from-gexf on the same text"
  (edge-strings (import-gexf gexf-path))
  (edge-strings (graph-from-gexf GEXF-TEXT)))
(cassert-eq "import-gexf: both edges are present"
  (edge-strings (import-gexf gexf-path)) "a>b b>c")
(cassert-eq "import-gexf: takes direction from defaultedgetype"
  (string (adj-of (import-gexf gexf-path) "a") "|"
          (adj-of (import-gexf gexf-path) "b"))
  "b|c")
(cassert-eq "import-gexf: :directed false overrides the file"
  (string (adj-of (import-gexf gexf-path :directed false) "a") "|"
          (adj-of (import-gexf gexf-path :directed false) "b"))
  "b|a,c")
(cassert-eq "import-gexf: a weight survives by default"
  (weight-of (import-gexf gexf-path) "a" "b") 2.0)

#  import-file

(print "\n* import-file")

(cassert-eq "import-file: a .dot file is recognised"
  (edge-strings (import-file dot-path)) "a>b b>c")
(cassert-eq "import-file: a .gexf file is recognised"
  (edge-strings (import-file gexf-path)) "a>b b>c")

(cassert-eq "import-file: a .dot goes to the DOT reader"
  (edge-strings (import-file dot-path)) (edge-strings (graph-from-dot DOT-TEXT)))
(cassert-eq "import-file: a .gexf goes to the GEXF reader"
  (edge-strings (import-file gexf-path)) (edge-strings (graph-from-gexf GEXF-TEXT)))

# Options should survive the dispatch, not just the file read.
(cassert-eq "import-file: forwards options to the reader it chose"
  (string (adj-of (import-file gexf-path :directed false) "a") "|"
          (adj-of (import-file gexf-path :directed false) "b"))
  "b|a,c")
(cassert-eq "import-file: forwards :weighted to the DOT reader"
  (weight-of (import-file dot-path :weighted true) "a" "b") 2.0)

# An unknown extension raises an error
(def unsupported (try (import-file bogus-path) ([e] (string e))))
(cassert-eq "import-file: an unknown extension raises rather than answering empty"
  (not= nil (string/find "unsupported graph import format" (string unsupported))) true)
(cassert-eq "import-file: the message names the file it refused"
  (not= nil (string/find "g.txt" (string unsupported))) true)

# A non-existent path raises an error
(cassert-eq "import-file: a missing file raises"
  (= (try (import-file (string dir "/nope.dot")) ([_] "raised")) "raised") true)

(cleanup!)

(print-results)
