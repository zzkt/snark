# -*- mode: janet; -*-
# graphs/test/data/fixtures: named graphs for use in examples and testing

#  Each graph is a value (g-k3, g-c4, ...) with a description in the docstring.
#  The 'fixtures' table contains graphs which can be used to test the expected
#  results found in 'planar-status' and 'matching-size'.

(import ../../graphs :prefix "")

#  planar graphs

(def g-empty "empty graph: no vertices, no edges"
  (make-graph @{} @[]))

(def g-single-edge "one edge"
  (make-graph
    @{"x" @{"y" 1}
      "y" @{"x" 1}}
    @["x" "y"]))

(def g-k3 "complete graph K3 (triangle)"
  (make-graph
    @{"a" @{"b" 1 "c" 1}
      "b" @{"a" 1 "c" 1}
      "c" @{"a" 1 "b" 1}}
    @["a" "b" "c"]))

(def g-k4 "complete graph K4 (tetrahedron)"
  (make-graph
    @{"a" @{"b" 1 "c" 1 "d" 1}
      "b" @{"a" 1 "c" 1 "d" 1}
      "c" @{"a" 1 "b" 1 "d" 1}
      "d" @{"a" 1 "b" 1 "c" 1}}
    @["a" "b" "c" "d"]))

(def g-c4 "4-cycle"
  (make-graph
    @{"1" @{"2" 1 "4" 1}
      "2" @{"1" 1 "3" 1}
      "3" @{"2" 1 "4" 1}
      "4" @{"1" 1 "3" 1}}
    @["1" "2" "3" "4"]))

(def g-c5 "5-cycle"
  (make-graph
    @{"1" @{"2" 1 "5" 1}
      "2" @{"1" 1 "3" 1}
      "3" @{"2" 1 "4" 1}
      "4" @{"3" 1 "5" 1}
      "5" @{"1" 1 "4" 1}}
    @["1" "2" "3" "4" "5"]))

(def g-c6 "6-cycle"
  (make-graph
    @{"1" @{"2" 1 "6" 1}
      "2" @{"1" 1 "3" 1}
      "3" @{"2" 1 "4" 1}
      "4" @{"3" 1 "5" 1}
      "5" @{"4" 1 "6" 1}
      "6" @{"1" 1 "5" 1}}
    @["1" "2" "3" "4" "5" "6"]))

(def g-path4 "4-vertex path"
  (make-graph
    @{"a" @{"b" 1}
      "b" @{"a" 1 "c" 1}
      "c" @{"b" 1 "d" 1}
      "d" @{"c" 1}}
    @["a" "b" "c" "d"]))

(def g-cube "3-cube (Q3), V=8 E=12"
  (make-graph
    @{"a0" @{"a1" 1 "a2" 1 "a4" 1}
      "a1" @{"a0" 1 "a3" 1 "a5" 1}
      "a2" @{"a0" 1 "a3" 1 "a6" 1}
      "a3" @{"a1" 1 "a2" 1 "a7" 1}
      "a4" @{"a0" 1 "a5" 1 "a6" 1}
      "a5" @{"a1" 1 "a4" 1 "a7" 1}
      "a6" @{"a2" 1 "a4" 1 "a7" 1}
      "a7" @{"a3" 1 "a5" 1 "a6" 1}}
    @["a0" "a1" "a2" "a3" "a4" "a5" "a6" "a7"]))

(def g-octahedron "octahedron = K_{2,2,2}, V=6 E=12"
  (make-graph
    @{"0" @{"2" 1 "3" 1 "4" 1 "5" 1}
      "1" @{"2" 1 "3" 1 "4" 1 "5" 1}
      "2" @{"0" 1 "1" 1 "4" 1 "5" 1}
      "3" @{"0" 1 "1" 1 "4" 1 "5" 1}
      "4" @{"0" 1 "1" 1 "2" 1 "3" 1}
      "5" @{"0" 1 "1" 1 "2" 1 "3" 1}}
    @["0" "1" "2" "3" "4" "5"]))

(def g-wheel5 "wheel W5"
  (make-graph
    @{"c" @{"r0" 1 "r1" 1 "r2" 1 "r3" 1 "r4" 1}
      "r0" @{"c" 1 "r1" 1 "r4" 1}
      "r1" @{"c" 1 "r0" 1 "r2" 1}
      "r2" @{"c" 1 "r1" 1 "r3" 1}
      "r3" @{"c" 1 "r2" 1 "r4" 1}
      "r4" @{"c" 1 "r0" 1 "r3" 1}}
    @["c" "r0" "r1" "r2" "r3" "r4"]))

(def g-grid23 "2x3 grid"
  (make-graph
    @{"g00" @{"g01" 1 "g10" 1}
      "g01" @{"g00" 1 "g11" 1}
      "g10" @{"g00" 1 "g11" 1 "g20" 1}
      "g11" @{"g01" 1 "g10" 1 "g21" 1}
      "g20" @{"g10" 1 "g21" 1}
      "g21" @{"g11" 1 "g20" 1}}
    @["g00" "g01" "g10" "g11" "g20" "g21"]))

(def g-bowtie "two triangles sharing one vertex (articulation)"
  (make-graph
    @{"a" @{"b" 1 "s" 1}
      "b" @{"a" 1 "s" 1}
      "c" @{"d" 1 "s" 1}
      "d" @{"c" 1 "s" 1}
      "s" @{"a" 1 "b" 1 "c" 1 "d" 1}}
    @["s" "a" "b" "c" "d"]))

(def g-k3ring "triangle with an outer ring via a 3-path"
  (make-graph
    @{"a" @{"b" 1 "c" 1 "f" 1}
      "b" @{"a" 1 "c" 1}
      "c" @{"a" 1 "b" 1 "d" 1}
      "d" @{"c" 1 "e" 1}
      "e" @{"d" 1 "f" 1}
      "f" @{"a" 1 "e" 1}}
    @["a" "b" "c" "d" "e" "f"]))

(def g-tree7 "7-vertex tree (path & fork)"
  (make-graph
    @{"t0" @{"t1" 1}
      "t1" @{"t0" 1 "t2" 1}
      "t2" @{"t1" 1 "t3" 1}
      "t3" @{"t2" 1 "t4" 1 "t5" 1}
      "t4" @{"t3" 1}
      "t5" @{"t3" 1 "t6" 1}
      "t6" @{"t5" 1}}
    @["t0" "t1" "t2" "t3" "t4" "t5" "t6"]))


#  non-planar graphs

(def g-k5 "complete graph K5 (Kuratowski)"
  (make-graph
    @{"a" @{"b" 1 "c" 1 "d" 1 "e" 1}
      "b" @{"a" 1 "c" 1 "d" 1 "e" 1}
      "c" @{"a" 1 "b" 1 "d" 1 "e" 1}
      "d" @{"a" 1 "b" 1 "c" 1 "e" 1}
      "e" @{"a" 1 "b" 1 "c" 1 "d" 1}}
    @["a" "b" "c" "d" "e"]))

(def g-k6 "complete graph K6"
  (make-graph
    @{"0" @{"1" 1 "2" 1 "3" 1 "4" 1 "5" 1}
      "1" @{"0" 1 "2" 1 "3" 1 "4" 1 "5" 1}
      "2" @{"0" 1 "1" 1 "3" 1 "4" 1 "5" 1}
      "3" @{"0" 1 "1" 1 "2" 1 "4" 1 "5" 1}
      "4" @{"0" 1 "1" 1 "2" 1 "3" 1 "5" 1}
      "5" @{"0" 1 "1" 1 "2" 1 "3" 1 "4" 1}}
    @["0" "1" "2" "3" "4" "5"]))

(def g-k33 "complete bipartite K_{3,3} (Kuratowski)"
  (make-graph
    @{"a1" @{"b1" 1 "b2" 1 "b3" 1}
      "a2" @{"b1" 1 "b2" 1 "b3" 1}
      "a3" @{"b1" 1 "b2" 1 "b3" 1}
      "b1" @{"a1" 1 "a2" 1 "a3" 1}
      "b2" @{"a1" 1 "a2" 1 "a3" 1}
      "b3" @{"a1" 1 "a2" 1 "a3" 1}}
    @["a1" "a2" "a3" "b1" "b2" "b3"]))

(def g-k33-subdivision "K_{3,3} with one edge subdivided once"
  (make-graph
    @{"a1" @{"b1" 1 "b2" 1 "b3" 1}
      "a2" @{"b1" 1 "b2" 1 "b3" 1}
      "a3" @{"b1" 1 "b2" 1 "m" 1}
      "b1" @{"a1" 1 "a2" 1 "a3" 1}
      "b2" @{"a1" 1 "a2" 1 "a3" 1}
      "b3" @{"a1" 1 "a2" 1 "m" 1}
      "m" @{"a3" 1 "b3" 1}}
    @["a1" "a2" "a3" "b1" "b2" "b3" "m"]))

(def g-petersen "Petersen graph, class-2, non-planar"
  (make-graph
    @{"0" @{"1" 1 "4" 1 "5" 1}
      "1" @{"0" 1 "2" 1 "6" 1}
      "2" @{"1" 1 "3" 1 "7" 1}
      "3" @{"2" 1 "4" 1 "8" 1}
      "4" @{"0" 1 "3" 1 "9" 1}
      "5" @{"0" 1 "7" 1 "8" 1}
      "6" @{"1" 1 "8" 1 "9" 1}
      "7" @{"2" 1 "5" 1 "9" 1}
      "8" @{"3" 1 "5" 1 "6" 1}
      "9" @{"4" 1 "6" 1 "7" 1}}
    @["0" "1" "2" "3" "4" "5" "6" "7" "8" "9"]))

(def g-k5-pendant "K5 with a pendant edge"
  (make-graph
    @{"a" @{"b" 1 "c" 1 "d" 1 "e" 1}
      "b" @{"a" 1 "c" 1 "d" 1 "e" 1}
      "c" @{"a" 1 "b" 1 "d" 1 "e" 1}
      "d" @{"a" 1 "b" 1 "c" 1 "e" 1}
      "e" @{"a" 1 "b" 1 "c" 1 "d" 1 "f" 1}
      "f" @{"e" 1}}
    @["a" "b" "c" "d" "e" "f"]))


#  lookup & expectations for tests

(def fixtures
  "All named data graphs, keyed by keyword."
  (table
    :k3 g-k3 :k4 g-k4 :c4 g-c4 :c5 g-c5 :c6 g-c6
    :path4 g-path4 :cube g-cube :octahedron g-octahedron
    :wheel5 g-wheel5 :grid23 g-grid23 :bowtie g-bowtie
    :k3ring g-k3ring :tree7 g-tree7 :single-edge g-single-edge :empty g-empty
    :k5 g-k5 :k6 g-k6 :k33 g-k33 :k33-subdivision g-k33-subdivision
    :petersen g-petersen :k5-pendant g-k5-pendant))

(def planar-status
  "Expected is-planar? result per fixture keyword."
  (table
    :k3 true :k4 true :c4 true :c5 true :c6 true
    :path4 true :cube true :octahedron true
    :wheel5 true :grid23 true :bowtie true
    :k3ring true :tree7 true :single-edge true :empty true
    :k5 false :k6 false :k33 false :k33-subdivision false
    :petersen false :k5-pendant false))

(def matching-size
  "Expected maximum-matching size per fixture keyword."
  (table
    :k3 1 :k4 2 :c4 2 :c5 2 :c6 3
    :path4 2 :cube 4 :octahedron 3
    :wheel5 3 :grid23 3 :bowtie 2
    :k3ring 3 :tree7 3 :single-edge 1 :empty 0
    :k5 2 :k6 3 :k33 3 :k33-subdivision 3
    :petersen 5 :k5-pendant 3))
