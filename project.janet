(declare-project
 :name "snark"
 :description "Graphs and networks."
 :author "nik gaffney <nik@fo.am>"
 :url "https://codeberg.org/zzkt/snark"
 :license "EUPL-1.2"
 :version "0.7.1"
 :dependencies ["https://codeberg.org/zzkt/deft"]
 :source-paths ["graphs"]
 :test-paths ["test"])

(declare-source
 :source ["graphs/init.janet"
          "graphs/record.janet"
          "graphs/centrality.janet"
          "graphs/clustering.janet"
          "graphs/colouring.janet"
          "graphs/connectivity.janet"
          "graphs/cst.janet"
          "graphs/cuts.janet"
          "graphs/cores.janet"
          "graphs/cycles.janet"
          "graphs/dag.janet"
          "graphs/digraph.janet"
          "graphs/distance-matrix.janet"
          "graphs/flow.janet"
          "graphs/hamiltonian.janet"
          "graphs/heap.janet"
          "graphs/lca.janet"
          "graphs/matching.janet"
          "graphs/metrics.janet"
          "graphs/multigraph.janet"
          "graphs/pagerank.janet"
          "graphs/petri-net.janet"
          "graphs/planarity.janet"
          "graphs/types.janet"
          "graphs/random-graph.janet"
          "graphs/sequence.janet"
          "graphs/shortest-paths.janet"
          "graphs/similarity.janet"
          "graphs/simple-graph.janet"
          "graphs/spanning-tree.janet"
          "graphs/suffix-tree.janet"
          "graphs/tournament.janet"
          "graphs/traversal.janet"
          "graphs/tree.janet"
          "graphs/util.janet"
          "graphs/weighted-graph.janet"]
 :prefix "graphs")

(declare-source
 :source ["graphs/layout/init.janet"
          "graphs/layout/circle.janet"
          "graphs/layout/common.janet"
          "graphs/layout/dag.janet"
          "graphs/layout/force.janet"
          "graphs/layout/orthogonal.janet"
          "graphs/layout/phylo.janet"
          "graphs/layout/polyarc.janet"
          "graphs/layout/radial.janet"
          "graphs/layout/spectral.janet"
          "graphs/layout/spring.janet"
          "graphs/layout/tree.janet"]
 :prefix "graphs/layout")

(declare-source
 :source ["graphs/import/adjacency.janet"
          "graphs/import/dot.janet"
          "graphs/import/gexf.janet"
          "graphs/import/hog.janet"
          "graphs/import/init.janet"]
 :prefix "graphs/import")

(declare-source
 :source ["graphs/export/common.janet"
          "graphs/export/cytoscape.janet"
          "graphs/export/dot.janet"
          "graphs/export/gexf.janet"
          "graphs/export/petri-net.janet"
          "graphs/export/init.janet"
          "graphs/export/svg.janet"
          "graphs/export/tikz.janet"]
 :prefix "graphs/export")

(phony "benchmark" []
       (def benchmarks
         @["cst-benchmark.janet"
           "graph-types-benchmark.janet"
           "graphs-benchmark.janet"
           "lca-benchmark.janet"
           "suffix-tree-benchmark.janet"])
       (each t benchmarks
         (os/shell (string/format "janet benchmark/%s" t))))

(phony "docs" []
       (os/shell "janet doc/tangle.janet")
       (os/shell "janet test/doc-examples.janet")
       (os/shell "janet test/examples.janet"))
