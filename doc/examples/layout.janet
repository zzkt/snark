# Examples


(import graphs :prefix "")



# A small DAG ranked via the TALA pipeline


(def dag (make-digraph))
(add-nodes dag "s" "b1" "b2" "c1" "c2" "t")
(add-edges dag [["s" "b1"] ["s" "b2"] ["b1" "c1"]
                ["b2" "c2"] ["c1" "t"] ["c2" "t"]])

(def res (layout-dag dag))
(printf "tala eligible? %p  levels: %p positioned: %p"
        (res :eligible) (res :level-count) (length (res :pos)))



# #+RESULTS:
# : tala eligible? true  levels: 4 positioned: 6


(def tb (graph-layout dag "dag" :direction "TB"))
(def lr (graph-layout dag "dag" :direction "LR"))
(printf "TB eligible? %p\nLR eligible? %p" (tb :eligible) (lr :eligible))

(def ring (make-digraph))
(add-nodes ring "n1" "n2" "n3" "n4")
(add-edges ring [["n1" "n2"] ["n2" "n3"] ["n3" "n4"] ["n4" "n1"]])

(printf "ring force eligible? %p" ((graph-layout ring "force") :eligible))
(printf "ring circle eligible? %p" ((graph-layout ring "circle") :eligible))
(printf "unknown layout => %p" (try (graph-layout ring "wonky") ([e] e)))



# #+RESULTS:
# : TB eligible? true
# : LR eligible? true
# : ring force eligible? true
# : ring circle eligible? true
# : unknown layout => "graph-layout: unknown layout 'wonky'"

# The phylogram family draws weighted trees as rectilinear dendrograms


(def t (make-weighted-graph))
(add-nodes t "root" "a" "b" "c")
(add-edges t ["root" "a" 2] ["root" "b" 1] ["b" "c" 3])

(each name ["phylogram" "phylogenetic-tree" "unrooted-phylogram"]
  (def r (graph-layout t name :root "root"))
  (printf "%s => eligible? %p, positioned: %p" name (r :eligible) (length (r :pos))))
