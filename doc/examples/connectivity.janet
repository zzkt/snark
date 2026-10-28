(import graphs :prefix "")

(def g (make-graph))
(add-nodes g ["a" "b" "c" "d"])
(add-edges g [["a" "b"] ["b" "c"] ["c" "d"]])

(printf "components: %p" (length (connected-components g)))
(printf "bridges: %p" (length (bridges g)))
(printf "articulation points: %s"
        (string/join (sort (map string (articulation-points g))) " "))

(def t (make-digraph))
(add-nodes t ["r" "a" "b" "c"])
(add-edges t [["r" "a" 1] ["r" "b" 1] ["a" "c" 1]])

(def ans (tarjan-lca t "r" @[["c" "b"] ["c" "a"]]))

(printf "LCA(c,b) = %s, LCA(c,a) = %s" (get ans ["c" "b"]) (get ans ["c" "a"]))
