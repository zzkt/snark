# Examples


(import graphs :prefix "")



# A path of four nodes. one component, three bridges, two cut vertices.


(def g (make-graph))
(add-nodes g "a" "b" "c" "d")
(add-edges g ["a" "b"] ["b" "c"] ["c" "d"])

(printf "components: %p" (length (connected-components g)))
(printf "bridges: %p" (length (bridges g)))
(printf "articulation points: %s"
        (string/join (sort (map string (articulation-points g))) " "))



# #+RESULTS:
# : components: 1
# : bridges: 3
# : articulation points: b c

# LCA on a rooted tree where =r= is the root, =a= and =b= are its children, and =c= is a child of =a=


(def t (make-digraph))
(add-nodes t "r" "a" "b" "c")
(add-edges t ["r" "a" 1] ["r" "b" 1] ["a" "c" 1])

(def ans (tarjan-lca t "r" @[["c" "b"] ["c" "a"]]))

(printf "LCA(c,b) = %s, LCA(c,a) = %s" (get ans ["c" "b"]) (get ans ["c" "a"]))
