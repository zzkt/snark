(import graphs :prefix "")

(def g (make-graph))
(add-nodes g ["a" "b" "c" "d" "e"])
(add-edges g [["a" "b"] ["b" "c"] ["a" "d"] ["b" "e"]])

(printf "bfs from a: %p" (string/join (map string (bfs g "a")) "-"))
(printf "connected? %p" (connected? g))

(def d (make-digraph))
(add-nodes d ["a" "b" "c" "d" "e"])
(add-edges d [["a" "b" 1] ["b" "c" 1]
              ["c" "a" 1] ["c" "d" 1] ["d" "e" 1]])

(printf "SCCs: %p components" (length (kosaraju-scc d)))
(printf "semi-connected? %p" (semi-connected? d))
(printf "topological order (d): %p" (topological-sort d))

(def d2 (make-digraph))
(add-nodes d2 ["a" "b" "c" "d" "e"])
(add-edges d2 [["a" "b" 1] ["a" "c" 1]
               ["b" "c" 1] ["b" "d" 1]
               ["c" "d" 1] ["c" "e" 1]])

(printf "topological order (d2): %p" (topological-sort d2))
