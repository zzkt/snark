(import graphs :prefix "")

(def g (graph-from-adjacency-list @[@[1 2] @[0 2] @[0 1]]
                                  :labels @["a" "b" "c"]))

(printf "nodes: %p, edges: %p" (node-count g) (edge-count g))
(printf "labels: %s" (string/join (map string (nodes g)) " "))
