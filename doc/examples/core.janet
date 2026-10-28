(import graphs :prefix "")

# create a new empty graph
(def g (make-graph))

# add some nodes and edges
(add-nodes g "a" "b" "c" "d")
(add-edges g ["a" "b"] ["a" "c"] ["b" "c"] ["c" "d"])

(printf "nodes: %p\nedges: %p" (node-count g) (edge-count g))
(printf "degree(a) = %p" (degree g "a"))
(printf "neighbours(a) = %s" (string/join (map string (neighbours g "a")) ", "))

(def d (make-digraph @["s" "t"] @{"s" @{"t" 3}}))

(printf "digraph with %p nodes, edge s->t weight is %p"
        (node-count d) (edge-weight d "s" "t"))
