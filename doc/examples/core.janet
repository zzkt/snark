# Constructing graphs


(import graphs :prefix "")



# Every graph type has a constructor which takes zero or more parameters. Nodes (vertices) and edges can be added individually with =add-node= and/or =add-edge=. To add multiple nodes/edges to a graph, it can be more convenient to use =add-nodes= and/or =add-edges=.


# create a new empty graph
(def g (make-graph))

# add some nodes and edges
(add-nodes g "a" "b" "c" "d")
(add-edges g ["a" "b"] ["a" "c"] ["b" "c"] ["c" "d"])

(printf "nodes: %p\nedges: %p" (node-count g) (edge-count g))
(printf "degree(a) = %p" (degree g "a"))
(printf "neighbours(a) = %s" (string/join (map string (neighbours g "a")) ", "))



# #+RESULTS:
# : nodes: 4
# : edges: 4
# : degree(a) = 2
# : neighbours(a) = b, c

# The constructors can accept initialisation data, which is often the tersest option for small graphs.


(def d (make-digraph @["s" "t"] @{"s" @{"t" 3}}))

(printf "digraph with %p nodes, edge s->t weight is %p"
        (node-count d) (edge-weight d "s" "t"))
