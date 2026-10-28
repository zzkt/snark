# Example


(import graphs :prefix "")



# These predicates take a bare degree sequence, no graph is constructed.


(printf "[3 3 3 3 3 3] graphical? %p" (is-graphical? @[3 3 3 3 3 3]))

(printf "[3 3 2 1] graphical? %p" (is-graphical? @[3 3 2 1]))

(printf "[1 1 1 1] (C4) graphical? %p [multi] %p"
        (is-graphical? @[1 1 1 1]) (is-multigraphical? @[3 3 2 2 2]))
