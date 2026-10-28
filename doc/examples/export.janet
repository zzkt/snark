(import graphs :prefix "")

(def w (make-digraph))
(add-nodes w ["a" "b"])
(add-edges w [["a" "b" 1]])

(print (to-dot w))

(def t (make-weighted-graph))
(add-nodes t ["root" "a" "b"])
(add-edges t [["root" "a" 2] ["root" "b" 1]])

(print (to-tikz t :layout "phylogram" :root "root" :label-edges false))
