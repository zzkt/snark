(import graphs :prefix "")

(def g (make-graph))
(add-nodes g "a" "b" "c" "d")
(add-edges g ["a" "b"] ["b" "c"] ["c" "a"] ["c" "d"])

(printf "has cycle? %p" (has-cycle? g))
(printf "cycle basis size: %p" (length (graph-cycle-basis g)))

(def sq (make-graph))
(add-nodes sq "a" "b" "c" "d")
(add-edges sq ["a" "b"] ["b" "c"] ["c" "d"] ["d" "a"] ["a" "c"])

(def basis (graph-cycle-basis sq))
(printf "basis (%p cycles): %s" (length basis)
        (string/join (map |(string/join (map string $) "-") basis) "  "))
