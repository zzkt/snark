# -*- mode: janet; -*-
# graphs/test/distance-matrix: dynamic distance matrix

(import ./helper :prefix "")
(import ../graphs :prefix "")

(print "\n* construction and accessors")


(def dm (make-distance-matrix @["a" "b" "c"]
                              @[@[0 2 9]
                                @[2 0 4]
                                @[9 4 0]]))

(cassert-eq "size square matrix" (distance-matrix-size dm) 3)
(cassert-arr-eq "names" (distance-matrix-names dm) @["a" "b" "c"])
(cassert-eq "get a<->b (0,1)" (distance-matrix-get dm 0 1) 2)
(cassert-eq "get symmetric b<->a (1,0)" (distance-matrix-get dm 1 0) 2)
(cassert-eq "get self a<->a (0,0)" (distance-matrix-get dm 0 0) 0)
(cassert-eq "get b<->c (1,2)" (distance-matrix-get dm 1 2) 4)

(print "\n* set")

(distance-matrix-set dm 0 2 5)
(cassert-eq "set adjusts symmetric pair" (distance-matrix-get dm 0 2) 5)
(cassert-eq "set is symmetric (2,0)" (distance-matrix-get dm 2 0) 5)

(print "\n* nearest-pair")

(def np (distance-matrix-nearest-pair dm))
# after set a-c=5, distances: a-b=2, a-c=5, b-c=4 -> nearest a-b
(cassert-eq "nearest-pair distance" (np 2) 2)
(cassert-eq "nearest-pair indices distinct" (not= (np 0) (np 1)) true)

(print "\n* from-fn")

(def by-fn (distance-matrix-from-fn @["x" "y" "z"] (fn [i j names]
  (if (= i j) 0 (math/abs (- i j))))))

(cassert-eq "from-fn size" (distance-matrix-size by-fn) 3)
(cassert-eq "from-fn x<->z distance" (distance-matrix-get by-fn 0 2) 2)
(cassert-eq "from-fn self" (distance-matrix-get by-fn 1 1) 0)

(print "\n* map")

(def dmx (distance-matrix-map dm (fn [v] (+ v 1))))
(cassert-eq "map shifts a<->b" (distance-matrix-get dmx 0 1) 3)
(cassert-eq "map shifts self too" (distance-matrix-get dmx 0 0) 1)
(cassert-eq "map does not mutate original" (distance-matrix-get dm 0 1) 2)

(print "\n* remove")

(def dmr (distance-matrix-remove dm @[1]))   # drop index 1 (b)
(cassert-eq "remove shrinks size" (distance-matrix-size dmr) 2)
(cassert-arr-eq "remove drops name b" (distance-matrix-names dmr) @["a" "c"])
(cassert-eq "remove keeps a<->c" (distance-matrix-get dmr 0 1) 5)

(print "\n* merge")

(def m1 (make-distance-matrix @["p" "q"] @[@[0 1]@[1 0]]))

# merge q (index 1) into p (index 0), new distances to p from [p] = [2]
(def merged (distance-matrix-merge m1 0 1 @[2]))
(cassert-eq "merge reduces size to n-1"
            (distance-matrix-size merged) 1)
(cassert-arr-eq "merge produces a+b-style name"
                (distance-matrix-names merged) @["p+q"])

(print "\n* to-table")

(def tbl (distance-matrix-to-table dm))
(cassert-eq "to-table has a<->b key" (get tbl "a<->b") 2)
(cassert-eq "to-table has a<->c key" (get tbl "a<->c") 5)
(cassert-eq "to-table only stores i<j (no b<->a)" (nil? (get tbl "b<->a")) true)

(print-results)
