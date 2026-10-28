# -*- mode: janet; -*-
# graphs/test/heap tests: binary min-heap priority queue

(import ./helper :prefix "")
(import ../graphs :prefix "")

(print "\n* heap construction and min behavior *")

(def h (make-binary-min-heap :items @[5 3 8 1]))
(cassert-eq "peek returns smallest" (heap-peek h) 1)
(cassert-eq "size" (heap-size h) 4)
(cassert-eq "pop removes smallest first" (heap-pop h) 1)
(cassert-eq "pop removes next smallest" (heap-pop h) 3)
(cassert-eq "pop removes next smallest" (heap-pop h) 5)
(cassert-eq "pop removes largest last" (heap-pop h) 8)
(cassert-eq "size 0 after draining" (heap-size h) 0)
(cassert-eq "pop on empty returns nil" (heap-pop h) nil)

(print "\n* heap ordering property *")

(def h2 (make-binary-min-heap))
(heap-push h2 10)
(heap-push h2 1)
(heap-push h2 7)
(heap-push h2 4)
(cassert-eq "size after 4 pushes" (heap-size h2) 4)
(cassert-eq "peek after pushes is min" (heap-peek h2) 1)

# draining yields sorted ascending order
(def drained @[])
(while (> (heap-size h2) 0)
  (array/push drained (heap-pop h2)))
(def expected @[1 4 7 10])
(cassert-eq "drain order is ascending (same as sorted)"
            (and (= (length drained) (length expected))
                 (all (fn [i] (= (drained i) (expected i)))
                      (range (length drained)))) true)

(print "\n* custom compare (max-heap)")

(def mx (make-binary-min-heap :items @[2 9 5] :compare |(< $1 $0)))
(cassert-eq "max-heap peek is largest" (heap-peek mx) 9)
(cassert-eq "max-heap pop is largest" (heap-pop mx) 9)


(print "\n* heap with composite items")

(def pairs (make-binary-min-heap :items @[[3 "c"] [1 "a"] [2 "b"]]
                                 :compare (fn [x y] (< (x 0) (y 0)))))

(def first-pair (heap-pop pairs))

(cassert-eq "composite heap orders by first element" (first-pair 0) 1)
(cassert-eq "composite heap keeps payload" (first-pair 1) "a")

(print-results)
