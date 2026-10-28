(import graphs :prefix "")

(def h (make-binary-min-heap))
(heap-push h 3) (heap-push h 1) (heap-push h 2)

(printf "min-heap pops: %s"
        (string/join (map string [(heap-pop h) (heap-pop h) (heap-pop h)]) " "))
