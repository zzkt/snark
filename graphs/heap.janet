# -*- mode: janet; -*-
# graphs/heap: Binary min-heap priority queue

(import deft :prefix "")

(defn- _default-cmp [a b] (< a b))

(defn- _sift-down
  [data _cmp n idx]
  (var i idx)
  (while true
    (def left (+ (* i 2) 1))
    (def right (+ (* i 2) 2))
    (var smallest i)
    (when (and (< left n)
               (_cmp (data left) (data smallest)))
      (set smallest left))
    (when (and (< right n)
               (_cmp (data right) (data smallest)))
      (set smallest right))
    (if (= smallest i) (break))
    (def tmp (data i))
    (put data i (data smallest))
    (put data smallest tmp)
    (set i smallest)))

(defn- _sift-up
  [data _cmp idx]
  (var i idx)
  (while (> i 0)
    (def parent (brshift (- i 1) 1))
    (if (_cmp (data i) (data parent))
      (do
        (def tmp (data i))
        (put data i (data parent))
        (put data parent tmp)
        (set i parent))
      (break))))

(defn- _heap-abides?
  ```True when `data` satisfies the min-heap property under `_cmp`.```
  [data _cmp]
  (var n (length data))
  (var ok true)
  (var i 0)
  (while (and ok (< i n))
    (def left (+ (* i 2) 1))
    (def right (+ (* i 2) 2))
    (when (and (< left n) (_cmp (data left) (data i)))
      (set ok false))
    (when (and ok (< right n) (_cmp (data right) (data i)))
      (set ok false))
    (++ i))
  ok)


(deftrecord :binary-min-heap
  (optional data :array @[])
  (optional compare :function _default-cmp)
  (guard
    (fn [h]
      (def items (get h :items))
      (if items
        (do
          (def data (array/slice items))
          (def _cmp (or (get h :compare) _default-cmp))
          (var n (length data))
          (var i (brshift n 1))
          (while (>= i 0)
            (_sift-down data _cmp n i)
            (-- i))
          (put h :data data)
          (put h :items nil)
          (if (_heap-abides? data _cmp)
            true
            (error "binary-min-heap: heap property violated after construction")))
        true))))

(define heap-push
  [h :binary-min-heap item]
  (def data (h :data))
  (array/push data item)
  (_sift-up data (h :compare) (- (length data) 1)))

(define heap-pop
  [h :binary-min-heap]
  (def data (h :data))
  (def n (length data))
  (if (= n 0) nil
    (do
      (def result (data 0))
      (put data 0 (data (- n 1)))
      (array/pop data)
      (_sift-down data (h :compare) (length data) 0)
      result)))

(define heap-peek
  [h :binary-min-heap]
  ((h :data) 0))

(define heap-size
  [h :binary-min-heap] :number
  (length (h :data)))
