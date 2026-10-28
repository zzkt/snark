# -*- mode: janet; -*-
# graphs/distance-matrix: Labelled distance matrix

(import deft :prefix "")

(deftrecord :distance-matrix
  (field names :array)
  (field matrix :array))

(define distance-matrix-size
  "Number of terminals in the distance matrix."
  [dm :distance-matrix] :integer
  (length (dm :names)))

(define distance-matrix-get
  "Entry [i, j] of the symmetric distance matrix."
  [dm :distance-matrix i :integer j :integer] :number
  (get-in (dm :matrix) [i j]))

(define distance-matrix-set
  "Set entries [i, j] and [j, i] to val (symmetric assignment)."
  [dm :distance-matrix i :integer j :integer val :number] nil
  (put ((dm :matrix) i) j val)
  (put ((dm :matrix) j) i val))

(define distance-matrix-names
  "Terminal labels, in matrix row order."
  [dm :distance-matrix] :array
  (dm :names))

(define distance-matrix-nearest-pair
  "Returns (i j distance) of the closest distinct terminal pair, or nil."
  [dm :distance-matrix] (or :array :nil)
  (def m (dm :matrix))
  (def n (length m))
  (if (< n 2)
    nil
    (do
      (var best-i 0)
      (var best-j 1)
      (var best-val ((m 0) 1))
      (for i 0 n
        (def row (m i))
        (for j (+ i 1) n
          (def val (row j))
          (when (< val best-val)
            (set best-i i)
            (set best-j j)
            (set best-val val))))
      @[best-i best-j best-val])))

(defn- kept-indices
  "Indices 0..n-1 not in the excluded set."
  [n exclusions]
  (def excl @{})
  (each x exclusions (put excl x true))
  (seq [i :range [0 n] :unless (get excl i)] i))

(defn- names-at
  "Labels at the given matrix indices."
  [dm indices]
  (map |((dm :names) $) indices))

(define distance-matrix-remove
  "Return a new matrix with the given row/col indices removed."
  [dm :distance-matrix indices :array] :distance-matrix
  (let [n (distance-matrix-size dm)
        _keep (kept-indices n indices)
        new-names (names-at dm _keep)]
    (def new-matrix (seq [ki :in _keep]
                     (seq [kj :in _keep]
                       (distance-matrix-get dm ki kj))))
    (make-distance-matrix new-names new-matrix)))

(define distance-matrix-merge
  ```Return a new matrix from `dm` merging rows/cols `i` and `j`.```
  [dm :distance-matrix i :integer j :integer new-dists :array] :distance-matrix
  (let [n (distance-matrix-size dm)
        _keep (kept-indices n (seq [k :in [i j]] k))]
    (def new-names (array/concat
                     (names-at dm _keep)
                     @[(string ((dm :names) i) "+" ((dm :names) j))]))
    (let [ki-len (length _keep)
          top-rows (seq [ki-idx :range [0 ki-len]]
                    (def ki (_keep ki-idx))
                    (array/concat
                      (seq [kj-idx :range [0 ki-len]]
                        (distance-matrix-get dm ki (_keep kj-idx)))
                      @[(new-dists ki-idx)]))
          merged-row (array/concat (array/slice new-dists) @[0.0])
          new-matrix (array/concat top-rows @[merged-row])]
      (make-distance-matrix new-names new-matrix))))

(define distance-matrix-from-fn
 ```Build a matrix from `names` and a distance function `(f i j names)`.```
  [names :array f :function] :distance-matrix
  (def n (length names))
  (def matrix (seq [i :range [0 n]]
                (seq [j :range [0 n]]
                  (f i j names))))
  (make-distance-matrix names matrix))

(define distance-matrix-map
  ```Build a matrix by applying `f` to each entry in matrix `dm`.```
  [dm :distance-matrix f :function] :distance-matrix
  (def n (distance-matrix-size dm))
  (make-distance-matrix
    (array/slice (dm :names))
    (seq [i :range [0 n]]
      (seq [j :range [0 n]]
        (f (distance-matrix-get dm i j))))))

(define distance-matrix-to-table
  ```Map {name_i<->name_j: distance} for i < j.```
  [dm :distance-matrix] :table
  (def n (distance-matrix-size dm))
  (def names (dm :names))
  (merge @{} ;(seq [i :range [0 n] j :range [0 n] :when (< i j)]
                {(string (names i) "<->" (names j))
                 (distance-matrix-get dm i j)})))
