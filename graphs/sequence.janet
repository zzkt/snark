# -*- mode: janet; -*-
# graphs/sequence: degree sequence realisability tests

#  Decides whether a degree sequence (or pair of sequences for digraphs)
#  can be realized as a simple graph, multigraph, pseudograph, or digraph.

(import deft :prefix "")

(defn- sum-seq [s]
  (reduce + 0 s))

(defn- sorted-desc [s]
  (sort (array/slice s) >))

(defn- nonneg? [s]
  (all (fn [d] (>= d 0)) s))


# ==============================================================================
#  Tests for a single undirected degree sequence
# ==============================================================================

(define is-valid-havel-hakimi?
   ```Return true if `sequence` can be realized as a simple undirected graph.
Uses the Havel-Hakimi algorithm.```
  [sequence] :boolean
  (def ds (sorted-desc sequence))
  (if (empty? ds)
    true
    (if (not (nonneg? ds))
      false
      (do
      (defn step [deg]
        (if (empty? deg)
          true
          (do
            (def d (deg 0))
            (if (> d (length (array/slice deg 1)))
              false
              (do
                (def rest-arr (array/slice deg 1))
                (for i 0 d
                  (def idx (min i (- (length rest-arr) 1)))
                  (if (<= (rest-arr idx) 0)
                    (set (rest-arr idx) -1)
                    (-- (rest-arr idx))))
                (if (all (fn [x] (>= x 0)) rest-arr)
                  (step (sorted-desc rest-arr))
                  false))))))
      (step ds)))))


(define is-valid-erdos-gallai?
  ```Return true if `sequence` can be realized as a simple undirected graph.

Uses the Erdős-Gallai theorem; for every k, the sum of the k largest entries
must not exceed k(k-1) + sum_{i>k} min(d_i, k), and the total must be even.
```
  [sequence] :boolean
  (let [d (sorted-desc sequence)
        n (length d)
        total (sum-seq d)]
    (cond
      (= n 0) true
      (not (nonneg? d)) false
      (odd? total) false
      (do
        (def prefix (array/new (+ n 1)))
        (put prefix 0 0)
        (for i 0 n
          (put prefix (+ i 1) (+ (prefix i) (d i))))
        (var p n)
        (var ok true)
        (for k 1 (+ n 1)
          (while (and (> p 0) (< (d (- p 1)) k))
            (-- p))
          (def m (if (> p k) p k))
          (def lhs (prefix k))
          (def rhs (+ (* k (- k 1))
                      (- total (prefix m))
                      (* k (if (> p k) (- p k) 0))))
          (when (> lhs rhs)
            (set ok false)))
        ok))))


(define is-graphical?
  ```Return true if `sequence` is the degree sequence of some simple graph.```
  [sequence] :boolean
  (is-valid-erdos-gallai? sequence))

(define is-pseudographical?
  ```Return true if some pseudograph realizes the `sequence`.
Loops and multi-edges allowed. Requires non-negative terms and even total degree.```
  [sequence] :boolean
  (and (nonneg? sequence) (even? (sum-seq sequence))))

(define is-multigraphical?
  ```Return true if some multigraph realizes the `sequence`.
No loops, parallel edges allowed. Requires non-negativity, even sum, and that no
degree exceeds the sum of the others.```
  [sequence] :boolean
  (if (empty? sequence)
    true
    (if (not (nonneg? sequence))
      false
      (let [s (sum-seq sequence)]
        (and (even? s)
             (all (fn [d] (<= d (- s d))) sequence))))))


# ==============================================================================
#  Directed degree sequence (Fulkerson-Chen-Anstee)
# ==============================================================================

(define is-digraphical?
  ```True when the pair of sequences can be realized as a simple directed graph.
Uses the Fulkerson-Chen-Anstee criterion.```
  [out-seq in-seq] :boolean
  (def n (length out-seq))
  (if (not= n (length in-seq))
    false
    (do
      (def p2 (seq [i :in (range n)] @[(out-seq i) (in-seq i)]))
      (sort p2 (fn [p q] (> (in p 0) (in q 0))))
      (def a (seq [p :in p2] (in p 0)))
      (def b (seq [p :in p2] (in p 1)))
      # both must be non-negative and total out-degree must equal total in-degree
      (if (or (not (nonneg? a)) (not (nonneg? b)) (not= (sum-seq a) (sum-seq b)))
        false
        (do
          (var k 1)
          (var ok true)
          (while (and ok (< k (+ n 1)))
            (def lhs (sum-seq (array/slice a 0 k)))
            (def rhs (+ (sum-seq (seq [i :in (range 0 k)] (min (b i) (- k 1))))
                        (sum-seq (seq [i :in (range k n)] (min (b i) k)))))
            (when (> lhs rhs) (set ok false))
            (++ k))
          ok)))))
