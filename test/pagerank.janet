# -*- mode: janet; -*-
# graphs/test/pagerank: PageRank via power iteration.

(import ./helper :prefix "")
(import ../graphs/pagerank :prefix "pg-")
(import ../graphs :prefix "")

(defn ≈ [a b] (< (math/abs (- a b)) 1e-9))

(defn reference-pagerank
 " Simple PageRank: a fresh table per iteration, out-degrees read from the
adjacency. Independent of the module's buffers and cached out-degree table."
  [g & kws]
  (def opts (table ;kws))
  (def d (get opts :damping 0.85))
  (def max-iter (get opts :max-iter 100))
  (def tol (get opts :tol 1e-6))
  (def ns (nodes g))
  (def n (length ns))
  (if (= n 0) @{}
    (do
      (var rank @{})
      (each u ns (put rank u (/ 1.0 n)))
      (def pers @{})
      (def custom (get opts :personalization))
      (if custom
        (do
          (def tot (reduce + 0.0 (seq [u :in ns] (get custom u 0.0))))
          (if (> tot 0)
            (each u ns (put pers u (/ (get custom u 0.0) tot)))
            (each u ns (put pers u (/ 1.0 n)))))
        (each u ns (put pers u (/ 1.0 n))))
      (def scale (- 1.0 d))
      (for _ 0 max-iter
        (var dangling-sum 0.0)
        (each u ns
          (when (zero? (length (get (g :adj) u)))
            (+= dangling-sum (get rank u))))
        # create a new table every round
        (def nxt (merge @{} ;(seq [u :in ns]
                                  {u (* scale (get pers u))})))
        (each v ns
          (def outs (get (g :adj) v))
          (def od (length outs))
          (when (> od 0)
            (def contrib (/ (* d (get rank v)) od))
            (loop [[w _] :pairs outs]
              (put nxt w (+ (get nxt w) contrib)))))
        (each u ns
          (put nxt u (+ (get nxt u) (* d dangling-sum (get pers u)))))
        (def err (sum (seq [u :in ns] (math/abs (- (get nxt u) (get rank u))))))
        (set rank nxt)
        (when (< err tol) (break)))
      rank)))

(defn ranks-match? [a b]
  (and (= (length (keys a)) (length (keys b)))
       (all (fn [u] (≈ (get a u 0.0) (get b u 0.0))) (keys a))))


#  pinned values on a small graph
# a -> b -> c -> a, plus d hanging off c
(def k3 (make-digraph))
(each n ["a" "b" "c" "d"] (digraph-add-node k3 n))
(digraph-add-edge k3 "a" "b" 1)
(digraph-add-edge k3 "b" "c" 1)
(digraph-add-edge k3 "c" "a" 1)
(digraph-add-edge k3 "c" "d" 1)

(def pr3 (pagerank k3))
(def ref3 (reference-pagerank k3))

(cassert-eq "pagerank matches reference on a 4-node digraph"
            (ranks-match? pr3 ref3) true)

(cassert-eq "ranks sum to 1"
            (≈ (sum (values pr3)) 1.0) true)

(cassert-eq "ranks cover every node"
            (length (keys pr3)) 4)

(cassert-eq "the hub c ranks highest"
            (get pr3 "c") (reduce max 0.0 (values pr3)))

(cassert-eq "a sink does not accumulate: d ties a"
            (get pr3 "d") (get pr3 "a"))

(cassert-eq "b sits strictly between the tied pair and c"
            (and (> (get pr3 "b") (get pr3 "a"))
                 (< (get pr3 "b") (get pr3 "c"))) true)


#  randomised differential over shapes, densities and damping
(math/seedrandom 4242)
(defn random-dg [n p]
  (def g (make-digraph))
  (each i (range n)
    (digraph-add-node g (string i)))
  (for i 0 n
    (for j 0 n
      (when (and (not= i j) (< (math/random) p))
        (digraph-add-edge g (string i) (string j) 1.0))))
  g)

(var cases 0)
(var bad 0)
(each n [1 2 3 5 9 17 40 120]
  (each dens [0.0 0.02 0.08 0.25 0.6]
    # damping sweep, full convergence
    (each d [0.85 0.5 0.95]
      (def g (random-dg n dens))
      (++ cases)
      (unless (ranks-match? (pagerank g :damping d)
                            (reference-pagerank g :damping d))
        (++ bad)
        (when (< bad 4)
          (print "  DIFF n=" n " dens=" dens " damping=" d))))
    # iteration-cap sweep: :max-iter must truncate identically
    (each iters [1 3 50]
      (def g2 (random-dg n dens))
      (++ cases)
      (unless (ranks-match? (pagerank g2 :max-iter iters)
                            (reference-pagerank g2 :max-iter iters))
        (++ bad)
        (when (< bad 4) (print "  DIFF max-iter=" iters " n=" n " dens=" dens))))))

(cassert-eq (string "pagerank agrees with the reference on "
                    cases  " random digraph configurations") bad 0)


#  undirected graphs
(math/seedrandom 99)
(var ubad 0)
(var ucases 0)
(each n [4 12 40]
  (each p [0.1 0.4]
    (def g (make-graph @{}))
    (each i (range n) (add-node g (string i)))
    (for i 0 n
      (for j (inc i) n
        (when (< (math/random) p)
          (add-edge g (string i) (string j)))))
    (++ ucases)
    (unless (ranks-match? (pagerank g)
                          (reference-pagerank g))
      (++ ubad))))

(cassert-eq (string "pagerank agrees on "
                    ucases " random undirected graphs") ubad 0)


#  personalised teleport
(def pers @{"a" 3.0 "b" 1.0})
(def pp (pagerank k3 :personalization pers))

(cassert-eq "personalisation still agrees with the reference"
            (ranks-match? pp (reference-pagerank k3 :personalization pers)) true)

(cassert-eq "personalised ranks sum to 1"
            (≈ (sum (values pp)) 1.0) true)

(cassert-eq "personalisation actually changes the distribution"
            (deep= pp pr3) false)

(cassert-eq "b outranks a despite a's larger personalisation"
            (> (get pp "b") (get pp "a")) true)

(cassert-eq "personalising a and b starves the sink d"
            (< (get pp "d") (get pr3 "d")) true)

(def alien @{"a" 1.0 "zzz" 99.0})
(def pa (pagerank k3 :personalization alien))

(cassert-eq "foreign personalisation keys are ignored"
            (≈ (sum (values pa)) 1.0) true)
(cassert-eq "and the result still matches the in-graph-only reference"
            (ranks-match?
             pa (reference-pagerank k3 :personalization @{"a" 1.0}))
            true)


#  degenerate inputs

(cassert-eq "empty graph yields an empty table"
            (deep= (pagerank (make-digraph)) @{}) true)

(cassert-eq "single node carries all the rank"
            (≈ (get (pagerank
                     (make-digraph @["only"] @{})) "only") 1.0) true)

(cassert-eq "no edges at all is still a valid distribution"
            (≈ (sum (values (pagerank
                             (make-digraph @["a" "b" "c"] @{})))) 1.0) true)


(print-results)
