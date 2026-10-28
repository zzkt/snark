# -*- mode: janet; -*-
# graphs/test/flow: Edmonds-Karp, Dinic, min-cut.

(import ./helper :prefix "")
(import ../graphs :prefix "")

(print "\n* single antiparallel pair")

(def ab (make-digraph))
(add-node ab "a")
(add-node ab "b")
(add-edge ab "a" "b" 1)
(add-edge ab "b" "a" 1)
(cassert-eq "antiparallel a↔b max flow 1"
            (get (edmonds-karp ab "a" "b") :flow) 1.0)

(print "\n* path graph")

(def p (make-digraph))
(add-nodes p "0" "1" "2")
(add-edges p ["0" "1" 3] ["1" "2" 4])
(cassert-eq "path 0→2 max flow 3"
            (get (dinic p "0" "2") :flow) 3.0)
(cassert-eq "reverse direction has no flow"
            (get (dinic p "2" "0") :flow) 0.0)

(print "\n* K3 with unit antiparallel arcs")

(def k3 (make-digraph))
(add-nodes k3 "0" "1" "2")
(add-edges k3 [["0" "1" 1] ["1" "0" 1] ["0" "2" 1]
               ["2" "0" 1] ["1" "2" 1] ["2" "1" 1]])
(cassert-eq "K3 ek flow 2"
            (get (edmonds-karp k3 "0" "1") :flow) 2.0)
(cassert-eq "K3 dinic flow 2"
            (get (dinic k3 "0" "1") :flow) 2.0)
(cassert-eq "K3 min-cut capacity 2"
            (get (min-cut k3 "0" (dinic k3 "0" "1")) :capacity) 2.0)


# After one call the level graph must be exhausted
(defn level-respecting-path? [rg s t]
  (var found false)
  (def level @{})
  (def q @[s])
  (put level s 0)
  (var i 0)
  (while (and (< i (length q)) (not found))
    (def u (q i))
    (++ i)
    (loop [[v cap] :pairs (get (rg :adj) u)]
      (when (and (> cap 0.0) (nil? (get level v)))
        (put level v (+ (get level u) 1))
        (if (= v t)
          (set found true)
          (array/push q v)))))
  found)

# A layered graph with many independent s->t paths, all the same length
(defn layered [k width cap]
  (def g (make-digraph))
  (digraph-add-node g "s")
  (digraph-add-node g "t")
  (for d 0 k
    (def prev (if (= d 0)
                ["s"]
                (map (fn [w] (string "m" (dec d) "-" w)) (range width))))
    (each w (range width)
      (def node (string "m" d "-" w))
      (digraph-add-node g node)
      (each p prev (add-edge g p node cap))
      (when (= d (dec k)) (add-edge g node "t" cap))))
  g)

# 3 layers x 4 wide, capacity 5: 12 routes of 5 each -> max flow is 20.
(def lay (layered 3 4 5.0))
(cassert-eq "layered dinic max flow 20"
            (get (dinic lay "s" "t") :flow) 20.0)
(cassert-eq "layered min-cut agrees"
            (get (min-cut lay "s" (dinic lay "s" "t")) :capacity) 20.0)
(cassert-eq "no level-respecting s->t path survives the blocking flow"
            (level-respecting-path? (get (dinic lay "s" "t") :residual) "s" "t") false)

# A wide single-hop fan: one call should clear it.
(def fan (make-digraph))
(add-node fan "s")
(add-node fan "t")
(for i 0 8
  (def m (string "m" i))
  (add-node fan m)
  (add-edge fan "s" m 3.0)
  (add-edge fan m "t" 3.0))
(cassert-eq "fan dinic max flow 24" (get (dinic fan "s" "t") :flow) 24.0)
(cassert-eq "fan residual has no level-respecting path"
  (level-respecting-path? (get (dinic fan "s" "t") :residual) "s" "t") false)

# Bottleneck chains
(defn chain [len cap]
  (def g (make-digraph))
  (add-node g "s")
  (add-node g "t")
  (for i 0 len
    (def a (string "a" i))
    (def b (string "b" i))
    (add-node g a)
    (add-node g b)
    (add-edge g (if (= i 0) "s" (string "b" (dec i))) a cap)
    (add-edge g a b cap)
    (when (= i (dec len)) (add-edge g b "t" cap)))
  g)

(each len [1 2 4 7]
  (def c (chain len 2.0))
  (cassert-eq (string "chain len=" len " max flow 2")
              (get (dinic c "s" "t") :flow) 2.0)
  (cassert-eq (string "chain len=" len " min cut 2")
              (get (min-cut c "s" (dinic c "s" "t")) :capacity) 2.0))


# Randomised check of the min cut
(var seed 987654321)
(defn nxt []
  (set seed (mod (+ (* seed 75) 74) 65537))
  seed)
(var mismatches 0)
(for trial 0 120
  (def g (make-digraph))
  (def n (+ 4 (mod (nxt) 5)))
  (for i 0 n (add-node g (string i)))
  (for i 0 n
    (for j 0 n
      (when (and (not= i j) (< (mod (nxt) 4) 1))
        (add-edge g (string i) (string j) (+ 1 (mod (nxt) 9))))))
  # min-cut takes the result table, not the residual
  (def d (dinic g "0" (string (dec n))))
  (def mc (min-cut g "0" d))
  (when (not= (get d :flow) (get mc :capacity)) (++ mismatches)))
(cassert-eq "120 random digraphs: dinic flow equals min cut" mismatches 0)

(print-results)
