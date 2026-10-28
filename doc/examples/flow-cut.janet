# Examples


(import graphs :prefix "")



# A two-hop network, =0 → 1 → 2=, whose bottleneck is the capacity-3 arc


(def net (make-digraph))
(add-nodes net "0" "1" "2")
(add-edges net ["0" "1" 3] ["1" "2" 4])

(def r (dinic net "0" "2"))
(printf "max flow 0->2: %p" (get r :flow))

(def mc (min-cut net "0" r))
(printf "min cut capacity: %p  cut set: %p" (get mc :capacity) (get mc :cut-set))



# #+RESULTS:
# : max flow 0->2: 3
# : min cut capacity: 3  cut set: @["1" "2"]

# Cut statistics on a diamond


(def sq (make-graph))
(add-nodes sq "a" "b" "c" "d")
(add-edges sq ["a" "b"] ["a" "c"] ["b" "d"] ["c" "d"])

(printf "edge-expansion {a}: %p" (edge-expansion sq @["a"]))
(printf "cut-size {a}: %p" (cut-size sq @["a"]))
(printf "conductance {a}: %.3f" (conductance sq @["a"]))



# #+RESULTS:
# : edge-expansion {a}: 2
# : cut-size {a}: 2
# : conductance {a}: 1.000

# Dinic is faster on dense graphs but Edmonds-Karp is simpler


(def net2 (make-digraph))
(add-nodes net2 "0" "1" "2")
(add-edges net2 ["0" "1" 3] ["1" "2" 4])

(def r2 (edmonds-karp net2 "0" "2"))
(printf "edmonds-karp max flow 0->2 is %p" (get r2 :flow))
(printf "edmonds-karp min cut capacity is %p with cut set %p"
        (get (min-cut net2 "0" r2) :capacity)
        (get (min-cut net2 "0" r2) :cut-set))



# #+RESULTS:
# : edmonds-karp max flow 0->2 is 3
# : edmonds-karp min cut capacity is 3 with cut set @["1" "2"]

# Full cut statistics on a cycle C5 with node set {a, b}


(def c5 (make-graph))
(add-nodes c5 "a" "b" "c" "d" "e")
(add-edges c5 ["a" "b"] ["b" "c"] ["c" "d"] ["d" "e"] ["e" "a"])

(printf "volume {a b}: %p" (volume c5 @["a" "b"]))

(printf "cut-size {a b}: %p" (cut-size c5 @["a" "b"]))

(printf "edge-boundary {a b}: %s"
        (string/join (map (fn [e] (string (e 0) "-" (e 1)))
                          (edge-boundary c5 @["a" "b"])) " "))

(printf "node-boundary {a b}: %s"
        (string/join (node-boundary c5 @["a" "b"]) " "))

(printf "edge-expansion {a b}: %.3f" (edge-expansion c5 @["a" "b"]))

(printf "node-expansion {a b}: %.3f" (node-expansion c5 @["a" "b"]))

(printf "boundary-expansion {a b}: %.3f" (boundary-expansion c5 @["a" "b"]))

(printf "mixing-expansion {a b}: %.3f" (mixing-expansion c5 @["a" "b"]))

(printf "normalized-cut-size {a b}: %.3f" (normalized-cut-size c5 @["a" "b"]))

(printf "conductance {a b}: %.3f" (conductance c5 @["a" "b"]))
