# Example


(import graphs/petri-net :prefix "")
(import graphs/export/petri-net :prefix "ex/")



# A net can be constructed using =make-petri-net=, which provides a range of options, or using =petri-net-from-arcs= which is simpler in most cases.


(def p0 (petri-net-from-arcs
         @["p1" "p2"]             # places
         @["t1" "t2"]             # transitions
         @[["p1" "t1" 1]          # arcs [source target weight]
           ["t1" "p2" 1]
           ["p2" "t2" 1]
           ["t2" "p1" 1]]))

(printf "A Petri-net with %p places and %p transitions."
        (petri-net-place-count p0) (petri-net-transition-count p0))



# #+RESULTS:
# : A Petri-net with 2 places and 2 transitions.

# A producer net. =t1= consumes a token from =p1= and produces two into =p2=


(def pn (petri-net-from-arcs
          @["p1" "p2"] @["t1"]
          @[["p1" "t1"] ["t1" "p2" 2]]
          @{"p1" 1}))

(printf "enabled: %p" (petri-net-enabled-transitions pn))

(petri-net-fire pn "t1")

(printf "marking after firing: %p" (petri-net-marking pn))

(printf "bounded? %p\nsafe? %p" (petri-net-bounded? pn) (petri-net-safe? pn))



# #+RESULTS:
# : enabled: @["t1"]
# : marking after firing: @{"p1" 0 "p2" 2}
# : bounded? true
# : safe? false

# Firing preserves the S-invariant =2·p1 + p2=


(def pn2 (petri-net-from-arcs
           @["p1" "p2"] @["t1"]
           @[["p1" "t1"] ["t1" "p2" 2]]))

(def inv (petri-net-invariant-space pn2))

(printf "S-invariant basis: %p" (get inv :places))
(printf "consistent with {p2 2}? %p" (petri-net-consistent? pn2 @{"p2" 2}))



# #+RESULTS:
# : S-invariant basis: @[@[2 1]]
# : consistent with {p2 2}? false

# Reachability. A token shuttles =x → y → x= around a cycle.


(def cy (petri-net-from-arcs
          @["x" "y"] @["s" "f"]
          @[["x" "s"] ["s" "y"] ["y" "f"] ["f" "x"]]
          @{"x" 1}))

(printf "firing sequence to {y 1}: %p" (petri-net-firing-sequence cy @{"y" 1}))

(def marking->str (fn [m] (string/join (map (fn [kv] (string (kv 0) "=" (kv 1)))
                                             (pairs m)) "|")))
(printf "reachable markings: %s"
        (string/join (map marking->str (petri-net-reachable-markings cy)) " "))

(printf "live? %p\ndeadlock? %p" (petri-net-live? cy) (petri-net-deadlock? cy))



# #+RESULTS:
# : firing sequence to {y 1}: @["s"]
# : reachable markings: x=1|y=0 x=0|y=1
# : live? true
# : deadlock? false

# Unbounded nets have infinite entries in the coverability tree


(def unb (petri-net-from-arcs
           @["p"] @["t"]
           @[["p" "t"] ["t" "p" 2]]
           @{"p" 1}))

(printf "bounded? %p" (petri-net-bounded? unb))

(def ct (petri-net-coverability-tree unb))

(printf "coverability tree contains omega? %p"
        (some (fn [n] (some (fn [c] (= c math/inf)) (values (get n :marking))))
              (get ct :nodes)))



# #+RESULTS:
# : bounded? false
# : coverability tree contains omega? true

# Reproducible simulation using a =:seed=


(def sim (petri-net-from-arcs
           @["a" "b"] @["x"]
           @[["a" "x"] ["x" "b"] ["b" "x"]]
           @{"a" 1}))

(petri-net-simulate sim :steps 5 :seed 7)
(printf "tokens conserved: %p" (petri-net-token-count sim))



# #+RESULTS:
# : tokens conserved: 1

# Render with Graphviz


(def pn3 (petri-net-from-arcs
           @["p1" "p2"] @["t1"]
           @[["p1" "t1"] ["t1" "p2" 2]]
           @{"p1" 1}))

(print (ex/petri-net-to-dot pn3))
