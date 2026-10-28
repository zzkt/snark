# -*- mode: janet; -*-
# graphs/test/petri-net: construction, firing, reachability, invariants, coverability

(import ./helper :prefix "")
(import ../graphs :prefix "")
(import ../graphs/petri-net :prefix "")
(import ../graphs/export/petri-net :prefix "")

# Two-place producer net: t1 consumes 1 from p1, produces 2 into p2.
(def prod (petri-net-from-arcs
           @["p1" "p2"] @["t1"]
           @[["p1" "t1"] ["t1" "p2" 2]]
           @{"p1" 1}))

(cassert-arr-eq "places" (petri-net-places prod) @["p1" "p2"])
(cassert-arr-eq "transitions" (petri-net-transitions prod) @["t1"])
(cassert-eq "initial p1 marking" (petri-net-tokens prod "p1") 1)
(cassert-eq "initial p2 marking" (petri-net-tokens prod "p2") 0)
(cassert-eq "token count" (petri-net-token-count prod) 1)
(cassert-eq "arc-count" (petri-net-arc-count prod) 2)
(cassert-eq "arc weight p1->t1" (petri-net-arc-weight prod "p1" "t1") 1)
(cassert-eq "arc weight t1->p2" (petri-net-arc-weight prod "t1" "p2") 2)
(cassert-eq "input-arcs of t1" (= (length (petri-net-input-arcs prod "t1")) 1) true)
(cassert-eq "output-arcs of t1" (= (length (petri-net-output-arcs prod "t1")) 1) true)
(cassert-arr-eq "input-places of t1" (petri-net-input-places prod "t1") @["p1"])
(cassert-arr-eq "output-places of t1" (petri-net-output-places prod "t1") @["p2"])

# Structural
(cassert-eq "bipartite" (petri-net-bipartite? prod) true)
(cassert-eq "ordinary false (weight 2)" (petri-net-ordinary? prod) false)
(cassert-eq "pure" (petri-net-pure? prod) true)
(cassert-eq "source placeholder" (petri-net-source-place? prod "p1") true)
(cassert-eq "sink placeholder" (petri-net-sink-place? prod "p2") true)

# Enabledness and firing
(cassert-eq "t1 enabled" (petri-net-enabled? prod "t1") true)
(cassert-arr-eq "enabled transitions" (petri-net-enabled-transitions prod) @["t1"])
(cassert-eq "fire" (petri-net-fire prod "t1") prod)
(cassert-eq "marking after fire p1" (petri-net-tokens prod "p1") 0)
(cassert-eq "marking after fire p2" (petri-net-tokens prod "p2") 2)
(cassert-eq "t1 no longer enabled" (petri-net-enabled? prod "t1") false)
(cassert-arr-eq "no enabled transitions" (petri-net-enabled-transitions prod) @[])
(cassert-eq "dead" (petri-net-dead? prod) true)

# fire-effect on a fresh copy
(def eff (petri-net-fire-effect (petri-net-from-arcs
                                 @["p1" "p2"] @["t1"]
                                 @[["p1" "t1"] ["t1" "p2" 2]]
                                 @{"p1" 1}) "t1"))

(cassert-eq "fire-effect p1" (get eff "p1") -1)
(cassert-eq "fire-effect p2" (get eff "p2") 2)

# try-fire must not mutate on failure
(def pf (petri-net-from-arcs @["a"] @["t"] @[["a" "t"]] @{"a" 0}))

(cassert-eq "try-fire fails on empty place"
            (= (petri-net-try-fire pf "t") nil) true)
(cassert-eq "marking unchanged after failed fire"
            (petri-net-tokens pf "a") 0)

(petri-net-set-place-marking pf "a" 1)
(cassert-eq "try-fire succeeds"
            (= (petri-net-try-fire pf "t") "t") true)
(cassert-eq "marking after try-fire"
            (petri-net-tokens pf "a") 0)

# try-fire with a non-string (keyword) transition id must return nil, not error
(def pk (petri-net-from-arcs @["a"] @["t"] @[["a" "t"]] @{"a" 1}))

(cassert-eq "try-fire keyword id returns nil"
            (= (petri-net-try-fire pk :kw) nil) true)
(cassert-eq "try-fire keyword id leaves tokens"
            (petri-net-tokens pk "a") 1)
(cassert-eq "try-fire unknown string id returns nil"
            (= (petri-net-try-fire pk "nope") nil) true)

# remove-arc on a node that is absent (or already removed) is a no-op
(def pr (petri-net-from-arcs @["a"] @["t"] @[["a" "t"]] @{"a" 1}))
(petri-net-remove-place pr "a")

(cassert-eq "remove-arc after place removed"
            (petri-net-remove-arc pr "a" "t") pr)
(cassert-eq "remove-arc absent source"
            (petri-net-remove-arc pr "missing" "t") pr)
(cassert-eq "remove-arc absent target"
            (petri-net-remove-arc pr "t" "missing") pr)

# Fire sequences
(def seq1 (petri-net-from-arcs @["x" "y"] @["s" "f"]
                               @[["x" "s"] ["s" "y"] ["y" "f"] ["f" "x"]]
                               @{"x" 1}))

(cassert-eq "can-fire-sequence"
            (petri-net-can-fire-sequence? seq1 @["s" "f"]) true)

(def m0 (petri-net-marking seq1))
(cassert-eq "marking unchanged after can-fire-sequence"
            (deep= (petri-net-marking seq1) m0) true)

(def rm-str (sort (map (fn [m] (string (get m "x") "/" (get m "y")))
                       (petri-net-reachable-markings seq1))))

(cassert-eq "reachable-markings"
            (deep= rm-str @["0/1" "1/0"]) true)
(cassert-eq "y reachable"
            (petri-net-reachable? seq1 @{"y" 1}) true)
(cassert-eq "2-y unreachable"
            (petri-net-reachable? seq1 @{"y" 2}) false)
(cassert-arr-eq "firing sequence s->y"
                (petri-net-firing-sequence seq1 @{"y" 1}) @["s"])
(cassert-arr-eq "firing sequence round-trip"
                (petri-net-firing-sequence seq1 @{"x" 1}) @[])
(cassert-eq "unreachable target nil"
            (= (petri-net-firing-sequence seq1 @{"y" 2}) nil) true)
(cassert-eq "live"
            (petri-net-live? seq1) true)
(cassert-eq "transition s live"
            (petri-net-transition-live? seq1 "s") true)
(cassert-eq "bounded"
            (petri-net-bounded? seq1) true)
(cassert-eq "safe"
            (petri-net-safe? seq1) true)
(cassert-eq "not deadlock"
            (petri-net-deadlock? seq1) false)

# Producer reaches a deadlock after consuming the initial token
(cassert-eq "producer deadlock" (petri-net-deadlock? prod) true)
(cassert-eq "producer bounded" (petri-net-bounded? prod) true)
(cassert-eq "producer not safe" (petri-net-safe? prod) false)

# State graph
(def sg (petri-net-state-graph seq1))
(cassert-eq "state graph nodes" (length (get sg :nodes)) 2)
(cassert-eq "state graph edges" (length (get sg :edges)) 2)

# Coverability tree of an unbounded net: p1 -> t -> p1 (self loop, token doubling)
(def unb (petri-net-from-arcs @["p1"] @["t"] @[["p1" "t"] ["t" "p1" 2]] @{"p1" 1}))
(cassert-eq "unbounded" (petri-net-bounded? unb) false)

(def ct (petri-net-coverability-tree unb))
(def has-omega
  (some (fn [n] (some (fn [m] (= m math/inf)) (values (get n :marking))))
        (get ct :nodes)))

(cassert-eq "coverability has omega"
            has-omega true)

# Incidence matrix and invariants on the producer
(def inc (petri-net-incidence-matrix prod))
(cassert-eq "incidence p1/t1" (get (get inc "p1") "t1") -1)
(cassert-eq "incidence p2/t1" (get (get inc "p2") "t1") 2)

(def inv (petri-net-invariant-space prod))
(cassert-eq "invariant-space has places basis"
            (> (length (get inv :places)) 0) true)

# S-invariant y = (2,1): 2 * C(p1) + 1 * C(p2) = 2*(-1) + 1*(2) = 0
(def y (get (get inv :places) 0))
(cassert-eq "s-invariant kills incidence"
            (= (+ (* (y 0) -1) (* (y 1) 2)) 0) true)
(cassert-eq "t-invariants empty for producer"
            (= (length (get inv :transitions)) 0) true)
(cassert-eq "consistent p2=2"
            (petri-net-consistent? prod @{"p2" 2}) true)
(cassert-eq "inconsistent p2=3"
            (petri-net-consistent? prod @{"p2" 3}) false)

# Fire-vectors to a reachable marking of the cycle
(def fv (petri-net-fire-vectors seq1 @{"x" 1}))
(cassert-eq "fire-vectors non-empty"
            (>= (length fv) 1) true)
(cassert-eq "fire-vectors counts arise from valid firing parity"
            (some (fn [v] (and (= (get v "s") 1)
                               (= (get v "f") 1))) fv) true)

# Simulation (reproducible)
(def sim (petri-net-from-arcs @["a" "b"] @["x"]
                              @[["a" "x"] ["x" "b"] ["b" "x"]]
                              @{"a" 1}))

(petri-net-simulate sim :steps 5 :seed 7)
(cassert-eq "simulation conserved tokens"
            (= (petri-net-token-count sim) 1) true)

# Capacities: p1 -> t, capacity 3 stops over-critical markings
(def cap (petri-net-from-arcs @["p1"] @["t"]
                              @[["p1" "t"] ["t" "p1" 2]] @{"p1" 1}))

(petri-net-set-place-capacity cap "p1" 3)

(cassert-eq "capacity query"
            (petri-net-place-capacity cap "p1") 3)

(petri-net-fire cap "t")
(petri-net-fire cap "t")

(cassert-eq "capacity respected p1<=3"
            (<= (petri-net-tokens cap "p1") 3) true)

# DOT output
(def dot (petri-net-to-dot prod))
(cassert-eq "dot is string"
            (= (type dot) :string) true)
(cassert-eq "dot has vee arrowheads"
            (if (string/find "arrowhead=vee" dot) true false) true)
(cassert-eq "dot quotes graph name"
            (if (string/find "\"petri\" {" dot) true false) true)

# Reachability-graph DOT output
(def rg-dot (petri-net-state-space-to-dot prod :name "prod-rg" :limit 500))
(cassert-eq "rg dot is string"
            (= (type rg-dot) :string) true)
(cassert-eq "rg dot quotes graph name"
            (if (string/find "\"prod-rg\" {" rg-dot) true false) true)
(cassert-eq "rg dot edge labels"
            (if (string/find "[label=" rg-dot) true false) true)

# TikZ output
(def tikz (petri-net-to-tikz prod))
(cassert-eq "tikz is string"
            (= (type tikz) :string) true)

(cassert-eq "tikz has begin/end"
            (if (and (string/find "\\begin{tikzpicture}" tikz)
                     (string/find "\\end{tikzpicture}" tikz)) true false) true)

(cassert-eq "tikz has place style + name label"
            (if (and (string/find "place/.style" tikz)
                     (string/find "label=below:{p1}" tikz)) true false) true)

(cassert-eq "tikz has transition style + name label"
            (if (and (string/find "transition/.style" tikz)
                     (string/find "label=above:{t1}" tikz)) true false) true)

(cassert-eq "tikz has token dots"
            (if (string/find "tokens=2" tikz) true false) true)

(cassert-eq "tikz has weight label"
            (if (string/find "node[auto, font=\\scriptsize]" tikz) true false)
            true)

(def tikz-five (petri-net-to-tikz
                (petri-net-from-arcs @["p"] @["t"] @[["p" "t"]] @{"p" 5})))

(cassert-eq "tikz large marking shows count label"
            (if (string/find "label=45:{5}" tikz-five) true false) true)

(def tikz-plain (petri-net-to-tikz prod :show-marking false))
(cassert-eq "tikz show-marking false hides tokens"
            (if (string/find "tokens=" tikz-plain) true false) false)

(def tikz-mutex
  (petri-net-from-arcs
   @["idle1" "crit1" "idle2" "crit2" "free"]
   @["enter1" "exit1" "enter2" "exit2"]
   @[["idle1" "enter1"] ["free" "enter1"] ["enter1" "crit1"]
     ["crit1" "exit1"] ["exit1" "idle1"] ["exit1" "free"]
     ["idle2" "enter2"] ["free" "enter2"] ["enter2" "crit2"]
     ["crit2" "exit2"] ["exit2" "idle2"] ["exit2" "free"]]
   @{"idle1" 1 "idle2" 1 "free" 1}))

(cassert-eq "tikz routes shared/cycle arcs around nodes"
            (> (length (string/find-all
                        "controls"
                        (petri-net-to-tikz tikz-mutex))) 0) true)

# Construction forms
(def kw (petri-net-from-arcs @["q"] @["u"]
                             @[["q" "u"] ["u" "q"]]
                             @{"q" 1}))

(cassert-eq "keyword form marking"
            (petri-net-tokens kw "q") 1)
(cassert-eq "keyword form places unique"
            (= (length (petri-net-places kw)) 1) true)

(def pos (make-petri-net (make-digraph) @["q"] @["u"] @{"q" 1} @{}))
(cassert-eq "field-positional form marking"
            (petri-net-tokens pos "q") 1)

(print-results)
