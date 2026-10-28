# -*- mode: janet; -*-
# graphs/petri-net: Petri net place/transition graphs

#  A :petri-net is constructed from a :digraph whose nodes are places and
#  transitions of the net.

#  Export and visualisation, see: graphs/export/petri-net.janet

(import deft :prefix "")
(import ./record :prefix "ar-")
(import ./digraph :prefix "")

(deftrecord :petri-net
  (field graph :digraph)
  (field places :array)
  (field transitions :array)
  (field marking :table)
  (field capacity :table)
  (optional firing-cache :table))

(deftfn- bool
  "Coerce a truthy value to strict boolean."
  [x :dynamic] :boolean
  (if x true false))

(deftfn- marking-copy
  "Deep copy of a marking table."
  [m :table] :table
  (merge @{} m))

(deftfn- marking-key
  "Canonical key for marking table in the net's fixed place order."
  [pn :dynamic m :table] :tuple
  (tuple ;(map (fn [p] (get m p)) (pn :places))))

(deftfn- normalize-marking
  "Marking table with every place present, missing places default to 0."
  [pn :dynamic m :table] :table
  (let [out @{}]
    (each p (pn :places) (put out p (get m p 0)))
    out))

(deftfn- invalidate-firing!
  "Drop the firing table of `pn`. The next firing-data call rebuilds it."
  [pn :petri-net] :petri-net
  (put pn :firing-cache nil)
  pn)


# ==============================================================================
#  Firing data. Static per-transition pre/post sets and net effect. A query will
#  build the table once (prepare-firing), and firing-data is memoized.
# ==============================================================================

(deftfn- input-arcs-of
  "Input arcs of transition t as [place weight] pairs. Listed in place order."
  [pn :dynamic t :dynamic] :array
  (seq [p :in (pn :places)
        :let [w (ar-edge-weight (pn :graph) p t)]
        :when w]
       @[p w]))

(deftfn- output-arcs-of
  "Output arcs of transition t as [place weight] pairs."
  [pn :dynamic t :dynamic] :array
  (seq [[p w] :pairs (or (get (get (pn :graph) :adj) t) @{})] @[p w]))

(deftfn- in-neighbours
  "Sources holding an arc into node u (transitions feeding a place)."
  [pn :dynamic u :dynamic] :array
  (let [adj (get (pn :graph) :adj)]
    (seq [[s tbl] :pairs adj :when (get tbl u)] s)))

(deftfn- firing-record
 ```Static firing data for transition t of pn.

- input arcs (:in, [place weight] pairs consumed)
- net effect (:delta, {place change} applied when t fires)
- reference to the live capacity table (:cap).

 Input arcs are read in place order.
```
  [pn :dynamic t :dynamic] :table
  (let [in-arcs (input-arcs-of pn t)
        delta @{}]
    (each [p w] in-arcs (put delta p (- 0 w)))
    (each [p w] (output-arcs-of pn t)
      (put delta p (+ (get delta p 0) w)))
    @{:in in-arcs :delta delta :cap (pn :capacity)}))

(deftfn- prepare-firing
  "Static firing data {transition firing-record} for every transition of pn."
  [pn :dynamic] :table
  (def result @{})
  (each t (pn :transitions)
    (put result t (firing-record pn t)))
  result)

(deftfn- firing-data
  "Memoized {transition firing-record} for pn.
Rebuilt when the net's structure has changed since the last access."
  [pn :petri-net] :table
  (or (get pn :firing-cache)
      (let [fd (prepare-firing pn)]
        (put pn :firing-cache fd)
        fd)))

(deftfn- enabled-at?
  "True when the firing data fr is enabled at marking m."
  [fr :table m :table] :boolean
  (var ok true)
  (loop [[p w] :in (fr :in) :while ok]
    (when (> w (get m p 0)) (set ok false)))
  (when ok
    (loop [[p d] :pairs (fr :delta) :while ok]
      (let [cap (get (fr :cap) p)]
        (when (and cap (> (+ (get m p 0) d) cap))
          (set ok false)))))
  ok)

(deftfn- apply-firing
  "New marking after applying the effect of firing data fr to marking m."
  [fr :table m :table] :table
  (let [out (marking-copy m)]
    (each [p d] (pairs (fr :delta))
      (put out p (+ (get out p 0) d)))
    out))

(deftfn- apply-firing!
  "Apply the effect of firing data fr to marking m in place."
  [fr :table m :table] :table
  (each [p d] (pairs (fr :delta))
    (put m p (+ (get m p 0) d)))
  m)

(deftfn- deadlock-at?
  "True when no transition is enabled at marking `m` using firing record `fr`."
  [pn :dynamic fr :table m :table] :boolean
  (not (some (fn [t] (enabled-at? (get fr t) m))
             (pn :transitions))))

# ==============================================================================
#  Construction
# ==============================================================================

(define petri-net->digraph
  ```Return the underlying :digraph of the net (read only view).

Nodes contain :place and :transition data. An arc (adjacency) value is the
multiplicity/weight of the arc.
```
  [pn :petri-net] :digraph
  (pn :graph))

(define petri-net-place-count
  "Number of places in net `pn`."
  [pn :petri-net] :integer
  (length (pn :places)))

(define petri-net-transition-count
  "Number of transitions in net `pn`."
  [pn :petri-net] :integer
  (length (pn :transitions)))


# ==============================================================================
#  Mutators
# ==============================================================================

(define petri-net-place?
  "True when `name` is a place of the net `pn`."
  [pn :petri-net name :dynamic] :boolean
  (bool (= (ar-node-data (pn :graph) name) :place)))

(define petri-net-transition?
  "True when `name` is a transition of the net `pn`."
  [pn :petri-net name :dynamic] :boolean
  (bool (= (ar-node-data (pn :graph) name) :transition)))

(define petri-net-add-place
  ```Add a place to the net `pn` with an initial token count of 0.

Adding an existing place is a no-op, adding a node that is an existing
transition raises an error.

 Returns the net.
```
  [pn :petri-net name :dynamic] :petri-net
  (let [kind (ar-node-data (pn :graph) name)]
    (when (= kind :transition)
      (error (string "petri-net: " name " is already a transition")))
    (unless kind
      (ar-add-node (pn :graph) name)
      (array/push (pn :places) name)
      (put (get (pn :graph) :node-data) name :place)
      (put (pn :marking) name 0)
      (invalidate-firing! pn))
    pn))

(define petri-net-add-transition
    ```Add a transition to the net `pn`.

Adding an existing transition is a no-op, adding a node that is an existing
place raises an error.

 Returns the net.
```
  [pn :petri-net name :dynamic] :petri-net
  (let [kind (ar-node-data (pn :graph) name)]
    (when (= kind :place)
      (error (string "petri-net: " name " is already a place")))
    (unless kind
      (ar-add-node (pn :graph) name)
      (array/push (pn :transitions) name)
      (put (get (pn :graph) :node-data) name :transition)
      (invalidate-firing! pn))
    pn))

(define petri-net-remove-place
 ``` Remove a place and all its incident arcs from the net `pn`.

Returns the net. Removing a non-existent place is a no-op.
```
  [pn :petri-net name :dynamic] :petri-net
  (when (petri-net-place? pn name)
    (ar-remove-node (pn :graph) name)
    (let [ps (pn :places)
          idx (find-index |(= $0 name) ps)]
      (when idx (array/remove ps idx))
      (put (pn :marking) name nil)
      (put (pn :capacity) name nil)
      (invalidate-firing! pn)))
  pn)

(define petri-net-remove-transition
 ``` Remove a transition and all its incident arcs from the net `pn`.

Returns the net. Removing a non-existent transition is a no-op.
```
  [pn :petri-net name :dynamic] :petri-net
  (when (petri-net-transition? pn name)
    (ar-remove-node (pn :graph) name)
    (let [ts (pn :transitions)
          idx (find-index |(= $0 name) ts)]
      (when idx (array/remove ts idx))
      (invalidate-firing! pn)))
  pn)

(define petri-net-add-arc
  ```Add an arc of weight `w` between a place and transition.

- `u` → `v` as place → transition is an input arc (firing consumes w tokens)
- `u` → `v` as transition → place is an output arc (firing produces w tokens)

Both endpoints must already exist and be of opposite kinds.

Returns the net.
```
  [pn :petri-net u :dynamic v :dynamic &opt w] :petri-net
   (default w 1)
  (unless (and (number? w) (pos? w) (= w (math/floor w)))
    (error (string "petri-net: arc weight must be a positive integer, got " w)))
  (let [ku (ar-node-data (pn :graph) u)
        kv (ar-node-data (pn :graph) v)]
    (when (or (nil? ku) (nil? kv))
      (error "petri-net: arc endpoints must be existing places and transitions"))
    (when (= ku kv)
      (error "petri-net: arcs must join a place to a transition"))
    (put (get (get (pn :graph) :adj) u) v w)
    (invalidate-firing! pn)
    pn))

(define petri-net-remove-arc
  "Remove the arc `u` → `v` if present. Returns the net."
  [pn :petri-net u :dynamic v :dynamic] :petri-net
  (let [adj-u (get (get (pn :graph) :adj) u)]
    (when adj-u (put adj-u v nil))
    (invalidate-firing! pn))
  pn)


# ==============================================================================
#  Inspectors
# ==============================================================================

(define petri-net-arcs
  "All arcs as [source target weight] arrays."
  [pn :petri-net] :array
  (let [out @[]]
    (loop [[u tbl] :pairs (get (pn :graph) :adj)]
      (loop [[v w] :pairs tbl]
        (array/push out @[u v w])))
    out))

(define petri-net-arc-count
  "Number of arcs in net `pn`."
  [pn :petri-net] :integer
  (ar-edge-count-sum (pn :graph)))

(define petri-net-arc-weight
  "Weight of the arc u → v, or nil if the arc does not exist."
  [pn :petri-net u :dynamic v :dynamic] [:or :number :nil]
  (ar-edge-weight (pn :graph) u v))

(define petri-net-input-arcs
  "Input arcs of transition `t` as [place weight] pairs. Listed in place order."
  [pn :petri-net t :dynamic] :array
  (input-arcs-of pn t))

(define petri-net-output-arcs
  "Output arcs of transition `t` as [place weight] pairs."
  [pn :petri-net t :dynamic] :array
  (output-arcs-of pn t))

(define petri-net-input-places
  "Places feeding transition `t` (pre-set of t), in place order."
  [pn :petri-net t :dynamic] :array
  (seq [[p _] :in (input-arcs-of pn t)] p))

(define petri-net-output-places
  "Places fed by transition `t` (post-set of t)."
  [pn :petri-net t :dynamic] :array
  (seq [[p _] :in (output-arcs-of pn t)] p))

(define petri-net-fire-effect
  ```Change in `pn` place token counts when transition `t` fires.

Returns {place (output-weight - input-weight)}.
Places outside both sets are ignored.
```
  [pn :petri-net t :dynamic] :table
  (marking-copy (get (get (firing-data pn) t) :delta)))


# ==============================================================================
#  Marking
# ==============================================================================

(define petri-net-marking
  "Copy of the current marking {place token-count}."
  [pn :petri-net] :table
  (marking-copy (pn :marking)))

(define petri-net-tokens
  "Token count of a single place (or 0 when absent from the marking)."
  [pn :petri-net p :dynamic] :number
  (get (pn :marking) p 0))

(define petri-net-token-count
  "Total number of tokens in the net `pn`."
  [pn :petri-net] :number
  (sum (values (pn :marking))))

(define petri-net-place-capacity
  "Capacity limit (max tokens) of a place, or nil if the place is unbounded."
  [pn :petri-net p :dynamic] [:or :number :nil]
  (get (pn :capacity) p))

(define petri-net-place-capacities
  "Copy of the capacity table {place cap} of net `pn`."
  [pn :petri-net] :table
  (merge @{} (pn :capacity)))

(define petri-net-set-marking
 ```Set marking for the entire net `pn`.

Uses a {place count} table `m`. Fills missing places with current count (or 0).

Returns the net.
```
  [pn :petri-net m :table] :petri-net
  (let [current (pn :marking)
        fresh (normalize-marking pn m)]
    (each p (pn :places)
      (when (nil? (get m p))
        (put fresh p (get current p 0))))
    (each [p n] (pairs fresh) (put (pn :marking) p n))
    pn))

(define petri-net-set-place-marking
  "Set the token count of the place `p` to `n`. Returns the net."
  [pn :petri-net p :dynamic n :number] :petri-net
  (when (petri-net-place? pn p) (put (pn :marking) p n))
  pn)

(define petri-net-set-place-capacity
  ```Set the capacity of a place `p` to `cap` (or nil to make it unbounded).

Returns the  net.
```
  [pn :petri-net p :dynamic cap :dynamic] :petri-net
  (when (petri-net-place? pn p)
    (if cap
      (unless (and (number? cap) (pos? cap) (= cap (math/floor cap)))
        (error (string "petri-net: capacity must be a positive integer, got " cap)))
      (put (pn :capacity) p nil))
    (when cap (put (pn :capacity) p cap)))
  pn)


(define petri-net-enabled?
  "True if transition `t` can fire at the current marking, or given `marking` table."
  [pn :petri-net t :dynamic &opt marking] :boolean
  (unless (petri-net-transition? pn t)
    (error (string "petri-net: " t " is not a transition")))
  (let [m (or marking (pn :marking))]
    (enabled-at? (get (firing-data pn) t) m)))

(define petri-net-enabled-transitions
  "Transitions enabled at the current marking (or at the given `marking` table)."
  [pn :petri-net &opt marking] :array
  (let [m (or marking (pn :marking))
        fr (firing-data pn)]
    (filter (fn [t] (enabled-at? (get fr t) m)) (pn :transitions))))

(define petri-net-fire
  "Fire an enabled transition `t`, updating the net's marking in place."
  [pn :petri-net t :dynamic] :petri-net
  (unless (petri-net-transition? pn t)
    (error (string "petri-net: " t " is not a transition")))
  (let [fr (get (firing-data pn) t)]
    (unless (enabled-at? fr (pn :marking))
      (error (string "petri-net: transition " t " is not enabled")))
    (apply-firing! fr (pn :marking)))
  pn)

(defn petri-net-try-fire
  ```Fire transition `t` when enabled.

Return t on success, or nil. Does not modify the net if `t` is disabled.
```
  [pn t]
  (if (petri-net-transition? pn t)
    (let [fr (get (firing-data pn) t)]
      (if (enabled-at? fr (pn :marking))
        (do (apply-firing! fr (pn :marking)) t)
        nil))
    nil))

(define petri-net-fire-sequence
  "Fire each transition in the sequence in order, or error if not enabled."
  [pn :petri-net aseq :array] :petri-net
  (each t aseq (petri-net-fire pn t))
  pn)

(define petri-net-can-fire-sequence?
  "True when the transition sequence is enabled at the current marking."
  [pn :petri-net aseq :array] :boolean
  (let [fr (firing-data pn)
        m (marking-copy (pn :marking))]
    (bool
      (all (fn [t]
             (if (enabled-at? (get fr t) m)
               (do (apply-firing! (get fr t) m) true)
               false))
           aseq))))

(define petri-net-simulate
 ```Fire transitions from the current marking.

Chooses uniformly at random  among the enabled transitions at each step,
until a number of `:steps` have been fired or no transition is enabled.
Accepts an optional `:seed` for reproducibility.

Returns the net.
```
  [pn :petri-net & kws] :petri-net
  (let [opts (table ;kws)
        steps (get opts :steps 10)
        rng (math/rng (get opts :seed 12345))
        fr (firing-data pn)]
    (repeat steps
      (let [enabled (seq [t :in (pn :transitions)
                          :when (enabled-at? (get fr t) (pn :marking))] t)]
        (when (empty? enabled) (break))
        (let [t (get enabled (math/floor (* (math/rng-uniform rng) (length enabled))))]
          (apply-firing! (get fr t) (pn :marking)))))
    pn))


# ==============================================================================
#  Structural properties
# ==============================================================================

(deftfn- every-arc?
  "True when `pred` holds for every arc of the net's underlying graph."
  [g :dynamic pred :function] :boolean
  (var ok true)
  (loop [[u tbl] :pairs (get g :adj) :while ok]
    (loop [[v w] :pairs tbl :while ok]
      (set ok (pred u v w))))
  ok)

(define petri-net-bipartite?
  "True when every arc in `pn` joins a place to a transition (structural check)."
  [pn :petri-net] :boolean
  (bool (every-arc? (pn :graph)
    (fn [u v _] (not= (ar-node-data (pn :graph) u)
                      (ar-node-data (pn :graph) v))))))

(define petri-net-ordinary?
  "True when every arc of `pn` has weight 1."
  [pn :petri-net] :boolean
  (bool (every-arc? (pn :graph) (fn [_ _ w] (= w 1)))))

(define petri-net-pure?
  "True when no place is both an input and an output of the same transition."
  [pn :petri-net] :boolean
  (bool
    (all (fn [t]
           (let [outs (petri-net-output-places pn t)]
             (all (fn [p] (nil? (index-of p outs)))
                  (petri-net-input-places pn t))))
         (pn :transitions))))

(define petri-net-source-place?
  "True when the place has no incoming arcs."
  [pn :petri-net p :dynamic] :boolean
  (bool (empty? (in-neighbours pn p))))

(define petri-net-sink-place?
  "True when the place has no outgoing arcs."
  [pn :petri-net p :dynamic] :boolean
  (bool (empty? (ar-neighbours (pn :graph) p))))


# ==============================================================================
#  Reachability & state space
# ==============================================================================

(define reachable-states
  ```Breadth-first enumeration of the markings reachable from m0.

Returns {:states [{:marking table :parent i :via t}]
         :edges [[i j t]] :truncated bool}.
```
  [pn :dynamic m0 :table & kws] :table
  (let [opts (table ;kws)
        limit (get opts :limit 10000)
        target (get opts :target nil)
        target-key (when target
                    (marking-key pn (normalize-marking pn target)))
        fr (firing-data pn)
        out @[{:marking (marking-copy m0) :parent nil :via nil}]
        idx-of @{(marking-key pn m0) 0}
        edges @[]]
    (var head 0)
    (var truncated false)
    (var stop false)
    (while (and (< head (length out)) (not truncated) (not stop))
      (let [cur (get out head)
            i head]
        (++ head)
        (when (and target-key
                   (= target-key (marking-key pn (cur :marking))))
          (set stop true)
          (break))
        (each t (pn :transitions)
          (let [fd (get fr t)]
            (when (enabled-at? fd (cur :marking))
              (let [m2 (apply-firing fd (cur :marking))
                    k (marking-key pn m2)
                    j (get idx-of k)]
                # edge [i j t] -> transition t between state indices i and j
                (if j
                  (array/push edges @[i j t])
                  (if (>= (length out) limit)
                    (set truncated true)
                    (do
                      (put idx-of k (length out))
                      (array/push out {:marking m2 :parent i :via t})
                      (array/push edges @[i (- (length out) 1) t]))))))))))
    @{:states out :edges edges :truncated truncated}))

(deftfn- rebuild-path
  "Transition sequence from the root to state j."
  [states :array j :number] :array
  (let [steps @[]]
    (var cur j)
    (while cur
      (def s (get states cur))
      (when (s :via) (array/push steps (s :via)))
      (set cur (s :parent)))
    (reverse steps)))


(define petri-net-reachable-markings
 ```All markings reachable from the current marking.

Each marking is a {place count} table where the first entry is current marking.
Uses breadth-first search with a :limit (default 10000) on states explored.
```
  [pn :petri-net & kws] :array
  (let [opts (table ;kws)]
    (seq [s :in (get (reachable-states pn (pn :marking)
                                       :limit (get opts :limit 10000)) :states)]
      (s :marking))))

(define petri-net-reachable?
  "True when target {place count} is reachable from the current marking.
Optional :limit bounds the number of explored states (default 10000)."
  [pn :petri-net target :table & kws] :boolean
  (let [opts (table ;kws)
        res (reachable-states pn (pn :marking)
                              :limit (get opts :limit 10000)
                              :target target)]
    (not (nil? (some (fn [s] (= (marking-key pn (normalize-marking pn target))
                               (marking-key pn (s :marking))))
                       (res :states))))))

(define petri-net-firing-sequence
  ```Shortest firing sequence from current marking reaching `target`.
Optional `:limit` bounds the number of explored states (default 10000).

Returns array of {place count}, or nil if unreachable.
```
  [pn :petri-net target :table & kws] [:or :array :nil]
  (let [opts (table ;kws)
        res (reachable-states pn (pn :marking)
                              :limit (get opts :limit 10000)
                              :target target)
        tk (marking-key pn (normalize-marking pn target))]
    (var result nil)
    (each [i s] (pairs (res :states))
      (when (and (nil? result) (= tk (marking-key pn (s :marking))))
        (set result (rebuild-path (res :states) i))))
    result))

(define petri-net-state-space
  "Marking graph as a table, all reachable marking states and keyed edges."
  [pn :petri-net & kws] :table
  (let [opts (table ;kws)
        res (reachable-states pn (pn :marking)
                              :limit (get opts :limit 10000))
        ks (seq [s :in (res :states)] (marking-key pn (s :marking)))
        st @[]
        es @[]]
    (each [i s] (pairs (res :states))
      (array/push st {:marking (s :marking)
                      :path (rebuild-path (res :states) i)}))
    (each [i j t] (res :edges)
      (array/push es @[(ks i) (ks j) t]))
    @{:states st :edges es :truncated (res :truncated)}))

(define petri-net-state-graph
  ```Petri-net state space as a :digraph over markings.

Node data is the marking table, edges are labelled with the fired transition via
node data {:marking table :transitions [names]}.

Returns {:graph dg :markings {key marking} :nodes [tables] :edges [[i j t]]}
```
  [pn :petri-net & kws] :table
  (let [opts (table ;kws)
        res (reachable-states pn (pn :marking) :limit (get opts :limit 10000))
        ks (seq [s :in (res :states)] (marking-key pn (s :marking)))
        dg (make-digraph @[] @{})
        markings @{}
        nodes @[]]
    (each [i s] (pairs (res :states))
      (let [k (ks i)]
        (ar-add-node dg k)
        (put (get dg :node-data) k (s :marking))
        (put markings k (s :marking))
        (array/push nodes (s :marking))))
    (each [i j t] (res :edges)
      (put (get (get dg :adj) (ks i)) (ks j) t))
    @{:graph dg
      :markings markings
      :nodes nodes
      :edges (res :edges)
      :truncated (res :truncated)}))


# ==============================================================================
#  Coverability (Karp-Miller)
# ==============================================================================

(deftfn- marking-<=?
  "True when m1 <= m2 component-wise (with math/inf as maximal token count)."
  [m1 :table m2 :table] :boolean
  (all (fn [p]
         (let [a (get m1 p 0)
               b (get m2 p 0)]
           (or (= b math/inf) (<= a b))))
       (keys m1)))

(define petri-net-coverability-tree
  ```Karp-Miller coverability tree of the net from its current marking.

The coverability tree approximates the reachability set by promoting to omega
when a descendant marking dominates an ancestor on at least one place.

   Returns a table;
     :nodes      array of {:id i :marking {place count}}
                 unbounded places use omega (math/inf)
     :edges      array of [parent-id node-id transition] triples
     :truncated  whether the :limit on explored nodes was reached.
```
  [pn :petri-net & kws] :table
  (let [opts (table ;kws)
        limit (get opts :limit 1000)
        m0 (normalize-marking pn (pn :marking))
        fr (firing-data pn)
        nodes @[{:id 0 :marking m0}]
        parent-of @[nil]
        edges @[]
        seen @{(marking-key pn m0) 0}]
    (var qi 0)
    (var truncated false)
    (defn ancestors
      [idx]
      (let [out @[]]
        (var cur idx)
        (while cur
          (array/push out (get (get nodes cur) :marking))
          (set cur (get parent-of cur)))
        out))
    (while (and (not truncated) (< qi (length nodes)))
      (let [m (get (get nodes qi) :marking)]
        (each t (pn :transitions)
          (let [fd (get fr t)]
            (when (enabled-at? fd m)
              (let [m2 (apply-firing fd m)
                    ps (keys m2)]
                (each anc (ancestors qi)
                  (when (and (marking-<=? anc m2)
                             (some (fn [p] (< (get anc p 0) (get m2 p))) ps))
                    (each p ps
                      (when (< (get anc p 0) (get m2 p))
                        (put m2 p math/inf)))))
                (let [k (marking-key pn m2)
                      existing (get seen k)]
                  (if existing
                    (array/push edges @[qi existing t])
                    (do
                      (let [node-id (length nodes)]
                        (array/push nodes {:id node-id :marking m2}))
                      (array/push parent-of qi)
                      (put seen k (- (length nodes) 1))
                      (array/push edges @[qi (- (length nodes) 1) t])
                      (when (>= (length nodes) limit)
                        (set truncated true))))))))))
      (++ qi))
    @{:nodes nodes :edges edges :truncated truncated}))


(define petri-net-bounded?
  "True when the net `pn` is bounded at its current marking.

When the Karp-Miller coverability tree contains no omega place, every reachable
marking has finite token counts for every place."
  [pn :petri-net & kws] :boolean
  (let [tree (petri-net-coverability-tree pn ;kws)]
    (bool
      (when (not (tree :truncated))
        (all (fn [nd]
               (all (fn [v] (not= v math/inf)) (values (nd :marking))))
             (tree :nodes))))))

(define petri-net-safe?
  ```True when the net `pn` is safe.

A net is considered safe if every reachable marking holds at most one token
per place. A truncated coverability tree is treated as unsafe.
```
  [pn :petri-net & kws] :boolean
  (let [tree (petri-net-coverability-tree pn ;kws)]
    (bool
      (and (not (tree :truncated))
           (all (fn [nd]
                  (all (fn [v] (if (= v math/inf) false (<= v 1)))
                       (values (nd :marking))))
                (tree :nodes))))))

(deftfn- reverse-edges
  "Table {target-state-index [source-state-index ...]}  over the edges of an explored state graph."
  [edges :array] :table
  (let [rev @{}]
    (each e edges
      (let [v (e 1)]
        (unless (get rev v) (put rev v @[]))
        (array/push (get rev v) (e 0))))
    rev))

(deftfn- live-in?
  "True when transition t is live in an explored state graph"
  [fr :table rev :table states :array t :dynamic] :boolean
  (let [n (length states)
        enabling @{}]
    (each [i s] (pairs states)
      (when (enabled-at? (get fr t) (s :marking)) (put enabling i true)))
    (let [reached (merge @{} enabling)
          stack (keys enabling)]
      (var head 0)
      (while (< head (length stack))
        (let [u (get stack head)]
          (++ head)
          (each p (get rev u @[])
            (unless (get reached p)
              (put reached p true)
              (array/push stack p)))))
      (= n (length stack)))))

(deftfn- explore-liveness
  ```Reachable-state exploration shared by the liveness predicates.

Returns {:states :fr :rev}, the explored states, the net's firing data, and the
backwards edge table.```
  [pn :petri-net opts :table] :table
  (let [res (reachable-states pn (pn :marking) :limit (get opts :limit 10000))]
    @{:states (res :states)
      :fr (firing-data pn)
      :rev (reverse-edges (res :edges))}))

(define petri-net-transition-live?
  "True when transition t is live.
Optional  :limit bounds the explored states (default 10000)."
  [pn :petri-net t :dynamic & kws] :boolean
  (let [e (explore-liveness pn (table ;kws))]
    (bool (live-in? (e :fr) (e :rev) (e :states) t))))

(define petri-net-live?
  "True when every transition of the net is live.
Optional :limit bounds the explored states (default 10000)."
  [pn :petri-net & kws] :boolean
  (let [e (explore-liveness pn (table ;kws))]
    (bool (all (fn [t] (live-in? (e :fr) (e :rev) (e :states) t))
               (pn :transitions)))))

(define petri-net-dead?
  "True when no transition is enabled at the current marking."
  [pn :petri-net] :boolean
  (deadlock-at? pn (firing-data pn) (pn :marking)))

(define petri-net-deadlock?
  "True if the net can deadlock. i.e. can reach a marking at which no transition
is enabled from its current marking. Optional :limit bounds the explored states."
  [pn :petri-net & kws] :boolean
  (let [opts (table ;kws)
        res (reachable-states pn (pn :marking) :limit (get opts :limit 10000))
        fr (firing-data pn)]
    (bool (some (fn [s] (deadlock-at? pn fr (s :marking)))
                (res :states)))))

# ==============================================================================
#  Algebraic invariants
# ==============================================================================

(define petri-net-incidence-matrix
  "The matrix of changes in place token count when a transition fires once.

Incidence matrix C indexed {place {transition effect}} where
  C[p][t] = output-arc-weight(t,p) - input-arc-weight(p,t)"
  [pn :petri-net] :table
  (let [C @{}]
    (each p (pn :places) (put C p @{}))
    (each [t fd] (pairs (firing-data pn))
      (each [p d] (pairs (fd :delta))
        (put (get C p) t d)))
    C))

# Rational arithmetic. numbers represented as [num den]
(deftfn- rgcd
  [a :number b :number] :number
  (let [a (math/abs a)
        b (math/abs b)]
    (if (= b 0) a (rgcd b (% a b)))))

(deftfn- rat
  [n :number d :number] :array
  (let [g (rgcd n d)
        nn (/ n g)
        dd (/ d g)]
    (if (< dd 0) @[(- nn) (- dd)] @[nn dd])))

(deftfn- rzero
  [] :array
  @[0 1])

(deftfn- rplus
  [a :array b :array] :array
  (rat (+ (* (a 0) (b 1)) (* (b 0) (a 1))) (* (a 1) (b 1))))

(deftfn- rneg
  [a :array] :array
  @[(- (a 0)) (a 1)])

(deftfn- rminus
  [a :array b :array] :array
  (rplus a (rneg b)))

(deftfn- rdiv
  [a :array b :array] :array
  (rat (* (a 0) (b 1)) (* (a 1) (b 0))))

(deftfn- rz?
  [a :array] :boolean
  (= (a 0) 0))

(deftfn- counts->table
  "Names paired with counts as a {name count} table."
  [names :array counts :array] :table
  (let [out @{}]
    (each k (range (length names)) (put out (names k) (get counts k)))
    out))

(deftfn- rref
  "Reduced row echelon form of a matrix of rationals.
Returns [rref pivots] with pivots the pivot column indices."
  [mat :array] :array
  (let [rows (map (fn [r] (array/slice r)) mat)
        n (length rows)
        c (if (pos? n) (length (rows 0)) 0)
        pivots @[]]
    (var r 0)
    (each cc (range c)
      (var pivot -1)
      (for i r n
        (when (and (= pivot -1) (not (rz? (get (rows i) cc)))) (set pivot i)))
      (when (not= pivot -1)
        (let [tmp (get rows r)]
          (put rows r (get rows pivot))
          (put rows pivot tmp))
        (let [pr (get rows r)
              pv (get pr cc)]
          (each k (range c) (put pr k (rdiv (get pr k) pv)))
          (each i (range n)
            (when (not= i r)
              (let [ri (get rows i)
                    f (get ri cc)]
                (when (not (rz? f))
                  (each k (range c)
                    (put ri k (rminus (get ri k)
                                      (rat (* (f 0) (get (get pr k) 0))
                                           (* (f 1) (get (get pr k) 1)))))))))))
        (array/push pivots cc)
        (++ r)))
    @[(map (fn [r] (array/slice r)) rows) pivots]))

(deftfn- null-space-basis
  "Basis of the right null space of a matrix in RREF with its pivot columns."
  [mat :array pivots :array] :array
  (let [n (length mat)
        c (if (pos? n) (length (mat 0)) 0)
        pivot-set @{}
        result @[]]
    (each p pivots (put pivot-set p true))
    (each cc (range c)
      (when (nil? (get pivot-set cc))
        (let [vec (seq [i :range [0 c]] (rzero))]
          (put vec cc @[1 1])
          (each i (range (length pivots))
            (let [pc (get pivots i)]
              (put vec pc (rneg (get (get mat i) cc)))))
          (array/push result vec))))
    result))

(deftfn- to-integer-vector
  "Scale a rational vector to integers."
  [vec :array] :array
  (let [lcm (reduce (fn [a b]
                      (if (= b 0) a (/ (* a b) (rgcd a b))))
                    1
                    (map (fn [x] (x 1)) vec))
        nums (seq [x :in vec] (* (x 0) (/ lcm (x 1))))
        g0 (reduce (fn [a b] (rgcd a b)) 0 nums)
        g (if (= g0 0) 1 g0)]
    (map (fn [x] (/ x g)) nums)))

(deftfn- dot-counts
  "Dot product of a count vector with a row/column of values."
  [values :array counts :array] :number
  (sum (map * values counts)))

(deftfn- incidence-columns
  "Precomputed incidence-matrix columns for fast dot checks."
  [C :table places :array transitions :array for-places :boolean] :array
  (if for-places
    (seq [p :in places] (seq [t :in transitions] (get (get C p) t 0)))
    (seq [t :in transitions] (seq [p :in places] (get (get C p) t 0)))))

(deftfn- dot-zero-all?
  "True when every column vector dots to zero against counts."
  [cols :array counts :array] :boolean
  (var ok true)
  (loop [i :range [0 (length cols)] :while ok]
    (when (not (zero? (dot-counts (get cols i) counts)))
      (set ok false)))
  ok)

(define petri-net-invariant-space
  "Integer bases of the left and right null spaces of the incidence matrix."
  [pn :petri-net] :table
  (let [C (petri-net-incidence-matrix pn)
        ps (pn :places)
        ts (pn :transitions)
        Cm (seq [p :in ps]
             (seq [t :in ts] (rat (get (get C p) t 0) 1)))
        Ct (seq [t :in ts]
             (seq [p :in ps] (rat (get (get C p) t 0) 1)))
        [rct pivt] (rref Ct)
        [rcm pivm] (rref Cm)]
    @{:places (map to-integer-vector (null-space-basis rct pivt))
      :transitions (map to-integer-vector (null-space-basis rcm pivm))}))

(deftfn- bounded-solutions
  "Enumerate the integer vectors x in [0, radius]^k that satisfy pred, in
   lexicographic order, returning each as a {names[k] x[k]} table."
  [names :array radius :number pred :function] :array
  (let [result @[]
        k (length names)
        counts (array/new-filled k 0)]
    (defn rec
      [i]
      (if (= i k)
        (when (pred counts)
          (array/push result (counts->table names counts)))
        (for v 0 (+ radius 1)
          (put counts i v)
          (rec (+ i 1)))))
    (rec 0)
    result))

(deftfn- petri-net-invariants
  "Invariants of the net `pn`."
  [pn :petri-net for-places? :boolean opts :table] :array
  (let [radius (get opts :radius 4)
        C (petri-net-incidence-matrix pn)
        ts (pn :transitions)
        ps (pn :places)
        cols (incidence-columns C ps ts (not for-places?))]
    (bounded-solutions (if for-places? ps ts) radius
      (fn [counts]
        (and (some pos? counts)
             (dot-zero-all? cols counts))))))

(define petri-net-t-invariants
  "Transition invariants found by bounded enumeration.
The :radius option bounds each entry (default 4).

Returns an array of {transition count} tables."
  [pn :petri-net & kws] :array
  (petri-net-invariants pn false (table ;kws)))

(define petri-net-s-invariants
  "Place invariants found by bounded enumeration.
The :radius  option bounds each entry (default 4).

Returns an array of {place weight} tables."
  [pn :petri-net & kws] :array
  (petri-net-invariants pn true (table ;kws)))

(define petri-net-fire-vectors
  "Firing count vectors x for which the state equation M_final = M_0 + C . x holds with target marking {place count}. Entries are bounded by :radius (default 4)."
  [pn :petri-net target :table & kws] :array
  (let [opts (table ;kws)
        radius (get opts :radius 4)
        tn (normalize-marking pn target)
        m0 (get pn :marking)
        C (petri-net-incidence-matrix pn)
        ts (pn :transitions)
        ps (pn :places)
        cols (incidence-columns C ps ts true)]
    (bounded-solutions ts radius
      (fn [counts]
        (var ok true)
        # [0 n] rather than n: see dot-zero-all?.
        (loop [i :range [0 (length ps)] :while ok]
          (let [s (+ (get m0 (ps i) 0) (dot-counts (get cols i) counts))]
            (when (not= s (get tn (ps i) 0)) (set ok false))))
        ok))))


(define petri-net-consistent?
  "True when the marking equation M0 + C . x = target has a real solution.
This is a necessary condition for reachability, but not a sufficient one."
  [pn :petri-net target :table] :boolean
  (let [m0 (get pn :marking)
        tn (normalize-marking pn target)
        C (petri-net-incidence-matrix pn)
        ps (pn :places)
        ts (pn :transitions)
        aug (seq [p :in ps]
             (array/concat
               (seq [t :in ts] (rat (get (get C p) t 0) 1))
               @[(rat (- (get tn p 0) (get m0 p 0)) 1)]))
        [rrows _] (rref aug)]
    (bool
      (all (fn [row]
             (let [lhs (array/slice row 0 (- (length row) 1))]
               (or (some (fn [x] (not (rz? x))) lhs)
                   (rz? (last row)))))
           rrows))))

# ==============================================================================
#  Construction
# ==============================================================================

(define petri-net-from-arcs
  ```Construct a :petri-net from arrays of places, transitions and arcs.

Arcs are given by [source target weight] weight can be omitted, 1 by default.
A place-to-transition arc is an input and transition-to-place arc is an output.
```
  [places :array transitions :array arcs :array &opt marking] :petri-net
  (let [pn (make-petri-net (make-digraph @[] @{}) @[] @[] @{} @{})]
    (each p places (petri-net-add-place pn p))
    (each t transitions (petri-net-add-transition pn t))
    (each a arcs
      (petri-net-add-arc pn (a 0) (a 1) (if (> (length a) 2) (a 2) nil)))
    (when marking (petri-net-set-marking pn marking))
    pn))
