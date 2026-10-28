# -*- mode: janet; -*-
# graphs/flow: Max flow and min cut (Edmonds-Karp, Dinic)

(import deft :prefix "")
(import ./digraph :prefix "")
(import ./util :prefix "")

(define residual-adjust
  ```Adjust residual capacity of arc `u` → `v` by delta in the residual digraph `rg`.```
  [rg :digraph u :node-label v :node-label delta :number]
  (def adj-u (get (rg :adj) u))
  (put adj-u v (+ (get adj-u v 0.0) delta)))

(define make-residual-graph
 ```Build a residual digraph with forward/backward edges from a capacity digraph `g`.```
  [g :digraph] :digraph
  (def rg (make-digraph @[] @{}))
  (each u (nodes g)
    (digraph-add-node rg u))
  (loop [[u adj] :pairs (g :adj)]
    (loop [[v cap] :pairs adj]
      (residual-adjust rg u v cap)))
  rg)


(define bfs-augmenting-path
  ```BFS to find an augmenting path from `s` to `t` in residual graph `rg`.

Returns [path flow] or nil if none exists.```
  [rg :digraph s :node-label t :node-label]
  (def parent @{})
  (def data @[s])
  (var head 0)
  (put parent s nil)
  (var found nil)
  (while (and (< head (length data)) (nil? found))
    (def u (data head))
    (++ head)
    (when (= u t) (set found u))
    (loop [[v cap] :pairs (get (rg :adj) u)]
      (when (and (> cap 0) (nil? (get parent v)))
        (put parent v u)
        (array/push data v))))
  (if (nil? found) nil
    (do
      (def path @[])
      (def bottlenecks @[])
      (var cur found)
      (while (not= cur s)
        (array/push path cur)
        (def p (get parent cur))
        (array/push bottlenecks (get-in (rg :adj) [p cur]))
        (set cur p))
      (array/push path s)
      (def flow (apply min bottlenecks))
      [(reverse path) flow])))

(define residual-push
  ```Push `flow` along `path` in residual graph `rg`.```
  [rg :digraph path :array flow :number] :nil
  (for i 0 (- (length path) 1)
    (residual-adjust rg (path i) (path (+ i 1)) (- flow))
    (residual-adjust rg (path (+ i 1)) (path i) flow)))

(define extract-flows
  ```Extract flow on original edges of `g` from its residual graph `rg`.```
  [g :digraph rg :digraph] :table
  (def flows @{})
  (loop [[u adj] :pairs (g :adj)]
    (put flows u @{})
    (loop [[v cap] :pairs adj]
      (def residual-cap (get-in (rg :adj) [u v] 0.0))
      (def pushed (- cap residual-cap))
      (when (> pushed 0)
        (put (get flows u) v pushed))))
  flows)


(define edmonds-karp
  ```Edmonds-Karp max flow algorithm (BFS-based augmenting paths).

 `g` is a directed capacity graph
 `s` is the source node name
 `t` is the sink node name

Returns {:flow value :flows {u: {v: flow_on_edge}}
         :residual residual-digraph-after-max-flow
         :phases number-of-level-graphs :augmentations blocking-flow-calls}
```
  [g :digraph s :node-label t :node-label] :table
  (def rg (make-residual-graph g))
  (var max-flow 0.0)
  (while true
    (def aug (bfs-augmenting-path rg s t))
    (if (nil? aug) (break)
      (do
        (def [path flow] aug)
        (+= max-flow flow)
        (residual-push rg path flow))))
  @{:flow max-flow :flows (extract-flows g rg) :residual rg})


(define dinic
  ```Dinic's max flow algorithm (level graph & blocking flow).

 `g` is a directed capacity graph
 `s` is the source node name
 `t` is the sink node name

Returns {:flow value :flows {u: {v: flow_on_edge}}
         :residual residual-digraph-after-max-flow
         :phases number-of-level-graphs :augmentations blocking-flow-calls}
```
  [g :digraph s :node-label t :node-label] :table
  (def rg (make-residual-graph g))
  (defn build-level-graph []
    (def level @{s 0})
    (def data @[s])
    (var head 0)
    (while (< head (length data))
      (def u (data head))
      (++ head)
      (loop [[v cap] :pairs (get (rg :adj) u)]
        (when (and (> cap 0) (nil? (get level v)))
          (put level v (+ (get level u) 1))
          (array/push data v))))
    level)
  (defn send-flow [u flow t level ptr adjv]
    (if (= u t) flow
      (do
        (var result 0.0)
        (var remaining flow)
        (def nbrs (get adjv u))
        (var i (get ptr u 0))
        (while (and (> remaining 0.0) (< i (length nbrs)))
          (def v (nbrs i))
          (def cap (get-in (rg :adj) [u v] 0.0))
          (def usable (and (> cap 0.0) (= (get level v) (+ (get level u) 1))))
          (var exhausted (not usable))
          (when usable
            (def pushed (send-flow v (min remaining cap) t level ptr adjv))
            (when (> pushed 0.0)
              (residual-adjust rg u v (- pushed))
              (residual-adjust rg v u pushed)
              (+= result pushed)
              (set remaining (- remaining pushed)))
            (when (or (<= pushed 0.0) (<= (get-in (rg :adj) [u v] 0.0) 0.0))
              (set exhausted true)))
          (when exhausted
            (set i (inc i)))
          (put ptr u i))
        result)))
  (var max-flow 0.0)
  (var phases 0)
  (var augmentations 0)
  (while true
    (def level (build-level-graph))
    (when (nil? (get level t)) (break))
    (++ phases)
    (def adjv @{})
    (each u (nodes g) (put adjv u (keys (get (rg :adj) u))))
    (def ptr (merge @{} ;(seq [u :in (nodes g)] {u 0})))
    (while true
      (def pushed (send-flow s math/inf t level ptr adjv))
      (when (<= pushed 0) (break))
      (++ augmentations)
      (+= max-flow pushed)))
  @{:flow max-flow :flows (extract-flows g rg) :residual rg
    :phases phases :augmentations augmentations})


(define residual-reachable
  ```Nodes reachable from `s` in residual graph `rg` through arcs with positive capacity.```
  [rg :digraph s :node-label] :table
  (def reachable @{})
  # FIFO by array index; see bfs-augmenting-path above.
  (def data @[s])
  (var head 0)
  (put reachable s true)
  (while (< head (length data))
    (def u (data head))
    (++ head)
    (loop [[v cap] :pairs (get (rg :adj) u)]
      (when (and (> cap 0) (nil? (get reachable v)))
        (put reachable v true)
        (array/push data v))))
  reachable)


(define min-cut
 ```Find minimum s-t cut using residual graph from max-flow.

Uses output from edmonds-karp or dinic
 `g` is a directed capacity graph (:digraph)
 `s` is the source node name

Returns {:cut-set [node ...] :capacity value}
```
  [g :digraph s :node-label result :table] :table
  (var rg (get result :residual))
  (unless rg
    (set rg (make-residual-graph g))
    (loop [[u adj] :pairs (g :adj)]
      (loop [[v cap] :pairs adj]
        (def pushed (get-in (result :flows) [u v] 0.0))
        (residual-adjust rg u v (- pushed))
        (residual-adjust rg v u pushed))))
  (def reachable (residual-reachable rg s))
  (def cut-set (seq [v :in (nodes g) :unless (get reachable v)] v))
  @{:cut-set cut-set :capacity (result :flow)})
