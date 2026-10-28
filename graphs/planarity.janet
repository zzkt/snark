# -*- mode: janet; -*-
# graphs/planarity: planarity testing (Demoucron-Malgrange-Pertuisot)

#  A graph is planar if it can be drawn in the plane with no two edges crossing.
#
#  Kuratowski's theorem says a graph is planar iff it contains no subdivision
#  of K5 or K3,3. The minor form is Wagner's theorem.
#
#  Demoucron, Malgrange & Pertuiset (1964) describe an exact, non-backtracking,
#  polynomial test for planarity;
#
#  - decompose the graph into vertex-biconnected blocks (Tarjan)
#  - for each block (2-connected, >= 3 edges)
#    - embed an initial cycle (two faces)
#    - repeatedly take any bridge where all vertices of attachment lie on a
#       single face and embed a path of the bridge inside that face.
#    - if any bridge has attachments on no common face, graph is non-planar
#    -> a graph is planar iff every block is planar.
#
#  References
#  - Diestel 2025, Graph Thoery, Ch.4 (planar graphs)
#  - Even 1979, Graph Algorithms, Ch.5 (the DMP face-splitting test).

(import deft :prefix "")
(import ./types :prefix "")
(import ./util :prefix "")


(defn- edge-key
  ```Canonical key for the undirected edge between a and b.

Each label is written with its length in front of it. A bare "/" separator
was ambiguous: ("a/b","c") and ("a","b/c") both produced "a/b/c", so
marking one edge embedded in `in-h` also marked a different one. Nothing
in this Janet can key a table by a pair -- tuples, arrays and structs all
compare by identity, so a fresh @[a b] would never match the one stored --
which leaves an unambiguous string as the only sound option.```
  [a b]
  (if (< a b)
    (string (length (string a)) ":" a (length (string b)) ":" b)
    (string (length (string b)) ":" b (length (string a)) ":" a)))


# ===========================================================================
#  Vertex-biconnected blocks (Tarjan)
# ===========================================================================

(defn- vertex-blocks
  "adj is an array of neighbour-index arrays. indices are 0..n-1.
Returns an array of blocks, each block is an array of [u v] index pairs."
  [n adj]
  (let [dfn (array/new n)
        low (array/new n)
        parent (array/new n)]
    (for i 0 n (put dfn i -1) (put low i -1) (put parent i -1))
    (var count 0)
    (let [estack @[]
          blocks @[]]
      (defn- dfs [u]
        (put dfn u count)
        (put low u count)
        (set count (+ count 1))
        (each v (adj u)
          (if (= (dfn v) -1)
            (do
              (put parent v u)
              (array/push estack [u v])
              (dfs v)
              (put low u (min (low u) (low v)))
              (when (>= (low v) (dfn u))
                (def blk @[])
                (var e (array/pop estack))
                (while (not= e [u v])
                  (array/push blk e)
                  (set e (array/pop estack)))
                (array/push blk e)
                (array/push blocks blk)))
            (do
              (when (and (not= v (parent u)) (< (dfn v) (dfn u)))
                (array/push estack [u v])
                (put low u (min (low u) (dfn v))))))))
      (for u 0 n (when (= (dfn u) -1) (dfs u)))
      blocks)))


# ===========================================================================
#  Planarity test with planar embedding (Demoucron, Malgrange & Pertuiset)
# ===========================================================================

(defn- find-cycle
  "adj is an array of neighbour-index arrays for a connected component.
Returns a simple cycle [v0 ... vk] (edges vi-vi+1 and vk-v0) of length  >= 3,
or nil if the component is acyclic."
  [n adj]
  (let [parent (array/new n)
        dfn (array/new n)]
    (for i 0 n (put parent i -1) (put dfn i -1))
    (var count 0)
    (var back nil)
    (defn- go [u p]
      (when (nil? back)
        (put parent u p)
        (put dfn u count)
        (set count (+ count 1))
        (each v (adj u)
          (when (nil? back)
            (if (= (dfn v) -1)
              (go v u)
              # undirected DFS: any non-tree edge to an ancestor is a back edge
              (when (and (not= v p) (< (dfn v) (dfn u)))
                (set back [u v])))))))
    (go 0 -1)
    (if (nil? back)
      nil
      (do
        (def cyc @[])
        (var x (back 0))
        (array/push cyc x)
        (while (not= x (back 1))
          (set x (parent x))
          (array/push cyc x))
        (reverse cyc)))))

#  A bridge of the embedded subgraph H is either
#   - a trivial bridge: an unembedded edge whose two endpoints are in H;
#   - a non-trivial bridge: a connected component of G - V(H) together with
#     attachment edges into H.
#  {:atts [attachment indices] :edges [[u v] ...] }

(defn- bridges-of
  [n adj h-verts in-h]
  (def result @[])
  # trivial bridges. single pass over embedded edge neighbourhood
  (each u (range n)
    (when (get h-verts u)
      (each v (adj u)
        (when (and (> v u) (get h-verts v)
                   (not (get in-h (edge-key u v))))
          (array/push result {:atts @[u v] :edges @[[u v]]})))))
  # non-trivial bridges. BFS components of G - V(H)
  (def seen (array/new n))
  (for z 0 n (put seen z false))
  (each s (range n)
    (unless (or (get h-verts s) (seen s))
      (def comp @[s])
      (put seen s true)
      (var head 0)
      (while (< head (length comp))
        (def u (comp head))
        (set head (+ head 1))
        (each v (adj u)
          (when (and (not (get h-verts v)) (not (seen v)))
            (put seen v true)
            (array/push comp v))))
      # collect attachments and every bridge edge
      (let [comp-set (table)
            atts @[]
            att-set (table)
            redges @[]]
        (each c comp (put comp-set c true))
        (each c comp
          (each w (adj c)
            (if (get h-verts w)
              (do
                (unless (get att-set w)
                  (put att-set w true)
                  (array/push atts w))
                (array/push redges [c w]))
              (when (and (> w c) (get comp-set w))
                (array/push redges [c w])))))
        (array/push result {:atts atts :edges redges}))))
  result)


(defn- face-index
  ```Returns [vertex -> incident face ids, face id -> face].

The second half is what lets dmp-step get hold of the chosen face without a
linear scan over the face list on every iteration.```
  [faces]
  (let [ix (table)
        by-id (table)]
    (each f faces
      (put by-id (f :id) f)
      (each v (f :vs)
        (unless (get ix v) (put ix v @[]))
        (array/push (get ix v) (f :id))))
    @[ix by-id]))

(defn- faces-common
  ```Every face id containing all of a bridge's attachment vertices.

Returns an array of face ids (empty when no face holds them all), not just one
of them: the DMP step has to embed a bridge of *minimum* admissible-face count,
so it needs the whole admissible set to rank the bridges, not an arbitrary
member. The intersection is still a table lookup rather than a linear scan.
```
  [ix atts]
  (var shared nil)
  (when (> (length atts) 0)
    (each a atts
      (def fs (or (get ix a) @[]))
      (if (nil? shared)
        (do (set shared @{}) (each f fs (put shared f true)))
        (do
          (def fs-set @{})
          (each f fs (put fs-set f true))
          (def nxt @{})
          (each f (keys shared)
            (when (get fs-set f) (put nxt f true)))
          (set shared nxt)))))
  (if (nil? shared) @[] (array ;(keys shared))))

(defn- bridge-path
  "Shortest path inside bridge `b` from one attachment to a distinct one."
  [b]
  (def st (b :atts))
  (if (< (length st) 2)
    nil
    (do
      (let [start (st 0)
            badj (table)
            goal-set (table)
            visited @{start true}
            parent (table)
            q @[start]]
        (each [u v] (b :edges)
          (unless (get badj u) (put badj u @[]))
          (unless (get badj v) (put badj v @[]))
          (array/push (get badj u) v)
          (array/push (get badj v) u))
        (each a st (put goal-set a true))
        (var head 0)
        (var goal nil)
        (while (and (nil? goal) (< head (length q)))
          (def cur (q head))
          (set head (+ head 1))
          (if (and (get goal-set cur) (not= cur start))
            (set goal cur)
            (each nxt (or (get badj cur) @[])
              (unless (get visited nxt)
                (put visited nxt true)
                (put parent nxt cur)
                (array/push q nxt)))))
        (if (nil? goal)
          nil
          (do
            (def path @[])
            (var x goal)
            (array/push path x)
            (while (not= x start)
              (set x (get parent x))
              (array/push path x))
            (reverse path)))))))


(defn- embed-path
```Embed bridge path `p` in face `face`.

Split the boundary walk into two new faces and update {in-h, h-verts}.

    P = [p0 … pk], p0/pk on the face, p1..p(k-1) new vertices
  rot = face rotated so p0 first, pk at index j
    A = p0, P1..Pk, rot[j+1..m-1]
    B = p0, rot[1..j-1], pk, P(k-1)..P1

Returns [faces next-id], or nil when p does not fit.
```
  [faces face p h-verts in-h next-id]
  (var result nil)
  (let [vs (face :vs)
        m (length vs)
        k (- (length p) 1)]
    (var i0 -1)
    (for z 0 m (when (= (vs z) (p 0)) (set i0 z) (break)))
    (when (and (not= i0 -1) (> k 0))
      (let [rot (array/concat (array/slice vs i0)
                              (array/slice vs 0 i0))]
        (var j -1)
        (for z 1 m (when (= (rot z) (p k)) (set j z) (break)))
        (when (> j 0)
          (let [a (array/concat @[(p 0)]
                                (array/slice p 1)
                                (array/slice rot (+ j 1)))
                bb (array/concat @[(p 0)]
                                 (array/slice rot 1 j)
                                 @[(p k)]
                                 (reverse (array/slice p 1 k)))
                out @[]]
            (for zz 0 k (put in-h (edge-key (p zz) (p (+ zz 1))) true))
            (for zz 1 k 1 (put h-verts (p zz) true))
            (each f faces (unless (= f face) (array/push out f)))
            (array/push out {:id next-id :vs a})
            (array/push out {:id (+ next-id 1) :vs bb})
            (set result @[out (+ next-id 2)])))))
    result))

(defn- dmp-solve
  ```Run DMP to completion on one 2-connected block. No backtracking.

Takes a state table {:n :adj :h-verts :in-h :faces :next-id}

At every step it recomputes the bridges of the embedded subgraph and, for each,
the set of admissible faces (the faces that contain every attachment vertex)

  - no bridges left            -> the block is planar
  - some bridge has no
    admissible face            -> the block is non-planar
  - otherwise                  -> embed the shortest path of a bridge whose
                                  admissible-face set is smallest (ties by
                                  bridge order) into the first of its faces.

Returns @{:ok bool :faces faces}
```
  [st]
  (var verdict nil)
  (while (nil? verdict)
    (def bridges (bridges-of (get st :n) (get st :adj) (get st :h-verts) (get st :in-h)))
    (if (= (length bridges) 0)
      (set verdict @{:ok true :faces (get st :faces)})
      (do
        (def [ix by-id] (face-index (get st :faces)))
        (var best nil)
        (each b bridges
          (def cf (faces-common ix (b :atts)))
          (when (or (nil? best) (< (length cf) (best 1)))
            (set best @[b (length cf) nil nil])
            (when (> (length cf) 0)
              (put best 2 (cf 0))
              (put best 3 (bridge-path b)))))
        (if (= (best 1) 0)
          (set verdict @{:ok false :faces (get st :faces)})
          (do
            (def step (embed-path (get st :faces) (get by-id (best 2))
                                  (best 3) (get st :h-verts)
                                  (get st :in-h) (get st :next-id)))
            (put st :faces (step 0))
            (put st :next-id (step 1)))))))
  verdict)

(defn- block-embed
  "DMP test for one vertex-biconnected block given as [u v] index pairs.
  Returns [ok faces? vertices] where `ok` is the planarity result."
  [edges]
  (if (< (length edges) 3)
    @[true nil nil]
    (do
      (def verts (table))
      (each [u v] edges (put verts u true) (put verts v true))
      (def vs (keys verts))
      (sort vs)
      (def n (length vs))
      (def idx (table))
      (each v vs (put idx v (length (keys idx))))
      (def adj (array/new n))
      (for z 0 n (put adj z @[]))
      (each [u v] edges
        (array/push (adj (get idx u)) (get idx v))
        (array/push (adj (get idx v)) (get idx u)))
      (def cyc (find-cycle n adj))
      (if (or (nil? cyc) (< (length cyc) 3))
        @[true nil vs]
        (do
          (let [h-verts (table)
                in-h (table)
                faces @[{:id 0 :vs (array/slice cyc)}
                        {:id 1 :vs (reverse cyc)}]
                next-id 2]
            (each v cyc (put h-verts v true))
            (for zz 0 (- (length cyc) 1)
              (put in-h (edge-key (cyc zz) (cyc (+ zz 1))) true))
            (put in-h (edge-key (cyc 0) (cyc (- (length cyc) 1))) true)
            (def res (dmp-solve (table :n n :adj adj
                                       :h-verts h-verts :in-h in-h
                                       :faces faces :next-id next-id)))
            @[(get res :ok) (get res :faces) vs]))))))

(defn- block-planar?
  "DMP test for one vertex-biconnected block given as [u v] index pairs."
  [edges]
  ((block-embed edges) 0))


(defn- undirected-index
  ```Build the index required for block decomposition.

Returns [n adj es] deduplicated edge list, and index->node table.
```
  [g]
  (let [es @[]
        seen (table)
        nb (make-undirected-neighbours-fn g)]
    (each u (nodes g)
      (each v (nb g u)
        (unless (= u v)
          (unless (get-in seen [u v])
            (put seen u (put (or (get seen u) @{}) v true))
            (put seen v (put (or (get seen v) @{}) u true))
            (array/push es [u v])))))
    (def verts (table))
    (each [u v] es (put verts u true) (put verts v true))
    (def vs (keys verts))
    (sort vs (fn [a b] (< (string a) (string b))))
    (def idx (table))
    (each v vs (put idx v (length (keys idx))))
    (let [n (length vs)
          adj (array/new n)]
      (for z 0 n (put adj z @[]))
      (each [u v] es
        (array/push (adj (get idx u)) (get idx v))
        (array/push (adj (get idx v)) (get idx u)))
      @[n adj es vs])))


# ===========================================================================
#  Public API
# ===========================================================================

(define is-planar?
  ```True when the undirected graph `g` is planar.

A planar graph can be embedded in the plane with no two edges crossing.
Uses the Demoucron-Malgrange-Pertuisot face-splitting test on the
vertex-biconnected blocks of `g`. Exact, polynomial, non-heuristic.

Works on any graph type (edges are treated as undirected).

The test never backtracks: at each step it embeds a bridge with the fewest
admissible faces, which DMP proves is safe, so there is no search budget and no
way for the answer to depend on input order.```
  [g :any-graph] :boolean
  (let [[n adj es _vs] (undirected-index g)]
    # cheap necessary condition: m <= 3n - 6 for n >= 3
    (var ok (not (and (>= n 3) (> (length es) (- (* 3 n) 6)))))
    (when ok
      (each blk (vertex-blocks n adj)
        (unless (block-planar? blk)
          (set ok false))))
    ok))


(define planar-faces
  "Return a planar embedding of `g` as a list of faces, or nil if non-planar."
  [g :any-graph] (:or :array :nil)
  (let [[n adj _es vs] (undirected-index g)
        faces @[]]
    (var ok true)
    (each blk (vertex-blocks n adj)
      (def got (block-embed blk))
      (if (got 0)
        (when (got 1)
          (def block-vs (got 2))
          (each f (got 1)
            # compact indices -> global index -> node value
            (def ordered (map (fn [c] (vs (block-vs c))) (f :vs)))
            (array/push faces {:vs ordered})))
        (set ok false)))
    (if ok faces nil)))
