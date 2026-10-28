# -*- mode: janet; -*-
# graphs/lca: Tarjan's offline lowest common ancestor

(import deft :prefix "")
(import ./types :prefix "")
(import ./util :prefix "")

(defn- find-set
  "Find with path compression."
  [parent x]
  (def p (get parent x))
  (if (= p x) x
    (do
      (def root (find-set parent p))
      (put parent x root)
      root)))

(defn- union-sets [parent rank x y]
  "Union by rank."
  (def rx (find-set parent x))
  (def ry (find-set parent y))
  (unless (= rx ry)
    (def rx-rank (or (get rank rx) 0))
    (def ry-rank (or (get rank ry) 0))
    (cond
      (< rx-rank ry-rank) (put parent rx ry)
      (> rx-rank ry-rank) (put parent ry rx)
      (do (put parent ry rx) (put rank rx (+ rx-rank 1))))))

(defn- lca-init
  "Shared per-call state for the recursive and iterative LCA variants."
  [g queries]
  (let [nodes (nodes g)
        parent @{}
        rank @{}
        ancestor @{}
        color @{}
        query-map @{}
        result @{}]
    (each n nodes
      (put parent n n)
      (put rank n 0)
      (put color n :white)
      (put query-map n @[]))
    (each [u v] queries
      (unless (get query-map u)
        (error (string "tarjan-lca: query references node '" u
                       "', which is not in the graph")))
      (unless (get query-map v)
        (error (string "tarjan-lca: query references node '" v
                       "', which is not in the graph")))
      (array/push (get query-map u) [v [u v]])
      (array/push (get query-map v) [u [u v]]))
    @{:parent parent :rank rank :ancestor ancestor :color color
      :query-map query-map :result result}))


(define tarjan-lca
  ```Tarjan's offline LCA algorithm.

  `g` is graph
  `root` is the root node
  `queries` are in the form [[u v] ...].

Returns {[u v]: ancestor} for each query pair.
```
  [g :any-graph root :dynamic queries :array] :table
  (let [st (lca-init g queries)
        parent (get st :parent)
        rank (get st :rank)
        ancestor (get st :ancestor)
        color (get st :color)
        query-map (get st :query-map)
        result (get st :result)]
    (defn dfs [u]
      (put color u :gray)
      (put ancestor u u)
      (each v (neighbours g u)
        (when (= (get color v) :white)
          (dfs v)
          (union-sets parent rank u v)
          (put ancestor (find-set parent u) u)))
      (put color u :black)
      (each [other orig-query] (get query-map u)
        (when (= (get color other) :black)
          (def lca-node (get ancestor (find-set parent other)))
          (put result orig-query lca-node))))
    (dfs root)
    result))


(define tarjan-lca-iterative
  ```Tarjan's offline LCA algorithm with an explicit-stack.

Identical to `tarjan-lca` except the dfs is unfolded into a explicit stack,
so nesting is bounded by the graph's vertex count rather than the C stack.
Safe on arbitrarily deep trees (e.g. a 10^5-level path) where the recursive
variant can overflow.

Returns {[u v]: ancestor} for each query pair.
```
  [g :any-graph root :dynamic queries :array] :table
  (let [st (lca-init g queries)
        parent (get st :parent)
        rank (get st :rank)
        ancestor (get st :ancestor)
        color (get st :color)
        query-map (get st :query-map)
        result (get st :result)]
    (def stack @[[:visit root]])
    (while (> (length stack) 0)
      (def frame (array/pop stack))
      (case (frame 0)
        :visit
        (let [u (frame 1)]
          (put color u :gray)
          (put ancestor u u)
          (array/push stack @[:resume u 0 (neighbours g u) nil]))
        :resume
        (let [u (frame 1) i (frame 2) nbrs (frame 3) child (frame 4)]
          (when (not= child nil)
            (union-sets parent rank u child)
            (put ancestor (find-set parent u) u))
          (if (>= i (length nbrs))
            (do
              (put color u :black)
              (each [other orig-query] (get query-map u)
                (when (= (get color other) :black)
                  (def lca-node (get ancestor (find-set parent other)))
                  (put result orig-query lca-node))))
            (do
              (def v (nbrs i))
              (if (= (get color v) :white)
                (do
                  (array/push stack @[:resume u (+ i 1) nbrs v])
                  (array/push stack @[:visit v]))
                (array/push stack @[:resume u (+ i 1) nbrs nil])))))))
    result))
