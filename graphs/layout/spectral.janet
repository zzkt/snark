# -*- mode: janet; -*-
# graphs/layout/spectral: eigenvector layout of the graph Laplacian

(import deft :prefix "")
(import ../types :prefix "")
(import ../util :prefix "")
(import ../record :prefix "r-")

(defn- _laplacian
  "Combinatorial Laplacian L=D-A as a nested array over the canonical node ordering."
  [g nodes]
  (let [n (length nodes)
        node-idx @{}]
    (loop [i :range [0 n]] (put node-idx (nodes i) i))
    (def lapl @[])
    (loop [i :range [0 n]] (array/push lapl (array/new-filled n 0.0)))
    (def deg (array/new-filled n 0.0))
    (each u nodes
      (loop [[v _] :pairs (or (get (r-adjacency g) u) @{})]
        (when (and (not= u v) (not (nil? (get node-idx v))))
          (def i (node-idx u))
          (def j (node-idx v))
          (when (not= (get (lapl i) j) -1.0)
            (put (lapl i) j -1.0)
            (put (lapl j) i -1.0)
            (put deg i (+ 1.0 (get deg i)))
            (put deg j (+ 1.0 (get deg j)))))))
    # diagonal is the weighted degree
    (loop [i :range [0 n]] (put (lapl i) i (get deg i)))
    lapl))

(defn- _jacobi-eigen
  "Cyclic Jacobi eigensolver for a symmetric n x n matrix (nested array).

Returns {:vals [...] :vecs [...]} with eigenvalues ascending and eigenvectors
as columns of :vecs (vecs k i = component i of vector k)."
  [a n max-sweeps]
  (def w a)
  (def vt @[])
  (loop [i :range [0 n]]
    (def col (array/new-filled n 0.0))
    (put col i 1.0)
    (array/push vt col))
  (defn rot [p q c s]
    (def wp (w p))
    (def wq (w q))
    (def vp (vt p))
    (def vq (vt q))
    (loop [i :range [0 n]]
      (let [vip (get vp i)
            viq (get vq i)]
        (put vp i (- (* c vip) (* s viq)))
        (put vq i (+ (* s vip) (* c viq))))
      (let [row (w i)
            aip (get row p)
            aiq (get row q)
            np (- (* c aip) (* s aiq))
            nq (+ (* s aip) (* c aiq))]
        (put row p np)
        (put row q nq)
        (put wp i np)
        (put wq i nq))))
  # Stops as soon as a sweep rotates nothing
  (def rotated @[false])
  (loop [sweep :range [0 max-sweeps]]
    (put rotated 0 false)
    (loop [p :range [0 n]]
      (loop [q :range [(+ p 1) n]]
        (def apq (get (w p) q))
        (when (> (math/abs apq) 1e-15)
          (put rotated 0 true)
          (let [app (get (w p) p)
                aqq (get (w q) q)
                tau (/ (- aqq app) (* 2 apq))
                vp (math/sqrt (+ 1.0 (* tau tau)))
                sgn (if (>= tau 0.0) 1.0 -1.0)
                t (/ sgn (+ (math/abs tau) vp))
                c (/ 1.0 (math/sqrt (+ 1.0 (* t t))))
                s (* t c)]
            (rot p q c s)
            (put (w p) p (- app (* t apq)))
            (put (w q) q (+ aqq (* t apq)))
            (put (w p) q 0.0)
            (put (w q) p 0.0)))))
    (when (not (rotated 0)) (break)))
  # collect eigenvalues (diagonal) and sort ascending
  (def vals @[])
  (loop [i :range [0 n]] (array/push vals @{:lambda (get (w i) i) :i i}))
  (sort vals (fn [x y]
               (def a (x :lambda))
               (def b (y :lambda))
               (if (< (math/abs (- a b)) 1e-12)
                 (< (x :i) (y :i))
                 (< a b))))
  (def evecs @[])
  (loop [i :range [0 n]]
    (array/push evecs (array/slice (vt (get (vals i) :i)))))
  @{:vals (map |($ :lambda) vals) :vecs evecs})


(defn- _canonical-sign
  "Flip a vector in place so its largest-magnitude component is positive."
  [vec]
  (var best 0.0)
  (var best-i -1)
  (loop [i :range [0 (length vec)]]
    (def absv (math/abs (get vec i)))
    (when (or (< best-i 0) (> absv best))
      (set best absv)
      (set best-i i)))
  (when (< (get vec best-i) 0)
    (loop [i :range [0 (length vec)]]
      (put vec i (- 0.0 (get vec i)))))
  vec)


(defn- _components
  "Undirected connected components in deterministic order."
  [g nodes]
  # build the undirected adjacency once: {u -> {v true}} over either arc direction
  (def undirected @{})
  (each u nodes (put undirected u @{}))
  (each u nodes
    (loop [[v _] :pairs (or (get (r-adjacency g) u) @{})]
      (when (and (not= u v) (get undirected v))
        (put (get undirected u) v true)
        (put (get undirected v) u true))))
  (def seen @{})
  (def comps @[])
  (each start nodes
    (unless (get seen start)
      (def comp @[start])
      (put seen start true)
      (var qi 0)
      (while (< qi (length comp))
        (def u (comp qi))
        (loop [[v _] :pairs (get undirected u)]
          (when (not (get seen v))
            (put seen v true)
            (array/push comp v)))
        (++ qi))
      (array/push comps comp)))
  comps)


(defn- _typical-spacing
  "Mean distance from each of a sample of `pos` points to its closest other."
  [pos]
  (let [ks (keys pos)
        n (length ks)]
    (if (< n 2) nil
    (do
    (def step (max 1 (div n 200)))
    (var tot 0.0) (var cnt 0)
    (var i 0)
    (while (< i n)
      (def a (get pos (ks i)))
      (def ax (a 0)) (def ay (a 1))
      (var best nil)
      (loop [j :range [0 n]]
        (unless (= i j)
          (let [b (get pos (ks j))
                dx (- ax (b 0))
                dy (- ay (b 1))
                d (math/sqrt (+ (* dx dx) (* dy dy)))]
            (if (or (nil? best) (< d best)) (set best d)))))
      (when (and best (> best 0)) (+= tot best) (++ cnt))
      (+= i step))
    (if (zero? cnt) nil (/ tot cnt))))))

(defn- _degeneracy
  "How badly this embedding collapsed (in units used by to-svg)."
  [pos]
  (def sp (_typical-spacing pos))
  (if (or (nil? sp) (<= sp 0))
    nil
    (let [want (/ 60.0 sp)]
      (if (> want 1500.0) [true sp want] nil))))


(define layout-spectral
  ```Deterministic spectral layout (Puppe 2008)

 Places nodes at the coordinates given by eigenvectors of the graph Laplacian.
 Strongly connected communities cluster together and disconnected graphs are
 placed per connected component. Not suitable for all graphs.

   &keys
     :width    — canvas width  (default 10.0)
     :height   — canvas height (default 8.0)
     :sweeps   — Jacobi sweeps (default 40)

Returns {:pos {node [x y]} :eligible true :eigenvalues [...]}
```
  [g :weighted & keys] :table
  (let [keys (table ;keys)
        nodes (canonical-nodes (r-nodes g))
        n (length nodes)
        width (get keys :width 10.0)
        height (get keys :height 8.0)
        sweeps (get keys :sweeps 40)]
    (cond
      (<= n 0) @{:pos @{} :eligible true :eigenvalues @[]}
      (= n 1) @{:pos @{(nodes 0) [(/ width 2) (/ height 2)]}
                :eligible true :eigenvalues @[0.0]}
      (let [comps (_components g nodes)]
        # Multiple connected components. Solve each component separately
        (when (> (length comps) 1)
          (let [ncomps (length comps)
                total (sum (map length comps))
                width-of (fn [nc] (* width (/ nc total)))
                y-off (/ (- height (min width height)) 2)
                all-vals @[]
                pos @{}]
            (loop [ci :range [0 ncomps]]
              (def comp (comps ci))
              (def nc (length comp))
              (def slot-x0 (sum (map (fn [k] (width-of (length (comps k))))
                                     (range ci))))
              (def slot (width-of nc))
              (def slot-cx (+ slot-x0 (/ slot 2)))
              (if (= nc 1)
                (do (put pos (comp 0) [slot-cx (/ height 2)])
                    (array/push all-vals 0.0))
                (let [ev (_jacobi-eigen (_laplacian g comp) nc sweeps)
                      v1 (_canonical-sign (get (ev :vecs) 1))
                      v2 (if (>= nc 3)
                           (_canonical-sign (get (ev :vecs) 2))
                           (array/new-filled nc 0.0))]
                  (var x0 1e18) (var x1 -1e18)
                  (var y0 1e18) (var y1 -1e18)
                  (loop [i :range [0 nc]]
                    (set x0 (min x0 (v1 i))) (set x1 (max x1 (v1 i)))
                    (set y0 (min y0 (v2 i))) (set y1 (max y1 (v2 i))))
                  (let [xs (- x1 x0)
                        ys (- y1 y0)]
                    (loop [i :range [0 nc]]
                      (def xc (if (> xs 1e-9) (/ (- (v1 i) x0) xs) 0.5))
                      (def yc (if (> ys 1e-9) (/ (- (v2 i) y0) ys) 0.5))
                      (put pos (comp i)
                           [(+ (- slot-cx (/ slot 2)) (* xc (* slot 0.95)))
                            (+ y-off (* yc (min width height)))])))
                  (each lv (ev :vals) (array/push all-vals lv)))))
            (sort all-vals <)
            (break @{:pos pos :eligible true :eigenvalues all-vals
                     :degeneracy (_degeneracy pos)})))
        # single component: the Fiedler vector sets x, the next one sets y
        (let [ev (_jacobi-eigen (_laplacian g nodes) n sweeps)
              vals (ev :vals)
              vecs (ev :vecs)
              v1 (vecs 1)
              v2 (if (>= n 3) (vecs 2) (array/new-filled n 0.0))]
          (_canonical-sign v1)
          (_canonical-sign v2)
          # scale to fit the canvas
          (var xmin 1e18) (var xmax -1e18)
          (var ymin 1e18) (var ymax -1e18)
          (loop [i :range [0 n]]
            (def x (get v1 i))
            (def y (get v2 i))
            (set xmin (min xmin x)) (set xmax (max xmax x))
            (set ymin (min ymin y)) (set ymax (max ymax y)))
          (let [rx (- xmax xmin)
                ry (- ymax ymin)
                scale (if (> (max rx ry) 1e-9)
                        (/ (min width height) (max rx ry))
                        1.0)
                cx (/ width 2)
                cy (/ height 2)
                mx (/ (+ xmin xmax) 2)
                my (/ (+ ymin ymax) 2)
                pos @{}]
            (loop [i :range [0 n]]
              (put pos (nodes i)
                   [(+ cx (* (- (get v1 i) mx) scale))
                    (+ cy (* (- (get v2 i) my) scale))]))
            @{:pos pos :eligible true :eigenvalues vals
               :degeneracy (_degeneracy pos)}))))))
