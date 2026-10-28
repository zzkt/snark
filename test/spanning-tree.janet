# -*- mode: janet; -*-
# graphs/test/spanning-tree: determinant, Laplacian, Matrix-Tree, BEST-theorem
#                            Eulerian-circuit counts.

(import ./helper :prefix "")
(import ../graphs :prefix "")
(import ../graphs/util :as u)
(import ./data/graphs :prefix "fx-")

(print "\n* matrix-determinant (Bareiss)")

(cassert-eq "det 1x1" (matrix-determinant @[@[7]]) 7)
(cassert-eq "det 2x2" (matrix-determinant @[@[1 2] @[3 4]]) -2)
(cassert-eq "det identity 3x3" (matrix-determinant @[@[1 0 0] @[0 1 0] @[0 0 1]]) 1)
(cassert-eq "det singular" (matrix-determinant @[@[1 2] @[2 4]]) 0)
(cassert-eq "det empty matrix" (matrix-determinant @[]) 1)

(print "\n* graph-laplacian")

(def k4 (make-graph @{}))
(add-nodes k4 @["a" "b" "c" "d"])
(add-edges k4 ["a" "b"] ["a" "c"] ["a" "d"] ["b" "c"] ["b" "d"] ["c" "d"])

(cassert-eq "K4 laplacian diagonal 3"
            (get-in (graph-laplacian k4) [0 0]) 3)
(cassert-eq "K4 laplacian off-diagonal -1"
            (get-in (graph-laplacian k4) [0 1]) -1)

(print "\n* count-spanning-trees")

(cassert-eq "K4 spanning trees" (count-spanning-trees k4) 16)

(def k3 (make-graph @{}))
(add-nodes k3 @["a" "b" "c"])
(add-edges k3 ["a" "b"] ["b" "c"] ["a" "c"])

(cassert-eq "K3 spanning trees" (count-spanning-trees k3) 3)

(def c4 (fx-cycle 4 fx-alpha->name))
(cassert-eq "C4 spanning trees (cycle on n vertices has n)"
            (count-spanning-trees c4) 4)

(cassert-eq "single vertex has 1 spanning tree"
            (count-spanning-trees
             (do (def sg (make-graph @{}))
                 (add-node sg "s") sg)) 1)

(cassert-eq "empty graph has 0 spanning trees"
            (count-spanning-trees (make-graph @{})) 0)

(print "\n* multigraph and weighted Matrix-Tree")

(def mg (make-multigraph @{}))
(add-nodes mg "a" "b")
(add-edges mg ["a" "b"] ["a" "b"])

(cassert-eq "two parallel edges give 2 spanning trees"
            (count-spanning-trees mg) 2)

(def wg (make-weighted-graph @[] @{}))
(add-nodes wg @["a" "b" "c"])
(add-edges wg ["a" "b" 2] ["b" "c" 3] ["a" "c" 5])
(cassert-eq "weighted triangle (2,3,5) Σ_T ∏ w = 31"
            (count-spanning-trees wg) 31)

(print "\n* count-eulerian-circuits (BEST Theorem)")

(def cycle (make-digraph @[] @{}))
(add-nodes cycle @["0" "1" "2" "3"])
(add-edges cycle ["0" "1" 1] ["1" "2" 1] ["2" "3" 1] ["3" "0" 1])
(cassert-eq "directed 4-cycle has 1 eulerian circuit"
            (count-eulerian-circuits cycle) 1)

(def md (make-multidigraph @{} @{}))
(add-nodes md "0" "1")
(add-edges md ["0" "1"] ["0" "1"] ["1" "0"] ["1" "0"])
(cassert-eq "two arcs each way give 2 circuits"
            (count-eulerian-circuits md) 2)

(def ub (make-digraph @[] @{}))
(add-nodes ub "a" "b")
(add-edge ub "a" "b" 1)
(cassert-eq "unbalanced digraph returns nil"
            (count-eulerian-circuits ub) nil)


(defn euler-arcs [g]
  (def arcs @[])
  (each a (u/nodes g)
    (loop [[b c] :pairs (get (g :adj) a)]
      (for _ 0 (if (number? c) c 1) (array/push arcs @[a b]))))
  arcs)

(defn brute-fixed-first-arc [g fst]
  "Exhaustively count Eulerian circuits of g whose first arc is fst."
  (def arcs (euler-arcs g))
  (def m (length arcs))
  (def start (find-index
              (fn [a] (and (= (a 0) (fst 0))
                           (= (a 1) (fst 1)))) arcs))
  (if (nil? start)
    0
    (do
      (def used (array/new-filled m false))
      (put used start true)
      (var count 0)
      (defn go [v remaining]
        (if (= remaining 0)
          (++ count)
          (for i 0 m
            (unless (get used i)
              (def e (arcs i))
              (when (= (e 0) v)
                (put used i true)
                (go (e 1) (dec remaining))
                (put used i false))))))
      (go (fst 1) (dec m))
      count)))

(defn best-graph [names edges]
  (def g (make-digraph))
  (each n names (add-node g n))
  (each e edges (add-edge g (e 0) (e 1) 1))
  g)

# figure eight. Balanced, the spanning arborescence into "a" is forced,
# so tau = 1 and the product is 1.
(def f8 (best-graph ["a" "b" "c" "d" "e"]
                    [["a" "b"] ["b" "c"] ["c" "a"]
                     ["a" "d"] ["d" "e"] ["e" "a"]]))

(cassert-eq "figure eight BEST count"
            (count-eulerian-circuits f8) 1)
(cassert-eq "figure eight matches brute force with first arc a->b"
            (brute-fixed-first-arc f8 @["a" "b"])
            (count-eulerian-circuits f8))
(cassert-eq "figure eight matches brute force with first arc a->d"
            (brute-fixed-first-arc f8 @["a" "d"])
            (count-eulerian-circuits f8))

# bidirected triangle. every vertex has out-degree 2, tau = 3.
(def bk3 (best-graph ["a" "b" "c"]
                     [["a" "b"] ["b" "a"] ["a" "c"] ["c" "a"]
                      ["b" "c"] ["c" "b"]]))

(cassert-eq "bidirected triangle BEST count"
            (count-eulerian-circuits bk3) 3)
(cassert-eq "bidirected triangle matches brute force"
            (brute-fixed-first-arc bk3 @["a" "b"])
            (count-eulerian-circuits bk3))
(cassert-eq "bidirected triangle agrees on another first arc"
            (brute-fixed-first-arc bk3 @["a" "c"])
            (count-eulerian-circuits bk3))

# a single cycle. exactly one Eulerian circuit per choice of first arc.
(def c4 (best-graph ["a" "b" "c" "d"]
                    [["a" "b"] ["b" "c"] ["c" "d"] ["d" "a"]]))
(cassert-eq "cycle C4 BEST count"
            (count-eulerian-circuits c4) 1)
(cassert-eq "cycle C4 matches brute force"
            (brute-fixed-first-arc c4 @["a" "b"])
            (count-eulerian-circuits c4))

# Two disjoint triangles are balanced but disconnected. count is 0
(def two (best-graph ["a" "b" "c" "d" "e" "f"]
                     [["a" "b"] ["b" "c"] ["c" "a"]
                      ["d" "e"] ["e" "f"] ["f" "d"]]))

(cassert-eq "disconnected balanced digraph counts 0"
            (count-eulerian-circuits two) 0)
(cassert-eq "disconnected brute force agrees"
            (brute-fixed-first-arc two @["a" "b"]) 0)

(print-results)
