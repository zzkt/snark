# -*- mode: janet; -*-
# graphs/cst: a succinct compressed suffix tree (CST)

# A compact suffix-tree representation (Sadakane/Grossi-Vitter)
#   :sa    the suffix array
#   :lcp   the LCP array (Kasai)
#   :rmq   a sparse table over LCP (for LCA / lcp(i,j) in O(1))
#   :bp    a balanced-parentheses (DFUDS) encoding of the tree topology
#   :text  the sentinel-terminated input
#
# The core suffix-tree queries (i.e.parent/child/depth/suffix-link/lcp)
# are resolved from the compact pieces rather than the full explicit tree.

# References:
#   K. Sadakane, "Compressed suffix trees with full functionality" (2007)
#   Kärkkäinen, Sanders, Burkhardt; Kasai et al.

(import deft :prefix "")
(import ./suffix-tree :prefix "")


# ==============================================================================
#  Construction
# ==============================================================================

(defn- kasai-lcp
  ```LCP array where lcp[i] is the length of the longest common prefix of suffixes
sa[i] and sa[i+1], for i in [0, n-2). Linear-time Kasai.```
  [text sa]
  (let [n (length sa)
        rank (array/new-filled n 0)]
    (each i (range n) (put rank (get sa i) i))
    (def lcp (array/new-filled (max 0 (dec n)) 0))
    (var k 0)
    (for i 0 n
      (def r (get rank i))
      (if (not= r (dec n))
        (do
          (def j (get sa (+ r 1)))
          (while (and (< (+ i k) n) (< (+ j k) n)
                      (= (get text (+ i k)) (get text (+ j k))))
            (++ k))
          (put lcp r k)
          (set k (max 0 (dec k))))))
    lcp))


# Block width for the range-minimum structure
(def rmq-block 32)

(defn- build-rmq
  ```Range-minimum structure over `lcp` to find min over lcp[lo..hi].

Uses block decomposition rather than a full sparse table.The array is cut into
blocks of `rmq-block` size, each block's minimum is kept, and only those minima
are used. Queries stay around O(1) in n and the structure remains O(n) space.

 The `rmq-block` constant trades space against introducing latency with block
scan, raise it if space matters more than query latency.

Returns table {:lcp array :bmin block-minima :st sparse-table-over-bmin
               :nb block-count :n length}
```
  [lcp]
  (let [n (length lcp)
        nb (if (> n 0) (math/floor (/ (+ n (- rmq-block 1)) rmq-block)) 0)]
    # per-block minima
    (def bmin @[])
    (for b 0 nb
      (def lo (* b rmq-block))
      (def hi (min n (+ lo rmq-block)))
      (var m (get lcp lo))
      (for i (+ lo 1) hi
        (def v (get lcp i))
        (when (< v m) (set m v)))
      (array/push bmin m))
    # sparse table over block minima only
    (def st @[bmin])
    (when (> (length bmin) 1)
      (def lg (math/floor (math/log2 (+ (length bmin) .0000001))))
      (for lvl 1 (+ lg 1)
        (def len (length (get st 0)))
        (def half (math/pow 2 (dec lvl)))
        (def row (array/new (max 0 (- len half))))
        (for i 0 (- len half)
          (def a (get (get st (dec lvl)) i))
          (def b (get (get st (dec lvl)) (+ i half)))
          (array/push row (if (< a b) a b)))
        (array/push st row)))
    {:lcp lcp :bmin bmin :st st :nb nb :n n}))


(defn- lcp-rmq
  ```Range-minimum query over the LCP array.
Returns min of lcp[lo..hi], or 0 when the range is empty.```
  [rmq lo hi]
  (if (> lo hi)
    0
    (do
      (def lcp (get rmq :lcp))
      (def n (get rmq :n))
      (def b0 (math/floor (/ lo rmq-block)))
      (def b1 (math/floor (/ hi rmq-block)))
      (if (= b0 b1)
        # entirely inside one block
        (do
          (var m (get lcp lo))
          (for i (+ lo 1) (+ hi 1)
            (def v (get lcp i))
            (when (< v m) (set m v)))
          m)
        (do
          # tail of the first block
          (def lend (min n (* rmq-block (inc b0))))
          (var m (get lcp lo))
          (for i (+ lo 1) lend
            (def v (get lcp i))
            (when (< v m) (set m v)))
          # head of the last block
          (for i (* rmq-block b1) (+ hi 1)
            (def v (get lcp i))
            (when (< v m) (set m v)))
          # whole blocks in between (via the sparse table)
          (when (< (+ b0 1) b1)
            (def s0 (+ b0 1))
            (def s1 (dec b1))
            (def st (get rmq :st))
            (def k (math/floor (math/log2 (- (+ s1 1) s0))))
            (def bm (min (get (get st k) s0)
                         (get (get st k) (- (+ s1 1) (math/pow 2 k)))))
            (when (< bm m) (set m bm)))
          m)))))


# ==============================================================================
#  Balanced-topology (DFUDS)
#
#  DFUDS visit nodes in preorder, for a node with n children emit n '(' then
#  one ')'. Each node's position and degree is kept to support navigation
#  without using an explicit tree.
# ==============================================================================

(defn- build-bp
  ```Encode `tree` topology in DFUDS preorder.

Returns {:duds string :open array node->index
         :deg array node->degree :node array index->node}
```
  [tree adj]
  (let [nid (inc (length (get tree :edges)))
        duds (buffer/new 64)
        open (array/new-filled nid 0)
        deg (array/new-filled nid 0)
        index->node @[]]
    # frame is [node next-child-index] and closed on the way up.
    (def stack @[@[0 0]])
    (var frame (stack 0))
    (defn- enter [v]
      (def c (get adj v))
      (def k (if c (length c) 0))
      (put deg v k)
      (put open v (length duds))
      (array/push index->node v)
      (for _ 0 k (buffer/push-byte duds 40)))   # '('
    (enter 0)
    (while (> (length stack) 0)
      (def v (frame 0))
      (def i (frame 1))
      (def c (get adj v))
      (if (and c (< i (length c)))
        (do
          (put frame 1 (+ i 1))
          (def cv (get (c i) :child))
          (enter cv)
          (def nf @[cv 0])
          (array/push stack nf)
          (set frame nf))
        (do
          (buffer/push-byte duds 41)          # ')'
          (array/pop stack)
          (when (> (length stack) 0)
            (set frame (stack (dec (length stack))))))))
    {:duds (string duds) :open open :deg deg :node index->node}))


# ==============================================================================
#  Compressed Suffix Tree
# ==============================================================================

(define cst
  ```Build a compressed suffix tree from `text`.

   Returns a table:
     :text  the sentinel-terminated input string
     :sa    suffix array (leaf order of suffixes, 0-based starts)
     :lcp   LCP array, where lcp[i] is lcp of sa[i], sa[i+1]
     :rmq   block-decomposed RMQ over :lcp for O(1) range-min
     :bp    DFUDS balanced-parentheses encoding of tree topology
```
  [text :string]
  (def tree (suffix-tree text))  # :text is sentinel-terminated
  (def text* (get tree :text))
  (def adj (get tree :adj))
  (def sa (suffix-array-from-adj adj text*))
  (def lcp (kasai-lcp text* sa))
  {:text text*
   :sa sa
   :lcp lcp
   :rmq (build-rmq lcp)
   :bp (build-bp tree adj)})

(defn cst-sa
  "The suffix array of a CST."
  [t] (get t :sa))

(defn cst-lcp-array
  "The LCP array of a CST (length n-1)."
  [t] (get t :lcp))

(defn cst-lcp
  ```LCP (in characters) between suffixes at SA positions `i` and `j` of a CST.```
  [t i j]
  (if (= i j)
    (- (length (get t :text)) (get (get t :sa) i))
    (let [lo (min i j) hi (max i j)]
      (lcp-rmq (get t :rmq) lo (dec hi)))))

(defn- lcp-at
  "lcp[i] for i in range, returning -1 (unset) just outside [0, n-2]."
  [t i]
  (def lcp (get t :lcp))
  (def nn (length lcp))
  (if (and (>= i 0) (< i nn)) (get lcp i) -1))

(defn- node-depth
  "String depth (root-to-node label length) of the SA interval [i, j]."
  [t i j]
  (cst-lcp t i j))

(define cst-parent
  ```Parent of suffix-tree node represented by SA interval [i, j] (inclusive).

Returns struct {:lo :hi :depth} or nil for the root.```
  [t i :integer j :integer] [:or :struct :nil]
  (def d (node-depth t i j))
  (if (<= d 0)
    nil
    (let [lcp (get t :lcp)
          n (length (get t :sa))
          lv (lcp-at t (dec i))
          rv (lcp-at t j)
          pl (max lv rv)]
      (var lo i)
      (while (and (> lo 0) (>= (get lcp (dec lo)) pl)) (set lo (dec lo)))
      (var hi j)
      (while (and (< hi (dec n)) (>= (get lcp hi) pl)) (set hi (inc hi)))
      {:lo lo :hi hi :depth pl})))

(define cst-children
  ```Children of suffix-tree node represented by SA interval [i, j] (inclusive).

Retuns an array of {:lo :hi :depth} intervals. Leaf nodes (i == j) give empty list.
```
  [t i :integer j :integer] :array
  (def d (node-depth t i j))
  (def text (get t :text))
  (def sa (get t :sa))
  (def lcp (get t :lcp))
  (def rmq (get t :rmq))
  (def res @[])
  (when (< i j)
    (defn push-child [s e]
      (if (= s e)
        (array/push res {:lo s :hi e :depth (- (length text) (get sa s))})
        (array/push res {:lo s :hi e :depth (lcp-rmq rmq s (dec e))})))
    (var start i)
    (for k i j
      (if (= (get lcp k) d)
        (do
          (push-child start k)
          (set start (+ k 1)))))
    (push-child start j))
  res)


(define cst-child
  ```The child of node given by SA interval [i, j] whose first edge label
character is `ch`, or nil if none exists.```
  [t i :integer j :integer ch]
  (def sa (get t :sa))
  (def text (get t :text))
  (def d (node-depth t i j))
  (def kids (cst-children t i j))
  (first (filter |(= ch (get text (+ (get sa (get $ :lo)) d))) kids)))


# ==============================================================================
#  Shared interface with graphs/suffix-tree
#
#  cst and suffix-tree provide two representations of the same object, so the
#  following functions are semantically equivalent, with an identical interface.
#
#   (build text)                     -> build the structure for suffix-search
#   (suffix-search structure pat)    -> array of 0-based start positions
#   (suffix-array text)              -> array of suffix start positions
#   (longest-repeated-substring text)
# ==============================================================================

(def build :shadow
  ```Alias for `cst`, so cst and suffix-tree modules are interchangeable.```
  cst)

(defn suffix-array :shadow
  "The suffix array of `text` (0-indexed, lexicographic)."
  [text] (get (cst text) :sa))

(defn longest-repeated-substring :shadow
  ```Longest repeated substring of `text`, or an empty string if no repeats.```
  [text]  # [text :string] :string
  (def t (cst text))
  (def sa (get t :sa))
  (def lcp (get t :lcp))
  (var best 0)
  (var at 0)
  (for k 0 (length lcp)
    (def v (get lcp k))
    (when (> v best)
      (set best v)
      (set at k)))
  (def start (get sa at 0))
  (string/slice (get t :text) start (+ start best)))

(defn suffix-search :shadow
  ```Return a sorted array of starting positions where `pattern` occurs in the
text of  CST `t` or an empty array if there are no matches.```
  [t pattern] # [t pattern :string] :array
  (def text (get t :text))
  (def sa (get t :sa))
  (def n (length text))
  (def m (length pattern))
  (if (= m 0)
    (range n)
    (do
      (defn cmp-suf [pos]
        # compare suffix beginning at sa[pos] against pattern (in place).
        (def start (get sa pos))
        (def L (- n start))
        (var k 0)
        (while (and (< k L) (< k m)
                    (= (get text (+ start k)) (get pattern k)))
          (++ k))
        (if (= k m) 0
          (if (= k L) -1
            (if (< (get text (+ start k)) (get pattern k)) -1 1))))
      # lower bound: first position with suffix >= pattern
      (defn lower []
        (var lo 0) (var hi (length sa))
        (while (< lo hi)
          (def mid (math/floor (/ (+ lo hi) 2)))
          (if (< (cmp-suf mid) 0) (set lo (+ mid 1)) (set hi mid)))
        lo)
      # upper bound: first position with suffix > pattern
      (defn upper []
        (var lo 0) (var hi (length sa))
        (while (< lo hi)
          (def mid (math/floor (/ (+ lo hi) 2)))
          (if (< (cmp-suf mid) 1) (set lo (+ mid 1)) (set hi mid)))
        lo)
      (def L (lower))
      (def R (upper))
      (def result @[])
      (for i L R (array/push result (get sa i)))
      (sort result))))
