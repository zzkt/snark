# -*- mode: janet; -*-
# graphs/suffix-tree: suffix trees

(import deft :prefix "")

# ==============================================================================
#  Construction (Ukkonen)
#
#   A suffix tree is a fully-compacted trie of all suffixes of a string. It can
#   be used to answer questions about substrings; whether a string occurs, how
#   many times it occurs, where it occurs, the longest repeated substring, the
#   longest substring shared by two strings, and so on.
#
#   The tree is built using Ukkonen's algorithm for linear-time construction.
#   Adapted from Gusfield, "Algorithms on Strings, Trees, and Sequences" Ch.6
#   Construction runs in amortized O(n) time (linear for a fixed alphabet) and
#   O(n) space, compared to the O(n^2) cost of inserting suffixes into a trie.
# ==============================================================================

(defn- ukkonen-build
  ```Build the suffix tree of `text` using Ukkonen's algorithm.

Nodes are stored in parallel arrays (one per field)
   L array of :l (half-open [l, r) start; leaves keep l)
   R array of :r (leaves use r = (length text) sentinel)
   P array of :par (root's parent is -1)
   link array of :link (root's link is -1)
   next array of :next (per-node map byte -> child node id)

Node 0 is the root, and node i is the i-th slot of every array.

Returns an array of edge records {:parent p :child c :label s}
```
  [text]
  (let [n (length text)
        L (array/new n)
        R (array/new n)
        P (array/new n)
        link (array/new n)
        _next (array/new n)]
    (defn mk [l r par]
      (let [id (length L)]
        (array/push L l)
        (array/push R r)
        (array/push P par)
        (array/push link -1)
        (array/push _next @{})
        id))
    (def root (mk 0 0 -1))
    (defn node-len [v] (- (get R v) (get L v)))
    (defn node-next [v] (get _next v))
    # walk from st=(v,pos) along text[l..r-1]
    (defn go [st l r]
      (var v (get st 0))
      (var pos (get st 1))
      (var ll l)
      (var bad false)
      (var stop false)
      (while (and (< ll r) (not bad) (not stop))
        (if (= pos (node-len v))
          (let [ch (get (node-next v) (get text ll))]
            (if (nil? ch)
              (set bad true)
              (do (set v ch) (set pos 0))))
          (if (not= (get text (+ (get L v) pos)) (get text ll))
            (set bad true)
            (if (< (- r ll) (- (node-len v) pos))
              (do (set pos (+ pos (- r ll))) (set stop true))
              (do (set ll (+ ll (- (node-len v) pos)))
                  (set pos (node-len v)))))))
      (if bad @[-1 -1] @[v pos]))
    # split st=(v,pos) and return node id at that point.
    (defn split [st]
      (def v (get st 0))
      (def pos (get st 1))
      (if (= pos (node-len v))
        v
        (if (= pos 0)
          (get P v)
          (let [vl (get L v)
                id (mk vl (+ vl pos) (get P v))]
            (put (node-next (get P v)) (get text vl) id)
            (put (node-next id) (get text (+ vl pos)) v)
            (put P v id)
            (put L v (+ vl pos))
            id))))
    (defn get-link [v]
      (if (not= (get link v) -1)
        (get link v)
        (if (= (get P v) -1)
          root
          (let [pv (get P v)
                to (get-link pv)
                extra (if (= pv root) 1 0)
                res (split (go @[to (node-len to)] (+ (get L v) extra) (get R v)))]
            (put link v res)
            res))))
    (defn tree-extend [pos pv ppos]
      (var pv2 pv)
      (var ppos2 ppos)
      (var done false)
      (while (not done)
        (let [nptr (go @[pv2 ppos2] pos (+ pos 1))]
          (if (not= (get nptr 0) -1)
            (do (set pv2 (get nptr 0)) (set ppos2 (get nptr 1)) (set done true))
            (let [mid (split @[pv2 ppos2])
                  leaf (mk pos n mid)]
              (put (node-next mid) (get text pos) leaf)
              (set pv2 (get-link mid))
              (set ppos2 (node-len pv2))
              (if (= mid root) (set done true))))))
      @[pv2 ppos2])
    (var pv root)
    (var ppos 0)
    (for pos 0 n
      (def res (tree-extend pos pv ppos))
      (set pv (get res 0))
      (set ppos (get res 1)))
    (def edges @[])
    (for id 1 (length L)
      (array/push edges @{:parent (get P id)
                          :child id
                          :l (get L id)
                          :r (get R id)}))
    edges))


(defn- build-adj
  ```Build parent -> sorted outgoing-edges adjacency for `tree`.
One pass over the edge array to bucket edges by parent, then one sort per node.```
  [tree]
  (let [txt (get tree :text)
        adj @{}]
    (each e (get tree :edges)
      (def p (get e :parent))
      (if (get adj p)
        (array/push (get adj p) e)
        (put adj p @[e])))
    (each p (keys adj)
      (put adj p
        (sort (get adj p)
              (fn [a b] (< (get txt (get a :l)) (get txt (get b :l)))))))
    adj))

(defn- tree-adj
  ```The precomputed adjacency from `tree`.
Assuming no changes, queries can use this rather than rescanning the record.```
  [tree]
  (get tree :adj))

(defn- ensure-sentinel
  ```Return `text` with a terminator appended, unless it already ends in one.

Every suffix of the terminated string needs its own leaf (ie. no suffix is a
prefix of another), to keep the suffix array correct for strings with suffixes
that are prefixes of one another (e.g. "ABA", "AAAA").
This property requires the terminator sentinel to be unique and final.

The character '$' is appended if '$' does not occur in the string, and otherwise
the first byte not present in the text is used.
```
  [text]
  (def n (length text))
  (if (= n 0)
    text
    (do
      (defn- byte-str [b] (string/from-bytes b))
      (defn- byte-used? [b] (> (length (string/find-all (byte-str b) text)) 0))
      (def last-byte (get text (dec n)))
      (def dollar-hits (length (string/find-all (byte-str 36) text)))
      (cond
        # terminated by a unique final '$'
        (and (= dollar-hits 1) (= last-byte 36)) text
        # '$' is free to use
        (zero? dollar-hits) (string text "$")
        # '$' occurs somewhere in the string, so pick an unused byte
        (do
          (var b 1)
          (while (and (< b 256) (byte-used? b))
            (++ b))
          (if (< b 256)
            (string text (byte-str b))
            # Pathological case where no terminator exists.
            # text is of length >= 256 and contains 256 distinct values
            (error "suffix-tree: no unused byte available as a terminator")))))))


(defn suffix-tree
  ```Build and return the suffix tree of `text`.

 Returns a table
   :text    the (sentinel-terminated) input string
   :edges   array of edge records {:parent p :child c :l start :r end}
   :adj     parent -> sorted outgoing-edge adjacency
```
  [text]
  (let [ttext (ensure-sentinel text)
        edges (ukkonen-build ttext)]
    {:text ttext :edges edges :adj (build-adj {:text ttext :edges edges})}))


(defn edge-label
  "Return the label of edge `e` in `tree`."
  [tree e]
  (string/slice (get tree :text) (get e :l) (get e :r)))


(defn suffix-tree-edge-labels
  ```Return an array of edge labels of the suffix tree of `text`.
This generates the full label set, which is O(n^2) worst case.```
  [text]
  (def tree (suffix-tree text))
  (map |(edge-label tree $) (get tree :edges)))


# ==============================================================================
#  Traversal
# ==============================================================================

(defn- parent-of
  "Return a table mapping each child node id -> its incoming edge."
  [tree]
  (def m @{})
  (each e (get tree :edges) (put m (get e :child) e))
  m)

(defn- path-label
  "Concatenate the edge labels from root of `tree` to node `v`."
  [tree by-child v]
  (var out "")
  (var cur v)
  (while (not (nil? (get by-child cur)))
    (set out (string (edge-label tree (get by-child cur)) out))
    (set cur (get (get by-child cur) :parent)))
  out)


# ==============================================================================
#  Suffix array
# ==============================================================================

(defn suffix-array-from-adj
  ```Compute suffix array from parent -> sorted-outgoing-edge adjacency `adj`
over a suffix tree of `text`, via in-order leaf walk.
```
  [adj text]
  (let [n (length text)
        sa @[]
        stack @[]]
    # frame is [node depth next-child-index], root is at depth 0
    (var frame @[0 0 0])
    (array/push stack frame)
    (while (> (length stack) 0)
      (def v (frame 0))
      (def depth (frame 1))
      (def i (frame 2))
      (def chs (get adj v))
      (if (and chs (< i (length chs)))
        (do
          # advance frame before descending so it resumes at next child
          (put frame 2 (+ i 1))
          (def c (chs i))
          (def cv (get c :child))
          (def cdepth (+ depth (- (get c :r) (get c :l))))
          (def sub (get adj cv))
          (if (or (nil? sub) (= 0 (length sub)))
            (array/push sa (- n cdepth))
            (do
              (def _next @[cv cdepth 0])
              (array/push stack _next)
              (set frame _next))))
        (do
          (array/pop stack)
          (when (> (length stack) 0)
            (set frame (stack (dec (length stack))))))))
    sa))


# ==============================================================================
#  Shared interface with graphs/cst
#
#  cst and suffix-tree provide two representations of the same object, so the
#  following functions are semantically equivalent, with an identical interface.
#
#   (build text)                     -> build the structure for suffix-search
#   (suffix-search structure pat)    -> array of 0-based start positions
#   (suffix-array text)              -> array of suffix start positions
#   (longest-repeated-substring text)
# ==============================================================================

(def build
  ```Alias for `suffix-tree`.```
  suffix-tree)

(defn suffix-array
  ```Return the suffix array of `text`, the starting positions of all suffixes
of `text`, sorted lexicographically.```
  [text]
  (def tree (suffix-tree text))
  (suffix-array-from-adj (tree-adj tree) (get tree :text)))

(defn suffix-search
  ```Return a sorted array of starting positions where `pattern` occurs in the
text of `tree`, or an empty array if the pattern is not found.```
  [tree pattern]
  (def text (get tree :text))
  (def m (length pattern))
  (if (= m 0)
    (range (length text))
    (do
      (def adj (tree-adj tree))
      (var node 0)
      (var node-depth 0)
      (var i 0)
      (var ok true)
      (while (and ok (< i m))
        (def edge
          (find |(= (get text (get $ :l)) (get pattern i))
                (or (get adj node) @[])))
        (if (nil? edge)
          (set ok false)
          (do
            (def l0 (get edge :l))
            (def r0 (get edge :r))
            (def LM (- r0 l0))
            (var k 0)
            (while (and (< k LM) (< (+ i k) m)
                        (= (get text (+ l0 k)) (get pattern (+ i k))))
              (++ k))
            # a mismatch inside both the label and the pattern implies that the
            # pattern does not occur, otherwise consume edge and continue.
            (if (and (< k LM) (< (+ i k) m))
              (set ok false)
              (do
                (set node (get edge :child))
                (set node-depth (+ node-depth LM))
                (set i (+ i k)))))))
      (if (not ok)
        @[]
        (do
          # the match ends at or past the node, so every descending suffix
          # starts with the pattern. collect all leaf starting positions.
          (def starts @[])
          (defn collect [v depth-from-root]
            (def chs (or (get adj v) @[]))
            (if (= (length chs) 0)
              (array/push starts (- (length text) depth-from-root))
              (each c chs
                (collect (get c :child) (+ depth-from-root (- (get c :r) (get c :l)))))))
          (collect node node-depth)
          (sort starts))))))


(defn longest-repeated-substring
  ```Find the longest substring that occurs at least twice in `text`.

Returns the substring, or an empty string if no substring repeats.```
  [text]
  (let [tree (suffix-tree text)
        adj (tree-adj tree)
        by-child (parent-of tree)]
    (var best "")
    (each e (get tree :edges)
      (def child (get e :child))
      (when (> (length (or (get adj child) @[])) 0)
        (def lbl (path-label tree by-child child))
        (when (or (> (length lbl) (length best))
                  # tie break lexicographically
                  (and (= (length lbl) (length best))
                       (< lbl best)))
          (set best lbl))))
    best))
