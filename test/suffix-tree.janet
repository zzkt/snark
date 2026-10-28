# -*- mode: janet; -*-
# graphs/test/suffix-tree & cst: test both suffix-tree implementations

(import ./helper :prefix "")
(import ../graphs/cst :as cst)
(import ../graphs/suffix-tree :as st)

(defn join [a] (string/join (map string a) " "))

#  The shared interface
#   (build text)                      -> a structure suffix-search accepts
#   (suffix-search structure pattern) -> array of 0-based start positions
#   (suffix-array text)               -> array of suffix start positions
#   (longest-repeated-substring text) -> string, empty when nothing repeats
(def apis
  @[["suffix-tree"
     @{:build st/build
       :search st/suffix-search
       :sa st/suffix-array
       :lrs st/longest-repeated-substring}]
    ["cst"
     @{:build cst/build
       :search cst/suffix-search
       :sa cst/suffix-array
       :lrs cst/longest-repeated-substring}]])

# ==============================================================================
#  Shared API: suffix array
# ==============================================================================

(print "\n* suffix array (both implementations)")

# [text expected-suffix-array]
(def sa-cases
  @[["banana$" "6 5 3 1 0 4 2"]
    ["AAAAAA$" "6 5 4 3 2 1 0"]
    ["aaaa$" "4 3 2 1 0"]
    ["ABA" "3 2 0 1"]
    ["abcdef" "6 0 1 2 3 4 5"]
    ["AACGATAGCGGTAGA$" "15 14 0 1 12 6 4 2 8 13 3 7 9 10 11 5"]
    ["mississippi$" "11 10 7 4 1 0 9 8 6 3 5 2"]
    ["ATATCGTTTTATCGTT$" "16 0 10 2 12 4 13 5 15 9 1 11 3 14 8 7 6"]
    ["Paasisinnaannginnakkummi"
     "24 0 9 1 17 10 2 13 23 6 14 4 18 19 22 21 8 16 12 7 15 11 5 3 20"]])

(each [who api] apis
  (each [text want] sa-cases
    (cassert-eq (string who " suffix-array " text)
                (join ((get api :sa) text)) want)))

(each [text want] sa-cases
  (cassert-eq (string "cst-sa " text) (join (cst/cst-sa (cst/cst text))) want))

# ==============================================================================
#  Shared API: suffix-search
# ==============================================================================

(print "\n* suffix-search (both implementations)")

# [text pattern expected-positions]
(def search-cases
  @[["banana$" "na" "2 4"]
    ["banana$" "ana" "1 3"]
    ["banana$" "a" "1 3 5"]
    ["banana$" "nana$" "2"]
    ["banana$" "banana$" "0"]
    ["banana$" "ban" "0"]
    ["banana$" "$" "6"]
    ["banana$" "xyz" ""]
    ["banana$" "ananas" ""]
    ["banana$" "zzz" ""]
    ["AAAAAA$" "AA" "0 1 2 3 4"]
    ["ABA" "A" "0 2"]
    ["ABA" "B" "1"]
    ["aaaa$" "a" "0 1 2 3"]
    ["aaaa$" "aa" "0 1 2"]
    ["mississippi$" "issi" "1 4"]
    ["mississippi$" "ppi" "8"]
    ["abcdef" "cd" "2"]
    ["Paasisinnaannginnakkummi" "inna" "6 14"]])

(each [who api] apis
  (each [text pat want] search-cases
    (def struct ((get api :build) text))
    (cassert-eq (string who " search " text " / " pat)
                (join ((get api :search) struct pat)) want)))

# The two must agree on every case
(def st-api (get (get apis 0) 1))
(def cst-api (get (get apis 1) 1))
(each [text pat want] search-cases
  (cassert-eq (string "implementations agree on " text " / " pat)
              (join ((get cst-api :search) ((get cst-api :build) text) pat))
              (join ((get st-api :search) ((get st-api :build) text) pat))))

# ==============================================================================
#  Explicit tree only: edge labels
# ==============================================================================

(print "\n* suffix-tree-edge-labels")

(defn sorted-labels [text]
  (sort (st/suffix-tree-edge-labels text) |(< $0 $1)))

(cassert-eq "ATAAATG$ has 12 compacted edges"
            (length (st/suffix-tree-edge-labels "ATAAATG$")) 12)

(cassert-arr-eq "ATAAATG$ labels match"
                (sorted-labels "ATAAATG$")
                @["$" "A" "A" "AAATG$" "AAATG$"
                  "ATG$" "G$" "G$" "G$" "T" "T" "TG$"])

(cassert-eq "banana$ edge count"
            (length (st/suffix-tree-edge-labels "banana$")) 10)

# ==============================================================================
#  Shared API: longest-repeated-substring
# ==============================================================================

(print "\n* longest-repeated-substring")

# [text expected]
(def lrs-cases
  @[["banana$" "ana"]
    ["Paasisinnaannginnakkummi" "inna"]
    ["ATATCGTTTTATCGTT$" "TATCGTT"]
    ["aaaa$" "aaa"]
    ["AAAAAA$" "AAAAA"]
    ["mississippi$" "issi"]
    ["ATAAATG$" "AA"]
    ["abcdef" ""]])

(each [who api] apis
  (each [text want] lrs-cases
    (cassert-eq (string who " lrs " text) ((get api :lrs) text) want)))

# ==============================================================================
#  CST only: LCP array and lcp(i,j)
# ==============================================================================

(print "\n* LCP array")

# SA: $, a$, ana$, anana$, banana$, na$, nana$
#    6  5    3       1       0       4     2
# lcp: $/a$=0 a$/ana$=1 an$/anana$=3 ana/banana=0 b/n=0 n/na=0 na/nana=2
(cassert-eq "LCP banana$"
            (join (cst/cst-lcp-array (cst/cst "banana$"))) "0 1 3 0 0 2")

(print "\n* lcp(i,j) via RMQ")

(let [t (cst/cst "banana$")]
  # suffixes 6==$ and 5==a$ -> lcp 0
  (cassert-eq "lcp(0,1)" (cst/cst-lcp t 0 1) 0)
  # suffixes at SA[1](a$,start5) and SA[3](anana$,start1): lcp = 1 ("a")
  (cassert-eq "lcp(1,3)" (cst/cst-lcp t 1 3) 1)
  # suffixes at SA[2](ana$,3) and SA[3](anana$,1): lcp = 3 ("ana")
  (cassert-eq "lcp(2,3)" (cst/cst-lcp t 2 3) 3)
  # same position: full length of that suffix (sa[3]=1 -> "anana$", len 6)
  (cassert-eq "lcp(3,3)=len anana$" (cst/cst-lcp t 3 3) 6))

# RMQ blocks. A range lying inside one block, a range straddling two blocks, and
# one spanning many blocks all take different paths.
(defn- direct-min [lcp lo hi]
  (var m (get lcp lo))
  (for i (inc lo) (+ hi 1)
    (def v (get lcp i))
    (when (< v m) (set m v)))
  m)

(defn- repeated-text [half seed]
  (def buf (buffer/new (* 2 half)))
  (var x seed)
  (for _ 0 half
    (set x (mod (+ (* x 75) 74) 65537))
    (buffer/push-byte buf (+ 65 (mod (* x 3) 4))))
  (set x seed)
  (for _ 0 half
    (set x (mod (+ (* x 75) 74) 65537))
    (buffer/push-byte buf (+ 65 (mod (* x 3) 4))))
  (string buf))

(defn- rmq-mismatches [text]
  (def t (cst/cst text))
  (def lcp (cst/cst-lcp-array t))
  (def n (length lcp))
  (var bad 0)
  # widths: inside a block, exactly a block, a block either side
  (each w [1 2 15 31 32 33 63 64 65 96 127 128 129 200 400 800]
    # offsets: block starts, middles and ends
    (each off [0 1 15 16 31 32 33 63 64 65 127 128 200 333]
      (when (and (< off n) (<= (+ off w) n))
        (when (not= (direct-min lcp off (+ off (- w 1)))
                    (cst/cst-lcp t off (+ off w)))
          (++ bad)))))
  bad)

(cassert-eq "cst-lcp = direct min for every in-block, straddling and multi-block range"
            (rmq-mismatches (repeated-text 1600 7)) 0)
(cassert-eq "cst-lcp = direct min on a second corpus"
            (rmq-mismatches (repeated-text 900 4211)) 0)

# ==============================================================================
#  CST only: interval navigation
# ==============================================================================

(print "\n* navigation (LCP intervals)")

(let [t (cst/cst "banana$")]
  # root interval [0,6]: children $, a, b, n
  (def root-kids (cst/cst-children t 0 6))
  (cassert-eq "root children count" (length root-kids) 4)
  (cassert-eq "root child1 $ (0,0)"
              (join [(get (get root-kids 0) :lo) (get (get root-kids 0) :hi)]) "0 0")
  (cassert-eq "root child2 a [1,3] depth1"
              (join [(get (get root-kids 1) :lo) (get (get root-kids 1) :hi)
                     (get (get root-kids 1) :depth)]) "1 3 1")
  (cassert-eq "root child3 b [4,4]"
              (join [(get (get root-kids 2) :lo) (get (get root-kids 2) :hi)]) "4 4")
  (cassert-eq "root child4 n [5,6] depth2"
              (join [(get (get root-kids 3) :lo) (get (get root-kids 3) :hi)
                     (get (get root-kids 3) :depth)]) "5 6 2")

  # children of 'a' [1,3]: a$ leaf and ana [2,3]
  (def a-kids (cst/cst-children t 1 3))
  (cassert-eq "a children count"
              (length a-kids) 2)
  (cassert-eq "a child1 a$ [1,1]"
              (join [(get (get a-kids 0) :lo) (get (get a-kids 0) :hi)]) "1 1")
  (cassert-eq "a child2 ana [2,3] depth3"
              (join [(get (get a-kids 1) :lo) (get (get a-kids 1) :hi)
                     (get (get a-kids 1) :depth)]) "2 3 3")

  # parent relationships
  (def pa (cst/cst-parent t 1 3))
  (cassert-eq "parent of a is root [0,6] depth0"
              (join [(get pa :lo) (get pa :hi) (get pa :depth)]) "0 6 0")

  (def paz (cst/cst-parent t 1 1))
  (cassert-eq "parent of a$ leaf is a [1,3] depth1"
              (join [(get paz :lo) (get paz :hi) (get paz :depth)]) "1 3 1")

  (def pn (cst/cst-parent t 5 6))
  (cassert-eq "parent of n [5,6] is root"
              (join [(get pn :lo) (get pn :hi) (get pn :depth)]) "0 6 0")

  # child follow: root child 'a' (ASCII 97)
  (def ca (cst/cst-child t 0 6 97))
  (cassert-eq "root child 'a'" (join [(get ca :lo) (get ca :hi)]) "1 3"))

(let [t (cst/cst "banana$")]
  (cassert-eq "cst-parent of the root is nil"
              (cst/cst-parent t 0 6) nil)
  (cassert-eq "cst-parent of a non-root node is a struct"
              (struct? (cst/cst-parent t 1 3)) true))

# cst-child indexs text at sa[lo] + parent depth
(let [t (cst/cst "banana$")
      sa (get t :sa)
      txt (get t :text)
      kids (cst/cst-children t 1 3)]
  (cassert-eq "node [1,3] has two children" (length kids) 2)
  (def kid23 (kids 1))
  (def ch (get txt (+ (get sa (get kid23 :lo)) 1)))
  (cassert-eq "child [2,3] edge starts with n" ch (get "banana$" 2))
  (cassert-eq "cst-child finds child [2,3] by its true first char"
              (get (cst/cst-child t 1 3 ch) :lo) (get kid23 :lo))
  (cassert-eq "cst-child does not match the un-offset character"
              (cst/cst-child t 1 3 (get txt (get sa (get kid23 :lo)))) nil))

# cst-child must resolve every child the tree reports, wherever it appears
(var visits 0)
(defn check-child-lookup [t i j parent-depth]
  (var bad 0)
  (cond
    (> parent-depth (length (get t :text))) (++ bad)
    (> visits 400) (++ bad)
    (do
      (++ visits)
      (each kid (cst/cst-children t i j)
        (def lo (get kid :lo))
        (def ch (get (get t :text) (+ (get (get t :sa) lo) parent-depth)))
        (def found (cst/cst-child t i j ch))
        (when (or (nil? found) (not= (get found :lo) lo)) (++ bad))
        (set bad (+ bad (check-child-lookup t lo (get kid :hi) (get kid :depth)))))
      bad)))

(each text ["banana$" "ATAAATG$" "aaaa$" "abacaba$" "mississippi$"]
  (set visits 0)
  (cassert-eq (string "cst-child resolves every child (" text ")")
              (check-child-lookup (cst/cst text) 0 (dec (length text)) 0) 0))

# cst-parent must name the node cst-children was reached from.
(var pvisits 0)
(defn check-parent [t i j]
  (var bad 0)
  (cond
    (> pvisits 400) 1
    (do
      (++ pvisits)
      (each kid (cst/cst-children t i j)
        (def p (cst/cst-parent t (get kid :lo) (get kid :hi)))
        (when (or (nil? p) (not= (get p :lo) i) (not= (get p :hi) j)) (++ bad))
        (set bad (+ bad (check-parent t (get kid :lo) (get kid :hi)))))
      bad)))

(each text ["banana$" "ATAAATG$" "mississippi$" "abacaba$"]
  (set pvisits 0)
  (cassert-eq (string "cst-parent agrees with cst-children (" text ")")
              (check-parent (cst/cst text) 0 (dec (length text))) 0))

(print-results)
