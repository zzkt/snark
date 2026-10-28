# Example


(import graphs/suffix-tree :as st)
(import graphs/cst :as cst)

(def text "banana$")

(printf "suffix array: %s"
        (string/join (map string (st/suffix-array text)) " "))

(printf "occurrences of 'ana': %s"
        (string/join (map string (st/suffix-search (st/suffix-tree text) "ana")) " "))

(printf "CST occurrences of 'ana': %s"
        (string/join (map string (cst/suffix-search (cst/cst text) "ana")) " "))

(printf "root children (CST): %p" (length (cst/cst-children (cst/cst text) 0 6)))
