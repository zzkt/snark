(import graphs :prefix "")

(def g (make-digraph))
(add-nodes g "a" "b" "c" "d" "e" "f")
(add-edges g [["a" "b"] ["b" "a"]
              ["a" "c"] ["c" "a"]
              ["b" "c"] ["c" "b"]
              ["d" "e"] ["e" "d"]
              ["d" "f"] ["f" "d"]
              ["c" "d"] ["d" "c"]])

(def community (louvain g))

(printf "Louvain communities: %s"
        (string/join
         (map |(string/join (sort $) ",")
              (values (group-by (fn [n] (get community n))
                                (keys community)))) " | "))

(def g2 (make-digraph))
(add-nodes g2 ["a" "b" "c" "d" "e" "f"])
(add-edges g2 [["a" "b"] ["b" "a"]
               ["a" "c"] ["c" "a"]
               ["b" "c"] ["c" "b"]
               ["d" "e"] ["e" "d"]
               ["d" "f"] ["f" "d"]
               ["c" "d"] ["d" "c"]])

(def community2 (leiden g2))

(printf "Leiden communities: %s"
        (string/join
         (map |(string/join (sort $) ",")
              (values (group-by (fn [n] (get community2 n))
                                  (keys community2)))) " | "))
