# Example


(import graphs :prefix "")

(def t (make-digraph))
(add-nodes t ["a" "b" "c"])
(add-edges t [["a" "b" 1] ["a" "c" 1] ["b" "c" 1]])

(printf "tournament? %p" (is-tournament? t))

(printf "score sequence: %s" (string/join (map string (tournament-score-sequence t)) " "))

(printf "valid? %p" (valid-tournament-score-sequence? (tournament-score-sequence t)))

(printf "kings: %s" (string/join (tournament-kings t) " "))

(printf "transitive? %p  regular? %p" (transitive-tournament? t) (regular-tournament? t))



# #+RESULTS:
# : tournament? true
# : score sequence: 0 1 2
# : valid? true
# : kings: a
# : transitive? true  regular? false

# A 3-cycle is regular and strongly connected yet still has a Hamiltonian path


(def c3 (make-digraph))
(add-nodes c3 ["a" "b" "c"])
(add-edges c3 [["a" "b" 1] ["b" "c" 1] ["c" "a" 1]])

(printf "scores: %p" (tournament-score-sequence c3))

(printf "regular? %p\nstrong? %p"
        (regular-tournament? c3) (strong-tournament? c3))

(printf "Hamiltonian path length: %p"
        (length (tournament-hamiltonian-path c3)))
