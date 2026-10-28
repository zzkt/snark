# -*- mode: janet; -*-
# graphs/test/sequence: degree sequence realizability

(import ./helper :prefix "")
(import ../graphs/sequence :prefix "")

# Erdős-Gallai / is-graphical?
(cassert-eq "empty seq graphical" (is-graphical? @[]) true)
(cassert-eq "two nodes one edge" (is-graphical? @[1 1]) true)
(cassert-eq "path of length 3" (is-graphical? @[2 2 1 1]) true) # 1-2-3-4
(cassert-eq "C4" (is-graphical? @[2 2 2 2]) true)
(cassert-eq "K4" (is-graphical? @[3 3 3 3]) true)
(cassert-eq "star K1,3" (is-graphical? @[3 1 1 1]) true)
(cassert-eq "sum odd" (is-graphical? @[2 2 1]) false)
(cassert-eq "impossible max degree" (is-graphical? @[3 1 1]) false)
(cassert-eq "two nodes both degree 2" (is-graphical? @[2 2]) false)
(cassert-eq "negative term" (is-graphical? @[-1]) false)
(cassert-eq "empty seq EG" (is-valid-erdos-gallai? @[]) true)
(cassert-eq "EG same as graphical" (is-valid-erdos-gallai? @[3 2 2 2 1]) true)

# Havel-Hakimi
(cassert-eq "HH equal to EG on star" (is-valid-havel-hakimi? @[3 1 1 1]) true)
(cassert-eq "HH on K4" (is-valid-havel-hakimi? @[3 3 3 3]) true)
(cassert-eq "HH rejects [3,2,1]" (is-valid-havel-hakimi? @[3 2 1]) false)
(cassert-eq "HH empty" (is-valid-havel-hakimi? @[]) true)
(cassert-eq "HH [1,1,0,0]" (is-valid-havel-hakimi? @[1 1 0 0]) true)
(cassert-eq "HH rejects negative" (is-valid-havel-hakimi? @[-1]) false)
(cassert-eq "HH rejects mixed negative" (is-valid-havel-hakimi? @[2 1 -1]) false)

(def hh-src @[3 1 1 1])
(is-valid-havel-hakimi? hh-src)
(cassert-eq "HH keeps input order" (length hh-src) 4)
(cassert-eq "HH input first element intact" (hh-src 0) 3)
(cassert-eq "HH [3,1,1,1] still sorts to star"
            (deep= (sorted (array/slice hh-src)) @[1 1 1 3]) true)
(cassert-eq "HH result equal to EG"
            (is-valid-havel-hakimi? hh-src) (is-valid-erdos-gallai? hh-src))

# Pseudographical / multigraphical
(cassert-eq "pseudo odd sum" (is-pseudographical? @[3 1 1]) false)
(cassert-eq "pseudo 3+1+1+1" (is-pseudographical? @[3 1 1 1]) true)
(cassert-eq "pseudo loop pair" (is-pseudographical? @[2]) true)
(cassert-eq "multi [2,2,2] triangle" (is-multigraphical? @[2 2 2]) true)
(cassert-eq "multi [4,1,1] impossible" (is-multigraphical? @[4 1 1]) false)
(cassert-eq "multi [2,2,2,2] cycle" (is-multigraphical? @[2 2 2 2]) true)
(cassert-eq "multi empty" (is-multigraphical? @[]) true)
(cassert-eq "multi rejects negative" (is-multigraphical? @[-1]) false)
(cassert-eq "multi rejects mixed negative" (is-multigraphical? @[1 -1]) false)

# Directed (Fulkerson-Chen-Anstee)
(cassert-eq "digraphical 3-cycle orientation" (is-digraphical? @[2 1 1] @[1 2 1]) true)
(cassert-eq "digraphical mismatch sum" (is-digraphical? @[2 2] @[2 0]) false)
(cassert-eq "digraphical impossible k" (is-digraphical? @[2 2 0] @[2 0 2]) false)
(cassert-eq "digraphical length mismatch" (is-digraphical? @[1 1] @[1]) false)
(cassert-eq "digraphical empty both" (is-digraphical? @[] @[]) true)
(cassert-eq "digraphical negative" (is-digraphical? @[-1 1] @[0 0]) false)

(print-results)
