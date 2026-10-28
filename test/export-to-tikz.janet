# -*- mode: janet; -*-
# graphs/test/export: to-tikz

(import ./helper :prefix "")
(import ../graphs :prefix "")

(defn- has?
  [hay needle]
  (not= nil (string/find needle hay)))

(defn- misses?
  [hay needle]
  (nil? (string/find needle hay)))

(print "\n* export: to-tikz")

(def dg (make-digraph))
(digraph-add-edge dg "a" "b" 1)
(digraph-add-edge dg "b" "c" 2)

(def wg (make-weighted-graph @[] @{}))
(weighted-graph-add-edge wg "a" "b" 1.5)

(def tz (to-tikz dg))
(cassert-eq "tikzpicture open"
            (string/find "\\begin{tikzpicture}" tz) 0)
(cassert-eq "tikzpicture close"
            (has? tz "\\end{tikzpicture}") true)
(cassert-eq "graph command" (has? tz "\\graph ") true)
(cassert-eq "grow right default"
            (has? tz "grow right sep=1.5cm") true)
(cassert-eq "directed chain first edge" (has? tz "a ->[\"1\"") true)
(cassert-eq "directed chain second edge"
            (has? tz "] b ->[\"2\"") true)
(cassert-eq "directed chain ends at c" (has? tz "] c;") true)
(cassert-eq "node style applied"
            (has? tz "every node/.style={circle") true)
(cassert-eq "node style fits a circle"
            (has? tz "minimum size=2.0em") true)
(cassert-eq ">=latex in env" (has? tz ">=latex") true)
(cassert-eq "graph edges semithick"
            (has? tz "edges={semithick}") true)

(cassert-eq ":direction TB"
            (has? (to-tikz dg :direction "TB") "grow down sep=1.5cm") true)
(cassert-eq ":direction BT"
            (has? (to-tikz dg :direction "BT") "grow up sep=1.5cm") true)
(cassert-eq ":direction RL"
            (has? (to-tikz dg :direction "RL") "grow left sep=1.5cm") true)
(cassert-eq ":sep"
            (has? (to-tikz dg :sep "4cm") "grow right sep=4cm") true)
(cassert-eq ":graph-opts"
            (has? (to-tikz dg :graph-opts "branch down")
                  "grow right sep=1.5cm, branch down") true)
(cassert-eq ":node-style"
            (has? (to-tikz dg :node-style "draw")
                  "every node/.style={draw}") true)
(cassert-eq ":tikz-opts"
            (has? (to-tikz dg :tikz-opts "scale=0.8") "scale=0.8") true)
(cassert-eq ":label-edges off"
            (misses? (to-tikz dg :label-edges false) "\"1\"") true)
(cassert-eq ":edge-style"
            (has? (to-tikz dg :edge-style "red") "->[red,") true)

(def tz-ug (to-tikz wg))
(cassert-eq "undirected uses --" (has? tz-ug "a --") true)
(cassert-eq "undirected has no ->" (misses? tz-ug "->") true)

(def br (make-digraph))
(digraph-add-edge br "a" "b" 1)
(digraph-add-edge br "a" "c" 1)
(def tz-br (to-tikz br))
(cassert-eq "branching graph keeps both chains"
            (length (string/find-all "->[" tz-br)) 2)

(def tz-iso (to-tikz (make-weighted-graph @["solo"] @{})))
(cassert-eq "isolated node emitted" (has? tz-iso "solo;") true)

(def tz-lab (to-tikz (make-digraph @["X-Men" "y2"] @{})))
(cassert-eq "sanitised name"
            (has? tz-lab "xmen[as={X-Men}]") true)
(def tz-esc (to-tikz (make-digraph @["a_b" "c"] @{})))
(cassert-eq "underscore escaped" (has? tz-esc "as={a\\_b}") true)

# explicit layout
(def tz-tala (to-tikz dg :layout "tala"))
(cassert-eq "tala uses \\node" (has? tz-tala "\\node ") true)
(cassert-eq "tala uses \\draw" (has? tz-tala "\\draw") true)
(cassert-eq "tala has no \\graph" (misses? tz-tala "\\graph ") true)
(cassert-eq "tala edge semithick"
            (has? tz-tala "\\draw[->, semithick]") true)
(cassert-eq "tala label neutralised"
            (has? tz-tala "fill=none, draw=none") true)

(def tz-tala-ug (to-tikz wg :layout "tala"))
(cassert-eq "tala undirected never emits \\draw[--]"
            (misses? tz-tala-ug "\\draw[--]") true)
(cassert-eq "tala undirected draws plainly"
            (has? tz-tala-ug "\\draw[semithick]") true)

(def tz-sym (to-tikz (make-digraph @["!!!" "y"] @{})))
(cassert-eq "symbol label gets a valid node name"
            (has? tz-sym "n0[as={!!!}]") true)

(def tz-orth (to-tikz dg :layout "orthogonal" :label-edges false))
(cassert-eq "orthogonal uses the -| connector"
            (has? tz-orth " -| ") true)
(cassert-eq "orthogonal edge semithick"
            (has? tz-orth "\\draw[->, semithick] ") true)

(def tz-orth-lab (to-tikz dg :layout "orthogonal"))
(cassert-eq "orthogonal label sits above near the start"
            (has? tz-orth-lab "node[near start,above") true)
(cassert-eq "orthogonal never puts the label right"
            (misses? tz-orth-lab "near start,right") true)
(cassert-eq "orthogonal TB still above"
            (has? (to-tikz dg :layout "orthogonal" :direction "TB")
                  "node[near start,above") true)

(def tz-empty (to-tikz (make-digraph)))
(cassert-eq "empty graph picture"
            (has? tz-empty "\\begin{tikzpicture}") true)

(defn- tikz-ids
  [out]
  (def ids @[])
  (each line (string/split "\n" out)
    (def t (string/trim line))
    (when (string/find "[" t)
      (array/push ids (string/slice t 0 (string/find "[" t)))))
  ids)

(def collide (make-digraph))
(add-nodes collide "a&b" "axxb" "x<y" "xy")
(add-edge collide "a&b" "x<y" 1)

(def tk-ids (tikz-ids (to-tikz collide)))
(cassert-eq "colliding sanitised names stay unique"
            (length (keys (do (def u @{})
                              (each i tk-ids (put u i true))
                              u)))
            (length tk-ids))

# multigraphs
(def tk-mg (make-multigraph @{}))
(add-nodes tk-mg "a" "b")
(add-edge tk-mg "a" "b")

(cassert-eq "multigraph with one edge exports"
            (= (type (to-tikz tk-mg)) :string) true)
(cassert-eq "multigraph is written undirected"
            (has? (to-tikz tk-mg) "a --") true)

(def tk-par (make-multigraph @{}))
(add-nodes tk-par "a" "b")
(add-edges tk-par ["a" "b"] ["a" "b"] ["a" "b"])

(cassert-eq "parallel edges do not crash the chain walk"
            (= (type (to-tikz tk-par)) :string) true)
(cassert-eq "parallel edges keep both endpoints"
            (has? (to-tikz tk-par) "b;") true)

(def tk-md (make-multidigraph @{} @{}))
(add-nodes tk-md "a" "b")
(add-edge tk-md "a" "b")

(cassert-eq "multidigraph exports"
            (= (type (to-tikz tk-md)) :string) true)
(cassert-eq "multidigraph is written directed"
            (has? (to-tikz tk-md) "a ->") true)

(def mg3 (make-multidigraph @{} @{}))
(add-nodes mg3 "a" "b" "c")
(add-edges mg3 ["a" "b"] ["a" "b"] ["b" "c"])

(cassert-eq "parallel edges collapse into one step"
            (length (string/find-all "->[" (to-tikz mg3))) 2)

(print-results)
