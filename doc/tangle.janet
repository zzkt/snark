# -*- mode: janet; -*-
# doc/tangle: build doc/examples.janet from src blocks in the docs

(def emacs (or (os/getenv "EMACS") "emacs"))
(def dir "doc/examples")
(def out "doc/examples.janet")

(os/shell (string emacs " --batch -Q"))

(if (and (os/stat dir) (os/stat out))
  (do
    (def n
      (length (filter (fn [f] (string/has-suffix? ".janet" f))
                      (map string (os/dir dir)))))
    (def blocks (length (string/split "\n" (slurp out))))
    (print (string "tangled " n " files (" blocks " lines) into " out))
    n)
  0)
