# -*- mode: janet; -*-
# test/doc-examples: examples in the docs should actually work

# set SNARK_SKIP_DOCS to False to skip

(import ./helper :prefix "")

(def scratch (string "/tmp/snark-doc-examples-" (os/time)))

(defn repo-root
  "The project root, from this file's own location."
  []
  (def cur (dyn :current-file))
  (def abs (if (or (= cur "") (not (string/has-prefix? "/" cur)))
             (string (os/cwd) "/" cur)
             cur))
  (def parts (filter |(> (length $0) 0) (string/split "/" abs)))
  (string "/" (string/join (array/slice parts 0 (- (length parts) 2)) "/")))

(defn child-env
  "Env resolves the library from the working tree."
  []
  (def parts @[scratch])
  (def sys (dyn :syspath))
  (when (and sys (not= sys "")) (array/push parts sys))
  (def inherited (os/getenv "JANET_PATH"))
  (when (and inherited (not= inherited ""))
    (array/concat parts (string/split ":" inherited)))
  (string "JANET_PATH=" (string/join parts ":") " "))

(defn run-janet
  "Run `source` in a subprocess. Returns [ok stdout+stderr]."
  [source]
  (def script (string scratch "/run.janet"))
  (def log (string scratch "/run.out"))
  (spit script source)
  (def cmd (string "cd " scratch " && " (child-env)
                   " janet run.janet > " log " 2>&1"))
  (os/shell cmd)
  [true (if (os/stat log) (slurp log) "")])

(os/mkdir scratch)
(os/symlink (string (repo-root) "/graphs") (string scratch "/graphs"))


# Layer 1

(cassert-eq "emacs can tangle the org docs"
            (try (do (os/shell "command -v emacs >/dev/null 2>&1") true) ([_] false))
            true)

# Layer 2

(defn results-lines
  "The recorded output for the src block that ended at index `after`."
  [lines after]
  (def acc @[])
  (var figure false)
  (var seen false)
  (var done false)
  (var i after)
  (while (and (< i (length lines)) (not done))
    (def l (get lines i))
    (if seen
      (cond
        (string/has-prefix? "#+begin_SRC" (string/trim l))
        (do (set figure true) (set done true))
        (string/has-prefix? ": " l)
        (array/push acc (string/slice l 2))
        (= l "") nil                    # blank line inside the output block
        (set done true))                # prose or anything else ends it
      (cond
        (string/has-prefix? "#+results" (string/ascii-lower (string/trim l)))
        (set seen true)
        (= l "") nil                    # blank filler between the blocks
        (set done true)))               # no results belong to this block
    (++ i))
  (if seen (if figure @[] acc)))


(defn org-in [dir]
  (map (fn [f] (string dir "/" f))
       (sort (filter (fn [f]
                       (and (string/has-suffix? ".org" f)
                            (not (string/has-prefix? "." f))))
                     (map string (os/dir dir))))))

(def org-files
  (array/concat @["doc/overview.org"]
                (org-in "doc/topics")
                (org-in "doc/texts")))


(var total-checked 0)
(var total-bad 0)

(each p (if (os/getenv "SNARK_SKIP_DOCS") [] org-files)
  (def lines (string/split "\n" (slurp p)))
  (def stem (last (string/split "/" p)))
  # accumulate janet source and expected output, block by block
  (var code @"")
  (var expect @[])
  (var i 0)
  (var checked 0)
  (var bad 0)
  (while (< i (length lines))
    (def l (get lines i))
    (if (string/has-prefix? "#+begin_src janet"
                            (string/ascii-lower (string/trim l)))
      (do
        (def header (string/trim l))
        (++ i)
        (def body @[])
        (while (and (< i (length lines))
                    (not (string/has-prefix?
                          "#+end_src"
                          (string/ascii-lower (string/trim (get lines i))))))
          (array/push body (get lines i))
          (++ i))
        (def want (results-lines lines (inc i)))
        (unless (string/find "tangle no" header)
          (buffer/push-string code (string/join body "\n") "\n"))
        # tangled block
        (if (and want (not (string/find "tangle no" header)))
          (do
            (array/concat expect want)
            (unless (empty? want)
              (def [ok out] (run-janet code))
              (def got (filter (fn [l] (not= l ""))
                               (string/split "\n" (string/trim out))))
              (if (and ok (deep= got expect))
                (do (++ checked)
                    (print "  ✓ " stem ": " (length expect) " output lines match"))
                (do
                  (++ bad)
                  (print "  ✗ " stem ": documented output does not match")
                  (print "      expected: " (string/join expect " | "))
                  (print "      got     : " (string/join got " | "))))))))
      (++ i)))
  (set total-checked (+ total-checked checked))
  (set total-bad (+ total-bad bad))
  (when (> checked 0)
    (cassert-eq (string stem " recorded output verified") (> checked 0) true)))


(cassert-eq (string "documented output matches the code, in " total-checked
                    " comparisons across " (length org-files) " files")
            total-bad 0)

(print-results)
