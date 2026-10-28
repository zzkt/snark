# -*- mode: janet; -*-
# graph/test/helper: asserts and shared

(import deft :prefix "")

(var pass-count 0)
(var fail-count 0)

(defn cassert-eq [name got expected]
  (if (= got expected)
    (do (++ pass-count) (print "  ✓ " name))
    (do (++ fail-count)
      (print "  ✗ " name
             ": expected " (string expected)
             ", got " (string got)))))

(defmacro cassert-err [name body]
  ~(cassert-eq ,name
              (not= nil
                (try (do ,body nil) ([e] e)))
              true))

(defn arr-eq [a b]
  (and (= (length a) (length b))
       (all (fn [i] (= (get a i) (get b i))) (range (length a)))))

(defn cassert-arr-eq [name got expected]
  (if (arr-eq got expected)
    (do (++ pass-count) (print "  ✓ " name))
    (do (++ fail-count)
      (print "  ✗ " name
             ": got " (string got)
             " expected " (string expected)))))


(defn arr-contains [arr val]
  (var found false)
  (each v arr (when (= v val) (set found true)))
  found)

(defn set-eq
  "True when a and b hold the same elements, in any order."
  [a b]
  (and (= (length a) (length b))
       (all (fn [x] (arr-contains b x)) a)))

(defn cassert-set-eq [name got expected]
  (if (set-eq got expected)
    (do (++ pass-count) (print "  ✓ " name))
    (do (++ fail-count)
      (print "  ✗ " name
             ": got " (string got)
             " expected " (string expected)))))


(defn print-results []
  (print "\n--- " pass-count " passed, " fail-count " failed ---")
  (if (> fail-count 0) (os/exit 1)))
