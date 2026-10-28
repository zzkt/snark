# -*- mode: janet; -*-
# graphs/test/examples: the scripts in examples/ should actually run.

# set SNARK_SKIP_EXAMPLES to False to skip

(import ./helper :prefix "")

(def examples "examples") # assume tests are run from project root
(def scratch (string "/tmp/snark-examples-" (os/time)))
