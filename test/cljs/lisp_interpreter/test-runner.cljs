(ns lisp_interpreter.test-runner
  (:require
   [cljs.test :refer-macros [run-tests]]
   [lisp_interpreter.core-test]))

(enable-console-print!)

(defn runner []
  (if (cljs.test/successful?
       (run-tests
        'lisp_interpreter.core-test))
    0
    1))
