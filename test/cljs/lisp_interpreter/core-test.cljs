(ns lisp_interpreter.core-test
  (:require-macros [cljs.test :refer (is deftest testing)])
  (:require [cljs.test]
            [lisp-interpreter.core :as l]))

(def global-env {:#f false
                 :#t true
                 :car first
                 :cdr rest
                 :cons cons
                 :null? nil?
                 := =
                 :+ +
                 :- -
                 :* *
                 :/ / })

(def atom-env (atom {:#f false
                     :#t true
                     :car first
                     :cdr rest
                     :cons cons
                     :null? nil?
                     := =
                     :+ +
                     :- -
                     :* *
                     :/ / }))

(deftest example-passing-test
  (is (= :foo (l/thing))))

(deftest env-test
  (is (== 1 ((:car global-env) [1 2 3]))))

(deftest self-eval-test
  (is (= true (l/self-evaluating? "bar")))
  (is (= true (l/self-evaluating? 423)))
  (is (= false (l/self-evaluating? :ftw))))

(deftest variable?-test
  (is (= true (l/variable? 'bar))))

(deftest lookup-test
  (let [cur-env (assoc global-env :foo 43)]
    (is (= 43 (l/lookup-variable 'foo cur-env)))))

(deftest atom-test
  (is (= true (= @atom-env global-env))))

(deftest tagged-test
    (is (l/tagged-list? ['define 32] 'define))
    (is (l/tagged-list? (list 'define 43) 'define))
    (is (l/tagged-list? '(define 43) 'define)))

(deftest definition-fn-test
  "tests that the definition function works"
  (is (l/definition? (list 'define 'foo 32)))
  (is (l/definition? '(define foo 32))))

(deftest basic-eval-test
  (is (= "foo" (l/my-eval "foo" nil))))
