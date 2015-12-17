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

#_(def atom-env (atom {:#f false
                     :#t true
                     :car first
                     :cdr rest
                     :cons cons
                     :null? nil?
                     := =
                     :+ (list 'primitive +)
                     :- -
                     :* *
                     :/ / }))

(def atom-env (atom {:#f (list 'primitive false)
                     :#t (list 'primitive false)
                     :car (list 'primitive first)
                     :cdr (list 'primitive rest)
                     :cons (list 'primitive cons)
                     :null? (list 'primitive nil?)
                     := (list 'primitive =)
                     :+ (list 'primitive +)
                     :- (list 'primitive -)
                     :* (list 'primitive *)
                     :/ (list 'primitive /)}))

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
  (swap! atom-env assoc :foo 43)
  (is (= 43 (l/lookup-variable "foo" atom-env)))
  (swap! atom-env dissoc :foo)
  (is (= nil (l/lookup-variable "foo" atom-env)))
  (is (= (list 'primitive +) (l/lookup-variable "+" atom-env)))
  (is (= (list 'primitive +) (l/lookup-variable (first '(+ 1 2)) atom-env))))

;; I wrote this test to verify that derefenced atoms
;; are equivalent to their non-atom equivalent
#_(deftest atom-test
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
  (is (= "foo" (l/my-eval "foo" nil)))
  (is (l/variable? (first '(+ 1 2))))
  (is (= false (l/definition? (first '(+ 1 2)))))
  (is (= (list 'primitive +) (l/my-eval (first '(+ 1 2)) atom-env)))
  (is (= 5 (l/my-eval (+ 2 3) ))))

(deftest eval-def-test
  (l/eval-definition '(define foo 99) atom-env)
  (is (= (:foo @atom-env) 99)))

#_(deftest primitive-procedure?-test
  (is (l/primitive-procedure? (list 'primitive 43)))
  (is (l/primitive-procedure? '(primitive :foo))))

#_(deftest primitive-implementation-test
  (is (= 5 (l/apply-primitive-proc ('primitive +) '(3 2)))))

(deftest my-apply-test
  (is (= 3 (l/my-apply (list 'primitive +) (list 1 2))))
  (is (= 25 (l/apply-primitive-proc (list 'primitive +) (list 20 5))))
  (is (= 9 (apply (l/primitive-implementation (list 'primitive +)) (list 5 4)))))

(deftest variable-test
  (is (l/variable? (first '(+ 1 2)))))
