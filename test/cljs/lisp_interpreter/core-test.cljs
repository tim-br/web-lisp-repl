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

(def atom-env (atom {:frames {}
                     :x "string"
                     :#f (list 'primitive false)
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

(deftest extend-environment
  (let [foo (list 'proc '(x) (list '(- x 5)) atom-env)
        my-env atom-env
        _ (swap! my-env assoc-in [:frames :x] 24)
        my-args (list 24)
        _    (l/extend-environment (l/procedure-params foo) my-args (l/proc-env foo))]
    (is (= @my-env @atom-env))
    (is (= 19 (l/my-eval '(- x 5) atom-env)))
    (is (= 24 (l/eval-sequence '(- 5 x) atom-env))))
  (let [fun (list 'procedure '(r q) '((- q (+ r 32))) atom-env)
        my-args (list 3 9)
        params (l/procedure-params fun)
        proc-env (l/proc-env fun)
        body (l/procedure-body fun)
        _  (l/extend-environment params my-args proc-env)]
    (is (= 3 (l/lookup-variable (keyword (first params)) proc-env)))
    (is (= 9 (l/lookup-variable (keyword (first (rest params))) proc-env)))))

(deftest proc-functions
  (is (= '(x) (l/procedure-params (list 'proc '(x) (list '(- x 5)) atom-env))))
  (is (= atom-env (l/proc-env (list 'proc '(x) (list '(- x 5)) atom-env))))
  (is (= (list '(- x 5)) (l/procedure-body (list 'proc '(x) (list '(- x 5)) atom-env)))))

(deftest eval-sub-exps
  (is (= '(3) (l/eval-sub-exps '((+ 1 2)) atom-env)))
  (is (= '(3 9) (l/eval-sub-exps '((+ 2 1)(* 3 3)) atom-env))))

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
  (is (= (list 'primitive +) (l/lookup-variable (first '(+ 1 2)) atom-env)))
  (let [fun (list 'procedure '(x) '((- x 1)) atom-env)
        my-args (list 3)
        params (l/procedure-params fun)
        proc-env (l/proc-env fun)
        body (l/procedure-body fun)
        _  (l/extend-environment params my-args proc-env)]
    (is (= 3 (l/lookup-variable (keyword (first params)) proc-env)))))

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

(deftest lambda?
  (is (l/lambda? '(fn 32 2) 'fn))
  (is (l/lambda? '(fn [x] (+ x 32)) 'fn)))

(deftest basic-eval-test
  (is (= "foo" (l/my-eval "foo" nil)))
  (is (l/variable? (first '(+ 1 2))))
  (is (= false (l/definition? (first '(+ 1 2)))))
  (is (= (list 'primitive +) (l/my-eval (first '(+ 1 2)) atom-env)))
  ;; basic math
  (is (= 5 (l/my-eval '(+ 2 3) atom-env)))
  (is (= 10 (l/my-eval '(+ (+ 3 4) 3) atom-env)))
  (is (= 12 (l/my-eval '(+ (+ 6 4) 2) atom-env)))
  (is (= 12 (l/my-eval '(* 4 (/ 9 3)) atom-env)))
  (is (= 3 (l/my-eval '(/ (* 3 3) (+ 2 1)) atom-env)))
  (is (= 16 (l/my-eval '(+ (* 3 4) (+ 2 2)) atom-env)))
  (is (= 8 (l/my-eval '(* (/ 16 4) 2) atom-env)))
  (is (= 16 (l/my-eval '(- (* 4 5) (* 2 (+ 1 1))) atom-env)))
  (is (= 4 (l/my-eval '(define foo (+ 2 2)) atom-env)))


  ;;; would like to get the following to pass in someway
  ;;; I think its a matter of modifying l/lookup-variable
  #_(is (= "ERROR ---- function/var does not exist" (l/my-eval '(! 2) atom-env))))

(deftest eval-def-test
  (l/eval-definition '(define foo 99) atom-env)
  (is (= (:foo @atom-env) 99)))


;; I don't think I need these tests, may be clutter
#_(deftest primitive-procedure?-test
  (is (l/primitive-procedure? (list 'primitive 43)))
  (is (l/primitive-procedure? '(primitive :foo))))

#_(deftest primitive-implementation-test
  (is (= 5 (l/apply-primitive-proc ('primitive +) '(3 2)))))

(deftest my-apply
  (is (= 3 (l/my-apply (list 'primitive +) (list 1 2))))
  (is (= 25 (l/apply-primitive-proc (list 'primitive +) (list 20 5))))
  (is (= 9 (apply (l/primitive-implementation (list 'primitive +)) (list 5 4))))
  (is (= 9 (l/my-apply (:* @atom-env) (list 3 3))))
  (is (= 9 (l/my-apply (l/my-eval. '* atom-env) (list 3 3))))
  (is (l/compound-proc? '(proc 32)))
  (is (= atom-env (l/proc-env (list 'proc '(x) (list '(- x 5)) atom-env))))
  (is (= 5 (l/my-apply (list 'proc '(x) (list '(/ 15 x)) atom-env) (list 3))))
  (is (= -1 (l/my-apply (list 'proc '(x) (list '(- x 5)) atom-env) (list 4))))
  (is (= 14 (l/my-apply (list 'proc '(x) (list '(* 2 x)) atom-env) (list 7))))
  (is (= 40 (l/my-apply (list 'proc '(y) (list '(* y 4)) atom-env) (list 10))))
  (is (= 5 (l/my-apply (list 'proc '(v s) (list '(- (* v 3) s)) atom-env) (list 3 4))))
  (is (= 7 (l/my-apply (list 'proc '(v s r) (list '(- (* v 3) (/ r s))) atom-env) (list 3 4 8)))))

(deftest variable-test
  (is (l/variable? (first '(+ 1 2)))))

(deftest eval-sequence
  (is (= 9 (l/eval-sequence '((+ 8 1)) atom-env)))
  (is (= 12 (l/eval-sequence '((+ 8 1) (* 3 4)) atom-env)))
  #_(is (= 65 (l/eval-sequence '((l/extend-environment (x) (65) atom-env) x) atom-env)))
  (let [fun (list 'procedure '(x) '((- x 1)) atom-env)
        my-args (list 3)
        params (l/procedure-params fun)
        proc-env (l/proc-env fun)
        body (l/procedure-body fun)]
    #_(is (= 2 (l/eval-sequence body (l/extend-environment params my-args proc-env))))))

