(ns lisp-interpreter.core
  (:require-macros [cljs.core.async.macros :refer [go]])

  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.reader :as reader]
            [cljs.core.async :refer [put! chan <!]]))

(enable-console-print!)

(declare my-eval)

(defonce app-state (atom {:text "\" give me some in\""
                          :result "nil"}))

(defonce global-env (atom {:frames {}
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

(defn tagged-list?
  [exp tag]
  (= (first exp) tag))

(defn definition?
  [exp]
  (if (list? exp)
    (tagged-list? exp 'define)
    false))

(defn lambda?
  [exp]
  (tagged-list? exp 'fn))

(defn make-proc
  [params body env]
  (list 'proc params (list body) env))

(defn compound-proc?
  [p]
  (tagged-list? p 'proc))

(defn procedure-params
  [proc]
  (first (rest proc)))

(defn procedure-body
  [proc]
  (first (rest (rest proc))))

(defn proc-env
  [proc]
  (-> proc
      last))

(defn lookup?
  [exp]
  (tagged-list? exp 'lookup))

(defn eval-definition
  [exp env]
  (swap! env assoc (keyword (first (rest exp))) (my-eval (last exp) env)))

(defn lookup-variable
  ;[var]
  ;(lookup-variable var global-env)
  [var env]
  (js/console.log "the var is " var)
  (let [new-var (keyword var)]
    (or (new-var (:frames @env))
        (new-var @env))))

(defn eval-lookup
  [exp env]
  (let [lookup-var (keyword (last exp))]
    (lookup-variable lookup-var env)))

(defn self-evaluating?
  [exp]
  (cond (number? exp) true
        (string? exp) true
        :else false))

(defn variable?
  [exp]
  (symbol? exp))

(defn primitive-implementation
  [proc]
  (first (rest proc)))

(defn primitive-proc?
  [proc]
  (tagged-list? proc 'primitive))

(defn apply-primitive-proc
  [proc args]
  (apply (primitive-implementation proc) args))

(defn extend-environment
  [variable value env]
  ;(js/console.log "the env is " @env)
  (cond (or (empty? (rest variable)) (empty? (rest value)))
          (swap! env assoc-in [:frames (keyword (first variable))] (first value))
        :else (do (swap! env assoc-in [:frames (keyword (first variable))] (first value))
                  (extend-environment (rest variable) (rest value) env))))

(defn eval-sequence
  [exps env]
  (cond (empty? (rest exps)) (my-eval (first exps) env)
        :else (do (my-eval (first exps) env)
                  (eval-sequence (rest exps) env))))

(defn my-apply
  [proc args]
  ;(js/console.log "the proc is " proc)
  ;(js/console.log "the args are " args)
  (cond (primitive-proc? proc) (apply-primitive-proc proc args)
        (compound-proc? proc)  (do (extend-environment (procedure-params proc)
                                                       args
                                                       (proc-env proc))
                                 #_(my-eval (procedure-body proc) (proc-env proc))
                                  (eval-sequence
                                      (procedure-body proc)
                                      (proc-env proc)))
        :else "ERROR: unknown procedure type"))

(defn application? [exp]
  (list? exp))

(defn operator [exp]
  (js/console.log "the first of exp is" (first exp))
  (first exp))

(defn operands [exp]
  (rest exp))

(defn lambda-params
  [exp]
  (first (rest exp)))

(defn lambda-body
  [exp]
  (first (rest (rest exp))))

(defn eval-sub-exps
  [exps env]
  (map #(my-eval % env) exps))

(defn my-eval
  [exp env]
  #_(js/console.log "the exp is " exp)
  (cond (self-evaluating? exp) exp
        (definition? exp)  (do (eval-definition exp env)
                               :ok #_((keyword (first (rest exp))) @env))
        (variable? exp) (lookup-variable exp env)
        (lookup? exp) (eval-lookup exp env)
        (lambda? exp) (do
                        (js/console.log "hello ")
                        (make-proc (lambda-params exp) (lambda-body exp) env))
        (application? exp) (my-apply (my-eval (operator exp) env)
                                     (eval-sub-exps (operands exp) env))
        :else "ERROR ---- unknown exp"))

;; (defn list-of-values
;;   [exps env]
;;   (loop [expressions exps environment env]
;;     (if (nil? expressions)
;;       nil
;;       (cons (my-eval (first expressions) environment)
;;             (recur (rest expressions) environment)))))

#_(defn list-of-values
  [exps env]
  (if (empty? exps)
    nil
    (cons (my-eval (first exps) env)
          (list-of-values (rest exps) env))))

(defn do-s []
  (js/console.log "yo"))

(defn thing[]
  :foo)

(defn handle-change [param]
  (js/console.log "is string in handle-change? " (string? param)) ;; true
  (swap! app-state assoc :text param))

#_(defn is-first-char-quote? [str]
  (let [ch (js/charAt str 0)]
    (= ch "")))

(defn button [text owner]
  (reify
    om/IRenderState
      (render-state [this {:keys [reade]}]
        (dom/button #js {:onKeyDown #(when (= (.-key %) "Enter")
                                    (put! reade :anything))
                         :onClick #(put! reade :anything)}
                    (:text text)))))

(defn result-com [res owner]
  (reify
    om/IWillMount
        (will-mount [_]
          (let [output-chan (om/get-state owner :output-chan)]
            (go (loop []
              (let [output (<! output-chan)]
                ;(js/console.log (str "the output is " output))
                (om/set-state! owner :res output)
                (recur))))))
    om/IInitState
        (init-state [_]
          {:res "nil"})
    om/IRenderState
      (render-state [this _]
        (dom/div nil (str "the result is --- " (om/get-state owner :res))))))

(defn main []
  (om/root
    (fn [app owner]
      (reify
        om/IInitState
        (init-state [_]
          {:reade (chan)
           :output-chan (chan)})
        om/IWillMount
        (will-mount [_]
          (let [reade (om/get-state owner :reade)
                output-chan (om/get-state owner :output-chan)]
            (go (loop []
              (let [foo (<! reade)]
                (when foo
                  (let [user-input  (reader/read-string (:text @app-state) #_(.-value (om/get-node owner "ta")))
                        output (my-eval user-input global-env)]
                    (if (compound-proc? output)
                      (put! output-chan (list 'compound-procedure
                                           (procedure-params output)
                                           (procedure-body output)
                                           '<procedure-env>))
                      (put! output-chan (my-eval user-input global-env))))
                  (set! (.-value (om/get-node owner "ta")) ""))
                (recur))))))
        om/IRenderState
        (render-state [this {:keys [reade output-chan]}]
                (dom/div nil
                         (dom/textarea
                          #js {:ref "ta"
                               :value (:text app)
                               :onChange #(handle-change (.. % -target -value))}
                          nil)
                         (om/build result-com nil
                            {:init-state {:output-chan output-chan}})
                         (om/build button {:text "YOLO"}
                            {:init-state {:reade reade}})))))
    app-state
    {:target (. js/document (getElementById "app"))}))
