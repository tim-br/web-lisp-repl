(ns lisp-interpreter.core
  (:require-macros [cljs.core.async.macros :refer [go]])

  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.reader :as reader]
            [cljs.core.async :refer [put! chan <!]]))

(enable-console-print!)

(defonce app-state (atom {:text "\" give me some in\""
                          :result "nil"}))

(defonce global-env (atom {:#f (list 'primitive false)
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

(defn lookup?
  [exp]
  (tagged-list? exp 'lookup))

(defn eval-definition
  [exp env]
  (js/console.log "what is the func? " exp)
  (js/console.log "is seqable? " (seq? exp))
  (swap! env assoc (keyword (first (rest exp))) (last exp))
  (js/console.log "do I get here?"))

(defn lookup-variable
  ;[var]
  ;(lookup-variable var global-env)
  [var env]
  (js/console.log "the var is " var)
  (let [new-var (keyword var)]
    (new-var @env)))

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

(defn my-apply
  [proc args]
  (cond (primitive-proc? proc) (apply-primitive-proc proc args)
        :else "unknown procedure type"))

(defn application? [exp]
  (list? exp))

(defn operator [exp]
  (first exp))

(defn operands [exp]
  (rest exp))

(defn my-eval
  [exp env]
  (js/console.log "the exp is " exp)
  (cond (self-evaluating? exp) exp
        (definition? exp)  (do (eval-definition exp global-env)
                                 ((keyword (first (rest exp))) @global-env))
        (variable? exp) (lookup-variable exp global-env)
        (lookup? exp) (eval-lookup exp global-env)
        (application? exp) (my-apply (my-eval (operator exp) global-env)
                                     (operands exp))
        :else "ERROR ---- unknown exp"))

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
                        ;user-input (.-value (om/get-node owner "ta"))
                        ]
                    (js/console.log "the thingy I'm checking is: " (:text @app-state))
                    (js/console.log "user input : " user-input)
                    (js/console.log "is a string? I'm checking " (string? (:text @app-state)))
                    (put! output-chan (my-eval user-input global-env))
                    )
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
