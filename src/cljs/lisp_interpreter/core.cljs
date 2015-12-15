(ns lisp-interpreter.core
  (:require-macros [cljs.core.async.macros :refer [go]])

  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.reader :as reader]
            [cljs.core.async :refer [put! chan <!]]))

(enable-console-print!)

(defonce app-state (atom {:text "\" give me some in\""
                          :result "nil"}))

(defonce global-env (atom {:#f false
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


(defn tagged-list?
  [exp tag]
       (= (first exp) tag))

(defn definition?
  [exp]
  (tagged-list? exp 'define))

(defn eval-definition
  [exp env]
  (swap! env assoc (keyword (first (rest env))) (last env)))

(defn self-evaluating?
  [exp]
  (cond (number? exp) true
        (string? exp) true
        :else false))

(defn variable?
  [exp]
  (symbol? exp))

(defn lookup-variable
  [var env]
  (let [new-var (keyword var)]
    (new-var env)))

(defn my-eval
  [exp env]
  (js/console.log "the exp is " (keyword exp))
  (cond (self-evaluating? exp) exp
        (definition? exp) (eval-definition exp global-env)
        :else "ERROR ---- unknown exp"))

(defn do-s []
  (js/console.log "yo"))

(defn thing[]
  :foo)

(defn handle-change [param]
  (let [str-param (str param)]
    (js/console.log "is string in handle-change? " (string? str-param)) ;; true
    (swap! app-state assoc :text str-param)))

#_(defn is-first-char-quote? [str]
  (let [ch (js/charAt str 0)]
    (= ch "")))

(defn button [text owner]
  (reify
    om/IRenderState
      (render-state [this {:keys [reade]}]
        (dom/button #js {:onKeyDown #(when (= (.-key %) "Enter")
                                    (put! reade :anything))}
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
                    ;(js/console.log "the input is: " (last user-input))
                    (js/console.log "the thingy I'm checking is: " (:text @app-state))
                    (js/console.log "user input : " user-input)
                    (js/console.log "is a string? I'm checking " (string? (:text @app-state)))
                    ;(put! output-chan (my-eval user-input global-env))
                    ;(js/console.log "user input is: " user-input)
                    ;(js/console.log "is string? " (string? user-input))
                    ;(js/console.log "this is a string ? " (string? (str user-input)))
                    ;(js/console.log "are equal? " (= (str user-input) user-input))
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
