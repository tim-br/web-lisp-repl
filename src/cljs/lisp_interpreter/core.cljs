(ns lisp-interpreter.core
  (:require-macros [cljs.core.async.macros :refer [go]])

  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.reader :as reader]
            [cljs.core.async :refer [put! chan <!]]
            [lisp-interpreter.interpreter :as l]))

(enable-console-print!)

(defn handle-change [param]
  (js/console.log "is string in handle-change? " (string? param)) ;; true
  (swap! l/app-state assoc :text param))

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
