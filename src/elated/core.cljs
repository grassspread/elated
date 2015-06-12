(ns ^:figwheel-always elated.core
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [wilkerdev.util.macros :refer [dochan]])
  (:require [wilkerdev.util.reactive :as r]
            [cljs-http.client :as http]
            [cljs.core.async :as async :refer [chan <! >! put! close!]]
            [elated.styles :as s :refer [css]]
            [om.core :as om]
            [om.dom :as dom]))

(enable-console-print!)

(defn flatten-words [thesaurus]
  "This is the documentation"
  (set
    (flatten
      (for [[_ x] thesaurus]
        (for [[rel words] x]
          (for [word words]
            {:word word :relation rel}))))))

(def words-api-key "28f194fbde8f31c8629b46cacf5c0223")

(def relation-tone
  {:syn "light-green"
   :ant "amber"})

(defn request-words [word]
  (go
    (let [url (str "http://words.bighugelabs.com/api/2/" words-api-key "/" word "/json")]
      (-> (http/jsonp url {:callback-name "callback" :timeout 3000})
          <!
          :body))))

(defonce app-state (atom {:data {:input "" :pinned [] :loading? false}}))

(defn update-thesaurus-words [data word]
  (go
    (om/update! data :loading? true)
    (let [result (<! (request-words word))]
      (om/update! data :words (flatten-words result))
      (om/update! data :loading? false))))

(defn select-thesaurus [data {:keys [word]}]
  (om/update! data :input word)
  (update-thesaurus-words data word))

(defn icon [name options] (dom/i (css {:className (str "mdi-" name)} options)))

(defn radio [state options]
  (icon (str "toggle-radio-button-" (if state "on" "off")) options))

(defn related-word [{{:keys [word pinned? relation] :as related} :word :keys [on-select on-pin] :or {on-select identity
                                                                                                     on-pin    identity}} _]
  (reify
    om/IDisplayName
    (display-name [_] "RelatedWord")

    om/IRender
    (render [_]
      (dom/div #js {:className (str "waves-effect waves-light btn " (relation-tone relation))
                    :onClick   (fn [_] (on-select related))
                    :style     (css s/word-box)}
        (radio pinned? {:onClick (fn [e]
                                   (.stopPropagation e)
                                   (on-pin related)
                                   (println "pinned"))
                        :style   (css {:margin-right 5})})
        word))))

(defn cursor-link [cursor k]
  {:value    (k cursor)
   :onChange #(om/update! cursor k (.. % -target -value))})

(defn distinctv [l] (vec (distinct l)))

(defn pin-word [data word]
  (om/transact! data :pinned #(distinctv (conj % (assoc word :pinned? true)))))

(defn unpin-word [data word]
  (om/transact! data :pinned #(filterv (partial not= word) %)))

(defn thesaurus [{:keys [input words pinned loading?] :as data} _]
  (reify
    om/IDisplayName
    (display-name [_] "Thesaurus")

    om/IRender
    (render [_]
      (let [on-select (partial select-thesaurus data)]
        (dom/div #js {:style (css s/text-center)}
          (dom/div #js {:className "input-field"}
            (dom/input (css (cursor-link data :input)
                            {:type        "text"
                             :placeholder "Start with a word"
                             :onKeyPress  (fn [e]
                                            (if (= "Enter" (.-key e))
                                              (update-thesaurus-words data input)))
                             :style       (css s/main-input)})))
          (apply dom/div #js {:style (css s/flex-wrap)}
                 (om/build-all related-word (map
                                              (fn [w]
                                                {:word      w
                                                 :on-select on-select
                                                 :on-pin    #(unpin-word data %)})
                                              pinned)))
          (apply dom/div #js {:style (css s/flex-wrap {:margin-top 20})}
                 (om/build-all related-word (map
                                              (fn [w]
                                                {:word      w
                                                 :on-select on-select
                                                 :on-pin    #(pin-word data %)})
                                              words)))
          (if loading? (dom/div nil "Loading...")))))))

(defn app-view [data _]
  (reify
    om/IDisplayName
    (display-name [_] "Elated")
    om/IRender
    (render [_]
      (om/build thesaurus (:data data)))))

(defn init []
  (om/root app-view app-state {:target (.getElementById js/document "app")}))

(defn on-js-reload [] (init))

(init)
