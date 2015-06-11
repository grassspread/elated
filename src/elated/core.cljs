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
  (flatten
    (for [[origin x] thesaurus]
      (for [[rel words] x]
        (for [word words]
          {:word word :relation rel :origin origin})))))

(def words-api-key "28f194fbde8f31c8629b46cacf5c0223")

(defn request-words [word]
  (go
    (let [url (str "http://words.bighugelabs.com/api/2/" words-api-key "/" word "/json")]
      (-> (http/jsonp url {:callback-name "callback" :timeout 3000})
          <!
          :body))))

(defonce app-state (atom {:data {:input ""}}))

(defn update-thesaurus-words [data word]
  (go
    (let [result (<! (request-words word))]
      (om/update! data :words (flatten-words result)))))

(defn select-thesaurus [data word]
  (om/update! data :input word)
  (update-thesaurus-words data word))

(defn related-word [{:keys [word relation origin parent]} _]
  (reify
    om/IDisplayName
    (display-name [_] "RelatedWord")
    om/IRender
    (render [_]
      (dom/div #js {:onClick (fn [_] (select-thesaurus parent word))
                    :style (css s/word-box)} word))))

(defn thesaurus [{:keys [input words] :as data} owner]
  (reify
    om/IDisplayName
    (display-name [_] "Thesaurus")
    om/IRender
    (render [_]
      (dom/div nil
        (dom/input #js {:onChange   #(om/update! data :input (.. % -target -value))
                        :onKeyPress (fn [e]
                                      (if (= "Enter" (.-key e))
                                        (update-thesaurus-words data input)))
                        :value      input})
        (apply dom/div #js {:style (css s/flex-wrap)} (om/build-all related-word (map #(assoc % :parent data) words)))))))

(defn app-view [data owner]
  (reify
    om/IDisplayName
    (display-name [_] "Elated")
    om/IRender
    (render [_]
      (om/build thesaurus (:data data)))))

(defn init []
  (om/root app-view app-state {:target (.getElementById js/document "app")})
  #_ (let [app (dom/$ "#app")]
    (dom/set-html! app "")
    (dom/append-to! (init-elation) app)))
;// [{word: "currenty", origin: "noun", relation: "syn"}]
(defn on-js-reload [] (init))

(init)
