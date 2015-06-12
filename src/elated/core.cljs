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
      (for [[origin x] thesaurus]
        (for [[rel words] x]
          (for [word words]
            {:word word :relation rel :origin origin}))))))

(defn loading [options]
  (dom/div (css {:className "preloader-wrapper big active"} options)
    (dom/div #js {:className "spinner-layer spinner-blue"}
      (dom/div #js {:className "circle-clipper left"}
        (dom/div #js {:className "circle"}))
      (dom/div #js {:className "gap-patch"}
        (dom/div #js {:className "circle"}))
      (dom/div #js {:className "circle-clipper right"}
        (dom/div #js {:className "circle"})))
    (dom/div #js {:className "spinner-layer spinner-red"}
      (dom/div #js {:className "circle-clipper left"}
        (dom/div #js {:className "circle"}))
      (dom/div #js {:className "gap-patch"}
        (dom/div #js {:className "circle"}))
      (dom/div #js {:className "circle-clipper right"}
        (dom/div #js {:className "circle"})))
    (dom/div #js {:className "spinner-layer spinner-yellow"}
      (dom/div #js {:className "circle-clipper left"}
        (dom/div #js {:className "circle"}))
      (dom/div #js {:className "gap-patch"}
        (dom/div #js {:className "circle"}))
      (dom/div #js {:className "circle-clipper right"}
        (dom/div #js {:className "circle"})))
    (dom/div #js {:className "spinner-layer spinner-green"}
      (dom/div #js {:className "circle-clipper left"}
        (dom/div #js {:className "circle"}))
      (dom/div #js {:className "gap-patch"}
        (dom/div #js {:className "circle"}))
      (dom/div #js {:className "circle-clipper right"}
        (dom/div #js {:className "circle"})))))

(def words-api-key "28f194fbde8f31c8629b46cacf5c0223")

(def materialize-colors
  ["red" "pink" "purple" "deep-purple" "indigo" "blue" "light-blue" "cyan" "teal"
   "green" "light-green" "lime" "yellow" "amber" "orange" "deep-orange"
   "brown" "grey" "blue-grey"])

(def color-variations
  ["lighten-4 black-text" "lighten-3 black-text" "lighten-2 black-text" "lighten-1 black-text"
   ""
   "darken-1" "darken-2" "darken-3" "darken-4"])

(def color-range
  (shuffle
    (for [color materialize-colors
         v     color-variations]
     (str color " " v))))

(def relations #{:syn :ant :sim :usr})
(def origins #{:adjective :noun :verb :adverb})

(def colorset
  (let [base  (for [r relations
                    o origins]
                #{r o})
        total (count base)]
    (into {}
          (map (fn [c i]
                    (let [pct (/ i total)]
                      [c (nth color-range (.floor js/Math (* pct (count color-range))))]))
                  base (range)))))

(defn word-color [{:keys [relation origin]}]
  (colorset #{relation origin}))

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

(defn fa-icon [name options] (dom/i (css {:className (str "fa fa-" name)} options)))

(defn radio [state options]
  (icon (str "toggle-radio-button-" (if state "on" "off")) options))

(defn related-word [{{:keys [word pinned?] :as related} :word :keys [on-select on-pin] :or {on-select identity
                                                                                            on-pin    identity}} _]
  (reify
    om/IDisplayName
    (display-name [_] "RelatedWord")

    om/IRender
    (render [_]
      (dom/div #js {:className (str "waves-effect waves-light btn " (word-color related))
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
          (if loading?
            (loading {:style {:margin-top 30}})
            (apply dom/div #js {:style (css s/flex-wrap {:margin-top 20})}
                   (om/build-all related-word (map
                                                (fn [w]
                                                  {:word      w
                                                   :on-select on-select
                                                   :on-pin    #(pin-word data %)})
                                                words)))))))))

(defn app-view [data _]
  (reify
    om/IDisplayName
    (display-name [_] "Elated")
    om/IRender
    (render [_]
      (dom/div #js {:style (css s/text-center)}
        (fa-icon "lightbulb-o" {:style (css {:font-size 210
                                             :margin-bottom 30})})
        (om/build thesaurus (:data data))))))

(defn init []
  (om/root app-view app-state {:target (.getElementById js/document "app")}))

(defn on-js-reload [] (init))

(init)
