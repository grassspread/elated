(ns elated.styles)

(defn css [& styles] (clj->js (apply merge styles)))

(def word-box
  {:border  "1px solid #000"
   :margin  "5px"
   :padding "5px 8px"
   :cursor "pointer"
   :display "flex"
   :align-items "center"})

(def flex-wrap
  {:display "flex"
   :flex-wrap "wrap"})

(def text-center {:text-align "center"})

(def main-input
  {:text-align "center"
   :font-size "2.5rem"})
