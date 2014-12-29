(ns ml.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [<! put! chan]]
            [om-bootstrap.grid :as grid]
            [om-bootstrap.panel :as p]
            [ajax.core :refer [GET]]
            [clojure.string :as string]
            [ml.vote-box :refer [vote-box]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(enable-console-print!)

(def language-name-ch (chan))
(def colorized-source-ch (chan))

(defn error-handler [{:keys [status status-text]}]
  (.log js/console (str "something bad happened: " status " " status-text)))

(defn colorize [source language-url]
  (GET language-url
       {:handler #(put! language-name-ch (:name %))
        :error-handler error-handler
        :response-format :json
        :keywords? true})
  (go (let [language-name (<! language-name-ch)]
        (GET "http://localhost:7000/beauty/"
             {:handler #(put! colorized-source-ch (:source %))
              :error-handler error-handler
              :response-format :json
              :keywords? true
              :params {:source source
                       :lang (string/lower-case language-name)}})
        (go (let [colorized-source (<! colorized-source-ch)]
              colorized-source)))))

(defn source-panel [source owner]
  (reify
    om/IRender
    (render [_]
      (p/panel {}
               (om.dom/div #js {:dangerouslySetInnerHTML #js {:__html source}})))))

(defn item-box [item owner]
  (reify
    om/IInitState
    (init-state [_]
      {:colorized-source ""})
    om/IWillMount
    (will-mount [_]
      (colorize (:source item) (:language item))
      (go (while true
            (let [colorized-source (<! colorized-source-ch)]
              (om/set-state! owner :colorized-source colorized-source)))))
    om/IRender
    (render [_]
      (dom/div {}
               (grid/grid {}
                          (grid/row {}
                                    (grid/col {:md 2}
                                              (om/build vote-box item))
                                    (grid/col {:md 10}
                                              (om/build source-panel
                                                        (om/get-state owner
                                                                      :colorized-source))
                                              (dom/div {}
                                                       (:commentary item))))
                          (dom/hr {}))))))

(defn item-list [app owner]
  (reify
    om/IRender
    (render [_]
      (grid/grid {}
                 (map (fn [item] (grid/row {}
                                           (grid/col {}
                                                     (om/build item-box item))))
                      (:items app))))))

(defonce app-state (atom {:items [{:id 1
                                   :source "from butterfiles import Darvon\n\nx = Darvon()"
                                   :tags [{:name "artsy"
                                           :url "http://www.example.com/tags/artsy"},
                                          {:name "pretty"
                                           :url "http://www.example.com/tags/pretty"}]
                                   :commentary "Very pretty and artsy."
                                   :votes (int (rand 10))
                                   :vote 0
                                   :language "http://localhost:8000/api/languages/1/"}
                                  {:id 2
                                   :source "import this  # what is this?"
                                   :tags [{:name "blue"
                                           :url "http://www.example.com/tags/blue"},
                                          {:name "blurry"
                                           :url "http://www.example.com/tags/blurry"}]
                                   :commentary "Blue and blurry."
                                   :votes (int (rand 10))
                                   :vote 0
                                   :language "http://localhost:8000/api/languages/1/"}]}))

(defn main []
  (om/root
   item-list
   app-state
   {:target (. js/document (getElementById "app"))}))
