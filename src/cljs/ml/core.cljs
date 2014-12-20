(ns ml.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [om-bootstrap.button :as b]
            [om-bootstrap.random :as r]
            [om-bootstrap.grid :as g]))

(defn tag-link [cursor _]
  (om/component
   (let [tag-name (:name cursor)]
     (dom/a #js {:href (:url cursor)} (:name cursor)))))

(defn tag-list [cursor _]
  (om/component
   (apply dom/ul nil
          (map (fn [data] (dom/li nil (om/build tag-link data))) (:tags cursor)))))

(defn missing-link [cursor _]
  (reify
    om/IRender
    (render [this]
      (let [img (:img cursor)
            {:keys [src alt-text width height]} img]
        (dom/div nil
                 (dom/a #js {:href (:url cursor)}
                        (dom/img #js {:src src
                                      :alt-text alt-text
                                      :width width
                                      :height height}))
                 (apply dom/div nil
                        (map (fn [data] (dom/div nil (om/build tag-link data))) (:tags cursor))))))))

(defn item [app owner]
  (reify
    om/IRender
    (render [this]
      (dom/li nil
              (dom/a #js {:href (:href app)}
                     (dom/img #js {:src (:src app)}))
              (om/build vote-box app)
              (apply dom/div nil (om/build-all tag (:tags app)))))))









(defn upvote-checkbox [app owner]
  (om/component
   (dom/input #js {:type "checkbox"
                   :checked (pos? (:vote app))
                   :onChange (fn [e]
                               (if (not :checked)
                                 (do (om/update! app :vote 1)
                                     (om/transact! app :vote-count inc))))}
              "Upvote")))

(defn downvote-checkbox [app owner]
  (om/component
   (dom/input #js {:type "checkbox"
                   :checked (neg? (:vote app))
                   :onChange (fn [e]
                               (if (not :checked)
                                 (do (om/update! app :vote -1)
                                     (om/transact! app :vote-count dec)))
                               )}
              "Downvote")))

(defn vote-box [app owner]
  (om/component
   (dom/form nil
            (om/build upvote-checkbox app)
            (dom/div nil (:vote-count app))
            (om/build downvote-checkbox app))))

(defonce app-state (atom {:item {:id 1
                                 :url "http://www.example.com/items/1"
                                 :img {:src "http://www.example.com/x.jpg"
                                       :alt-text "X JPG"
                                       :width 190
                                       :height 190}
                                 :tags [{:name "artsy"
                                         :url "http://www.example.com/tags/artsy"},
                                        {:name "pretty"
                                         :url "http://www.example.com/tags/pretty"}]}}))

(defn main []
  (om/root
   (fn [app owner]
     (reify
       om/IRender
       (render [_]
         (om/build missing-link (:item app))
         ;; (g/row {}
         ;;  (g/col {:md 3}
         ;;   (r/well {} "Well Text"
         ;;           (b/button {} "Default")
         ;;           (dom/h1 nil (:text app))))
         ;;  (g/col {:md 8}
         ;;         (r/well {} "More Well Text" )))))
         )))
   app-state
   {:target (. js/document (getElementById "app"))}))
