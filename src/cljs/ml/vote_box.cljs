(ns ml.vote-box
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [<! put! chan]]
            [om-bootstrap.grid :as grid]
            [om-bootstrap.button :as b]
            [om-bootstrap.random :as r])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(enable-console-print!)

(defn- down-vote-button [app owner]
  (reify
    om/IRender
    (render [_]
      (b/button {:onClick #(put! (om/get-state owner :ch) 1)
                 :bs-style (if (neg? (:vote app)) "primary" "default")}
                (r/glyphicon {:glyph "arrow-down"})))))

(defn- up-vote-button [app owner]
  (reify
    om/IRender
    (render [_]
      (b/button {:onClick #(put! (om/get-state owner :ch) 1)
                 :bs-style (if (pos? (:vote app)) "primary" "default")}
                (r/glyphicon {:glyph "arrow-up"})))))

(defn vote-box [app owner]
  (reify
    om/IInitState
    (init-state [_]
      {:downvote-ch (chan)
       :upvote-ch (chan)})
    om/IWillMount
    (will-mount [_]
      (let [down-ch (om/get-state owner :downvote-ch)
            up-ch (om/get-state owner :upvote-ch)]
        (go (while true
              (let [_ (<! down-ch)
                    vote (:vote @app)]
                (when (not (neg? vote))
                  (om/transact! app :votes dec)
                  (when (pos? vote)
                    (om/transact! app :votes dec))
                  (om/update! app :vote -1)))))
        (go (while true
              (let [_ (<! up-ch)]
                (when (not (pos? (:vote @app)))
                  (om/transact! app :votes inc)
                  (when (neg? (:vote @app))
                    (om/transact! app :votes inc))
                  (om/update! app :vote 1)))))))
    om/IRender
    (render [_]
      (grid/grid {}
                 (grid/row {:className "center-block"}
                           (grid/col {:className "pull-left"}
                                     (om/build up-vote-button
                                               app
                                               {:state {:ch (om/get-state owner :upvote-ch)}}))
                           (grid/col {:className "pull-left"} (:votes app))
                           (grid/col {}
                                     (om/build down-vote-button
                                               app
                                               {:state {:ch (om/get-state owner :downvote-ch)}})))))))
