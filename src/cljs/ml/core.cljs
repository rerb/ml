(ns ml.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [<! put! chan]]
            [om-bootstrap.button :as b]
            [om-bootstrap.grid :as grid]
            [om-bootstrap.input :as i]
            [om-bootstrap.panel :as p]
            [om-bootstrap.random :as r]
            [ajax.core :refer [GET POST]]
            [clojure.string :as string]
            [ml.vote-box :refer [up-vote-button down-vote-button]])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(enable-console-print!)

(def language-name-ch (chan))
(def colorized-source-ch (chan))

(defn error-handler [error]
  (.log js/console "something bad happended: ")
  (.log js/console (str ":status " (:status error)))
  (.log js/console (str ":status-text " (:status-text error)))
  (.log js/console (str ":response " (:response error)))  )

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

(defn source-div [source owner]
  (reify
    om/IRender
    (render [_]
      (dom/div #js {:dangerouslySetInnerHTML #js {:__html source}}))))

(defn add-comment [owner]
  (let [source (-> (om/get-node owner "source") .-value)
        language (-> (om/get-node owner "language") .-value)
        commentary (-> (om/get-node owner "commentary") .-value)]
    (POST "http://127.0.0.1:8000/api/comments/"
       {:handler #(.log js/console (str "add-comment:handler: " %))
        :error-handler error-handler
        :params {"source" source
                 "language" language
                 "commentary" commentary}
        :format :json
        :response-format :json
        :keywords? true})))

(def close-comment-form-ch (chan))
(def open-comment-form-ch (chan))

(defn comment-form [app owner]
  (reify
    om/IRender
    (render [this]
      (dom/div {}
               (i/input {:type "textarea"
                         :label "comment"
                         :placeholder "paste comment here"
                         :ref "source"})
               (apply i/input {:type "select"
                               :label "language"
                               :ref "language"}
                      (for [l (:languages app)]
                        (dom/option {:value l} l)))
               (i/input {:type "textarea"
                         :label "commentary"
                         :placeholder "any commentary"
                         :ref "commentary"})
               (b/button {:bs-style "primary"
                          ;; Note below that `or` is my hack for `progn`. I.e., it's just there so I can
                          ;; say, "Do these two things," rather than "Do just this one."
                          ;;
                          ;; See, I want to add a comment and hide the comment form. Seems silly to
                          ;; make `add-comment-and-hide-form` to just do that. So what's the idiomatic
                          ;; way of doing this? Something simple I've forgotten, didn't understand when
                          ;; I read about, or haven't even encountered yet. So much new. Worse recall ==
                          ;; more new (new, again!), too.
                          ;;
                          ;; I know this is stupid, but it appears to be working now and with a big
                          ;; honking comment like this above it, it's not something I'm likely to
                          ;; ignore. Right? Comment on smells?
                          :onClick #(or (add-comment owner) (put! close-comment-form-ch 1))}
                         "send")
               (b/button {:bs-style "default"
                          :onClick  #(put! close-comment-form-ch 1)}
                         "cancel")))))

(defn button-bar [comment owner]
  (reify
    om/IInitState
    (init-state [_]
      {:upvote-ch (chan)
       :downvote-ch (chan)})
    om/IWillMount
    (will-mount [_]
      (let [up-ch (om/get-state owner :upvote-ch)
            down-ch (om/get-state owner :downvote-ch)]
        (go (while true
              (let [_ (<! down-ch)
                    vote (:vote @comment)]
                (when (not (neg? vote))
                  (om/transact! comment :votes dec)
                  (when (pos? vote)
                    (om/transact! comment :votes dec))
                  (om/update! comment :vote -1)))))
        (go (while true
              (let [_ (<! up-ch)]
                (when (not (pos? (:vote @comment)))
                  (om/transact! comment :votes inc)
                  (when (neg? (:vote @comment))
                    (om/transact! comment :votes inc))
                  (om/update! comment :vote 1)))))))
    om/IRender
    (render [this]
      (dom/div {:className "center-block"}
               (om/build up-vote-button
                         comment
                         {:state {:ch (om/get-state owner :upvote-ch)}})
               (b/button {} (:votes comment))
               (om/build down-vote-button
                         comment
                         {:state {:ch (om/get-state owner :downvote-ch)}})
               (b/button  {:onClick #(put! open-comment-form-ch 1)}
                          "send a good comment")))))

(defn comment-box [comment owner]
  (reify
    om/IInitState
    (init-state [_]
      {:colorized-source ""})
    om/IWillMount
    (will-mount [_]
      (colorize (:source comment) (:language comment))
      (go (while true
            (let [colorized-source (<! colorized-source-ch)]
              (om/set-state! owner
                             :colorized-source
                             colorized-source)))))
    om/IRender
    (render [_]
      (dom/div {}
               (om/build button-bar
                         comment)
               (om/build source-div
                         (om/get-state owner
                                       :colorized-source))
               (dom/div {}
                        (:commentary comment))
               (dom/hr {})))))

(defn comment-list [app owner]
  (reify
    om/IRender
    (render [_]
      (grid/grid {}  ;; why, when I replace this `grid/grid` with `dom/div`, does it break?
                 (map (fn [comment] (om/build comment-box comment))
                      (:comments app))))))

(defn show [app view]
  (p/panel {}
           (grid/grid {}
                      (grid/row {} (grid/col {:className "pull-right"}
                                             (dom/h6 {} "please address the comments in the first person")))
                      (grid/row {} (grid/col {} (om/build view app))))))

(defn waterfall-view [app owner]
  (reify
    om/IRender
    (render [_]
      (show app comment-list))))

(defn comment-form-view [app owner]
  (reify
    om/IRender
    (render [_]
      (show app comment-form))))

(defn app [app owner]
  (reify
    om/IRender
    (render [_]
      (om/build (:view app) app))
    om/IWillMount
    (will-mount [_]
      (go (while true
            (let [_ (<! open-comment-form-ch)]
              (om/update! app :view comment-form-view))))
      (go (while true
            (let [_ (<! close-comment-form-ch)]
              (om/update! app :view waterfall-view)))))))

(defonce app-state (atom {:comments [{:id 1
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
                                      :language "http://localhost:8000/api/languages/1/"}]
                          :languages ["perl" "lisp" "ksh" "tcl" "pascal"]
                          :view waterfall-view}))

(defn main []
  (om/root
   app
   app-state
  {:target (. js/document (getElementById "app"))}))
