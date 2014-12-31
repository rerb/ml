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

(defn source-panel [source owner]
  (reify
    om/IRender
    (render [_]
      (p/panel {}
               (dom/div #js {:dangerouslySetInnerHTML #js {:__html source}})))))

(defn add-comment [owner]
  (let [source (-> (om/get-node owner "source") .-value)
        language (-> (om/get-node owner "language") .-value)
        commentary (-> (om/get-node owner "commentary") .-value)]

    (POST "http://127.0.0.1:8000/api/comments/"
       {:handler #(.log js/console (str ":handler: " %))
        :error-handler error-handler
        :params {"source" source
                 "language" language
                 "commentary" commentary}
        :format :json
        :response-format :json
        :keywords? true})

    (.log js/console (str "language: " language))))

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
      (grid/grid {}
                 (grid/row {:className "center-block"}
                           (grid/col {}
                                     (om/build up-vote-button
                                               comment
                                               {:state {:ch (om/get-state owner :upvote-ch)}})

                                     (b/button {} (:votes comment))

                                     (om/build down-vote-button
                                               comment
                                               {:state {:ch (om/get-state owner :downvote-ch)}})

                                     (b/button  {:onClick #(om/set-state! owner :show-comment-form true)}
                                                "Send a good comment")))

                 (when (om/get-state owner :show-comment-form)
                   (grid/row {}
                             (grid/col {}
                                       (p/panel {}
                                                (i/input {:type "textarea"
                                                          :label "Comment"
                                                          :placeholder "Paste comment here."
                                                          :ref "source"})
                                                (i/input {:type "select"
                                                          :label "Language"
                                                          :ref "language"}
                                                         (dom/option {:value "python"} "Python")
                                                         (dom/option {:value "c"} "C")
                                                         (dom/option {:value "bash"} "Bash")
                                                         (dom/option {:value "other"} "Other"))
                                                (i/input {:type "textarea"
                                                          :label "Commentary"
                                                          :placeholder "Any commentary?"
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
                                                           :onClick #(or (add-comment owner) (om/set-state! owner :show-comment-form false))}
                                                          "Send")
                                                (b/button {:bs-style "default"
                                                           :onClick #(om/set-state! owner :show-comment-form false)}
                                                          "Cancel")))))))))

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
               (grid/grid {}
                          (grid/row {}
                                    (om/build button-bar
                                              comment))
                          (grid/row {}
                                    (grid/col {:md 12}
                                              (om/build source-panel
                                                        (om/get-state owner
                                                                      :colorized-source))
                                              (dom/div {}
                                                       (:commentary comment)))))
               (dom/hr {})))))

(defn comment-list [app owner]
  (reify
    om/IRender
    (render [_]
      (grid/grid {}
                 (map (fn [comment] (grid/row {}
                                           (grid/col {}
                                                     (om/build comment-box comment))))
                      (:comments app))))))

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
                                   :language "http://localhost:8000/api/languages/1/"}]}))

(defn main []
  (om/root
   comment-list
   app-state
   {:target (. js/document (getElementById "app"))}))
