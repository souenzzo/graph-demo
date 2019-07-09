(ns souenzzo.graph-demo.client
  (:require #?@(:cljs [[goog.dom :as gdom]
                       [goog.events :as gevt]
                       [goog.events.EventType :as event-type]
                       [com.fulcrologic.fulcro.networking.http-remote :as fhr]
                       [com.fulcrologic.fulcro.dom :as dom]]
                :clj  [[com.fulcrologic.fulcro.dom-server :as dom]])
            [com.fulcrologic.fulcro.components :as fc]
            [com.fulcrologic.fulcro.routing.legacy-ui-routers :as fr]
            [com.fulcrologic.fulcro.application :as fa]
            [com.fulcrologic.fulcro.data-fetch :as df]
            [com.fulcrologic.fulcro.mutations :as fm]))


(fc/defsc Friend'sFriend [this {:user/keys [id]}]
  {:query [:user/id]
   :ident :user/id}
  (dom/button {:onClick #(fc/transact! this `[(user/focus ~{:user/id id})])}
              id))

(def ui-friend's-friend (fc/factory Friend'sFriend {:keyfn :user/id}))

(fc/defsc Friend [this {:user/keys [id friends color]}]
  {:query [:user/id
           :user/color
           {:user/friends (fc/get-query Friend'sFriend)}]
   :ident :user/id}
  (dom/div
    {:style (cond-> {}
                    color (assoc :backgroundColor color))}
    id
    (map ui-friend's-friend friends)))

(def ui-friend (fc/factory Friend {:keyfn :user/id}))

(fc/defsc User [this {:user/keys [id friends color]}]
  {:query [:user/id
           :user/color
           {:user/friends (fc/get-query Friend)}]
   :ident :user/id}
  (dom/div
    (dom/div
      "current user: '" id "'")
    (dom/br)
    "Color: " (dom/input {:value    (or color "#ffffff")
                          :type     "color"
                          :onChange #(fc/transact! this `[(user/set-color ~{:user/id    id
                                                                            :user/color (-> % .-target .-value)})])})
    (dom/div "'" id "'" " friend list:")
    (dom/div
      {:style {:borderWidth "3px"
               :borderStyle "solid"
               :borderColor "black"}}
      (map ui-friend friends))))


(fm/defmutation user/focus
  [{:user/keys [id]}]
  (action [{:keys [state]}]
          (swap! state (fn [st]
                         (cond-> (assoc-in st [:PAGE/users :PAGE/users :ui/current-id] id)
                                 (contains? (:user/id st) id) (assoc-in [:PAGE/users :PAGE/users :ui/current] [:user/id id])))))
  (remote [env]
          (-> env
              (fm/returning User)
              (fm/with-target [:PAGE/users :PAGE/users :ui/current]))))


(fm/defmutation user/set-color
  [{:user/keys [id color]}]
  (action [{:keys [state]}]
          (swap! state (fn [st]
                         (assoc-in st [:user/id id :user/color] color))))
  (remote [_]
          true))

(def ui-user (fc/factory User {:keyfn :user/id}))

(fc/defsc Users [this {:PAGE/keys [ident id]
                       :ui/keys   [current current-id new-friend]}]
  {:query         [:PAGE/ident
                   :PAGE/id
                   :ui/new-friend
                   :ui/current-id
                   {:ui/current (fc/get-query User)}]
   :ident         (fn []
                    [ident id])
   :initial-state (fn [_]
                    {:PAGE/ident    :PAGE/users
                     :ui/current-id "foo"
                     :ui/new-friend "bar"
                     :PAGE/id       :PAGE/users})}
  (dom/div
    (dom/input {:value    current-id
                :onChange #(fm/set-value! this :ui/current-id (-> % .-target .-value))})
    (dom/button {:onClick #(df/load! this [:user/id current-id] User
                                     {:target [:PAGE/users :PAGE/users :ui/current]})}
                ">")
    (dom/hr)
    "add a friend: "
    (dom/input {:value    new-friend
                :onChange #(fm/set-value! this :ui/new-friend (-> % .-target .-value))})
    (dom/button
      {:onClick #(fc/transact! this `[(user/add-friend ~{:user/id         current-id
                                                         :user/new-friend new-friend})])}
      "+")
    (ui-user current)))

(fm/defmutation user/add-friend
  [_]
  (action [{:keys [state]}]
          (swap! state (fn [st]
                         (-> st))))
  (remote [env]
          (-> env
              (fm/returning User)
              (fm/with-target [:PAGE/users :PAGE/users :ui/current]))))



(fr/defsc-router RootRouter [this {:PAGE/keys [ident id]}]
  {:default-route  Users
   :ident          (fn [] [ident id])
   :router-targets {:PAGE/users Users}
   :router-id      :PAGE/root-router}
  (dom/div "404"))

(def ui-root-router (fc/factory RootRouter))

(fc/defsc Root [this {:ui/keys [root-router]}]
  {:query         [{:ui/root-router (fc/get-query RootRouter)}]
   :initial-state (fn [_]
                    {:ui/root-router (fc/get-initial-state RootRouter _)})}
  (ui-root-router root-router))

(defonce state (atom nil))

(defn main
  [e]
  #?(:cljs (let [target (gdom/getElement "app")
                 app (fa/fulcro-app {:remotes {:remote (fhr/fulcro-http-remote {})}})]
             (fa/mount! app Root target)
             (reset! state app))))

(defn after-load
  []
  (fa/force-root-render! @state))

#?(:cljs (gevt/listen js/window event-type/LOAD main))
