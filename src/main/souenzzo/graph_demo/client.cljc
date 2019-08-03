(ns souenzzo.graph-demo.client
  (:require #?@(:cljs [[goog.dom :as gdom]
                       [goog.object :as gobj]
                       ["react" :as r]
                       [goog.events :as gevt]
                       [goog.history.EventType :as event-type]
                       [com.fulcrologic.fulcro.networking.http-remote :as fhr]
                       [com.fulcrologic.fulcro.dom :as dom]]
                :clj  [[com.fulcrologic.fulcro.dom-server :as dom]])
            #?@(:cljsrn [["react-native" :as rn]])
            [com.fulcrologic.fulcro.components :as fc]
            [edn-query-language.core :as eql]
            [com.fulcrologic.fulcro.routing.legacy-ui-routers :as fr]
            [com.fulcrologic.fulcro.application :as fa]
            [com.fulcrologic.fulcro.data-fetch :as df]
            [com.fulcrologic.fulcro.mutations :as fm]
            [clojure.string :as string]
            [cognitect.transit :as transit])
  #?(:cljs (:import (goog.history Html5History))))


(defn button
  [app {:keys [on-press title href]}]
  #?(:cljsrn  (r/createElement rn/Button #js {:onPress on-press
                                              :title   title})
     :default (if href
                (dom/a {:href href} title)
                (dom/button {:onClick on-press}
                            title))))

(defn text
  [& txts]
  #?(:cljsrn  (r/createElement rn/Text #js {} (string/join txts))
     :default (string/join txts)))

(defn view
  [{:keys [background-color]} & child]
  #?(:cljsrn  (apply r/createElement rn/View #js {} child)
     :default (apply dom/div {:style {:backgroundColor background-color}} child)))


(defn input
  [{:keys [on-change-text value]}]
  #?(:cljsrn  (r/createElement rn/TextInput #js {:onChangeText on-change-text
                                                 :value        value})
     :default (dom/input {:value    value
                          :onChange #(-> % .-target .-value on-change-text)})))

(fc/defsc Friend'sFriend [this {:user/keys [id]}]
  {:query [:user/id]
   :ident :user/id}
  (button this {:href  (str "/app/user/" id)
                :title (str id ">")}))

(def ui-friend's-friend (fc/factory Friend'sFriend {:keyfn :user/id}))

(fc/defsc Friend [this {:user/keys [id friends color]}]
  {:query [:user/id
           :user/color
           {:user/friends (fc/get-query Friend'sFriend)}]
   :ident :user/id}
  (view
    {:background-color color}
    (text id)
    (map ui-friend's-friend friends)))

(def ui-friend (fc/factory Friend {:keyfn :user/id}))

(fc/defsc User [this {:user/keys [id friends color]}]
  {:query [:user/id
           :user/color
           {:user/friends (fc/get-query Friend)}]
   :ident :user/id}
  (view
    {}
    (view
      {}
      (text "current user: '" id "'"))
    (text "Color: ")
    (input {:value          (or color "#ffffff")
            :on-change-text #(fc/transact! this `[(user/set-color ~{:user/id    id
                                                                    :user/color %})])})
    (text "'" id "'" " friend list:")
    (view
      {}
      (map ui-friend friends))))


(fm/defmutation user/focus
  [{:user/keys [id]}]
  (action [{:keys [state]}]
          (swap! state (fn [st]
                         (cond-> (assoc-in st [:PAGE/users :PAGE/users :ui/current-id] id)
                                 (contains? (:user/id st) id) (assoc-in [:PAGE/users :PAGE/users :current] [:user/id id])))))
  (remote [env]
          (assoc env
            :ast (eql/query->ast [{[:user/id id] (fc/get-query User)}]))))


(fm/defmutation user/set-color
  [{:user/keys [id color]}]
  (action [{:keys [state]}]
          (swap! state (fn [st]
                         (assoc-in st [:user/id id :user/color] color))))
  (remote [_]
          true))

(def ui-user (fc/factory User {:keyfn :user/id}))

(fc/defsc Users [this {:PAGE/keys [ident id]
                       :keys      [current]
                       :ui/keys   [current-id new-friend]}]
  {:query         [:PAGE/ident
                   :PAGE/id
                   :ui/new-friend
                   :ui/current-id
                   {:current (fc/get-query User)}]
   :ident         (fn []
                    [ident id])
   :initial-state (fn [_]
                    {:PAGE/ident    :PAGE/users
                     :ui/current-id "foo"
                     :ui/new-friend "bar"
                     :PAGE/id       :PAGE/users})}
  (view
    {}
    (input {:value          current-id
            :on-change-text #(fm/set-value! this :ui/current-id %)})
    (button this {:href  (str "/app/user/" current-id)
                  :title ">"})
    (text "add a friend: ")
    (input {:value          new-friend
            :on-change-text #(fm/set-value! this :ui/new-friend %)})
    (button this
            {:on-press #(fc/transact! this `[(user/add-friend ~{:user/id         current-id
                                                                :user/new-friend new-friend})])
             :title    "+"})
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
  (text "404"))

(def ui-root-router (fc/factory RootRouter))

(fc/defsc Root [this {:>/keys [root-router]}]
  {:query         [{:>/root-router (fc/get-query RootRouter)}]
   :initial-state (fn [_]
                    {:>/root-router (fc/get-initial-state RootRouter _)})}
  (ui-root-router root-router))

(def ui-root (fc/factory Root))

(defonce state (atom nil))

(defn trace-remote
  ([] (trace-remote identity))
  ([handler]
   (fn [request]
     (handler (update request :body conj :com.wsscode.pathom/trace)))))

(defn app->react-component-target
  [app]
  (let [ref-set-root (atom nil)]
    (fn []
      #?(:cljs (let [[root set-root] (r/useState nil)]
                 (reset! ref-set-root set-root)
                 (when-not root
                   (fa/mount! app Root (fn [ui]
                                         (@ref-set-root ui))))
                 (or root (fc/fragment)))))))

(defn ^:export main
  []
  (let [{:keys [targetId initialDb remoteUrl appKey]} #?(:cljsrn  {:appKey    "graphdemo"
                                                                   :remoteUrl "http://10.0.2.2:8080/api"}
                                                         :cljs    (->> (gobj/getValueByKeys js/document "body" "dataset")
                                                                       (.entries js/Object)
                                                                       (into {} (map (fn [[k v]]
                                                                                       [(keyword k) v]))))
                                                         :default {})
        initial-db #?(:cljs (some->> initialDb
                                     (transit/read (transit/reader :json)))
                      :default nil)
        initial-db? (map? initial-db)
        service (cond-> #?(:cljs    {:shared  {:history (new Html5History)}
                                     :remotes {:remote  (-> {:url                remoteUrl
                                                             :request-middleware (-> (fhr/wrap-fulcro-request)
                                                                                     (trace-remote))}
                                                            (fhr/fulcro-http-remote))
                                               :default {}}}
                           :default {})
                        appKey (assoc :render-root! (fn [ui set-root]
                                                      (set-root ui)))
                        initial-db? (assoc :initial-db initial-db))
        client-did-mount (fn [app]
                           #?(:cljs (let [history (:history (:shared service))]
                                      (doto history
                                        (gevt/listen event-type/NAVIGATE #(when-let [token (.-token %)]
                                                                            (df/load! app :>/load Root
                                                                                      {:params {:pathname token}
                                                                                       :target []})))
                                        (.setUseFragment false)
                                        (.setPathPrefix "http://localhost:8080")
                                        (.setEnabled true))
                                      (js/addEventListener "click" (fn [e]
                                                                     (when-let [pathname (-> e .-target .-pathname)]
                                                                       (.preventDefault e)
                                                                       (.setToken history pathname)))))))
        app (fa/fulcro-app (assoc service :client-did-mount client-did-mount))]
    #?(:cljsrn (.registerComponent rn/AppRegistry appKey (constantly (app->react-component-target app)))
       :cljs   (fa/mount! app Root (gdom/getElement targetId)
                          {:hydrate?          initial-db?
                           :initialize-state? (not initial-db?)}))
    (reset! state app)))

(defn after-load
  []
  (fa/force-root-render! @state))
