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
            [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
            [edn-query-language.core :as eql]
            [com.fulcrologic.fulcro.routing.dynamic-routing :as dr]
            [com.fulcrologic.fulcro.application :as fa]
            [com.fulcrologic.fulcro.data-fetch :as df]
            [com.fulcrologic.fulcro.mutations :as fm]
            [clojure.string :as string]
            [cognitect.transit :as transit])
  #?(:cljs (:import (goog.history Html5History))))


(defn button
  [{:keys [on-press title href color]}]
  #?(:cljsrn  (r/createElement rn/Button #js {:onPress on-press
                                              :title   title})
     :default (if href
                (dom/a {:style {:padding         "1rem"
                                :borderStyle     "solid"
                                :backgroundColor color}
                        :href  href} title)
                (dom/button {:onClick on-press}
                            title))))

(defn text
  [& txts]
  #?(:cljsrn  (r/createElement rn/Text #js {} (string/join txts))
     :default (string/join txts)))

(defn view
  [{:keys [flexDirection]} & child]
  #?(:cljsrn  (apply r/createElement rn/View #js {} child)
     :default (apply dom/div
                     {:style (cond-> {:display "flex"}
                                     flexDirection (assoc :flexDirection flexDirection))}
                     child)))


(defn input
  [{:keys [on-change-text value]}]
  #?(:cljsrn  (r/createElement rn/TextInput #js {:onChangeText on-change-text
                                                 :value        value})
     :default (dom/input {:value    value
                          :onChange #(-> % .-target .-value on-change-text)})))

(defsc Friend'sFriend [this {:user/keys [id color]}]
  {:query [:user/id
           :user/color]
   :ident :user/id}
  (button {:href  (str "#/user/" id)
           :color color
           :title id}))

(def ui-friend's-friend (comp/factory Friend'sFriend {:keyfn :user/id}))

(defsc Friend [this {:user/keys [id friends color]}]
  {:query [:user/id
           :user/color
           {:user/friends (comp/get-query Friend'sFriend)}]
   :ident :user/id}
  (comp/fragment
    (button {:href  (str "#/user/" id)
             :color color
             :title id})
    (view
      {}
      (map ui-friend's-friend friends))))

(def ui-friend (comp/factory Friend {:keyfn :user/id}))

(defsc User [this {:user/keys [id friends color]}]
  {:query [:user/id
           :user/color
           {:user/friends (comp/get-query Friend)}]
   :ident :user/id}
  (comp/fragment
    (text "current user: '" id "'")
    (text "Color: ")
    (input {:value          (or color "#ffffff")
            :on-change-text #(comp/transact! this `[(user/set-color ~{:user/id    id
                                                                      :user/color %})])})
    (text "'" id "'" " friend list:")
    (view
      {:flexDirection "column"}
      (map ui-friend friends))))


(fm/defmutation user/focus
  [{:user/keys [id]}]
  (action [{:keys [state]}]
          (swap! state (fn [st]
                         (cond-> (assoc-in st [:PAGE/users :PAGE/users :ui/current-id] id)
                                 (contains? (:user/id st) id) (assoc-in [:PAGE/users :PAGE/users :current] [:user/id id])))))
  (remote [env]
          (assoc env
            :ast (eql/query->ast [{[:user/id id] (comp/get-query User)}]))))


(fm/defmutation user/set-color
  [{:user/keys [id color]}]
  (action [{:keys [state]}]
          (swap! state (fn [st]
                         (assoc-in st [:user/id id :user/color] color))))
  (remote [_]
          true))

(def ui-user (comp/factory User {:keyfn :user/id}))

(fm/defmutation user/add-friend
  [{:user/keys [id]}]
  (action [{:keys [state]}]
          (swap! state (fn [st]
                         (-> st))))
  (remote [env]
          (-> env
              (fm/returning User))))



(defsc Home [this {:user/keys [id]
                   :ui/keys   [new-friend current-id]
                   :>/keys    [current]
                   :or        {new-friend ""}}]
  {:query         [:user/id
                   :ui/new-friend
                   :ui/current-id
                   {:>/current (comp/get-query User)}]
   :ident         (fn []
                    [:user/id id])
   :route-segment ["user" :user/id]
   :will-enter    (fn [app {:user/keys [id]}]
                    (dr/route-deferred [:user/id id]
                                       #(df/load! app [:user/id id] Home
                                                  {:post-mutation        `dr/target-ready
                                                   :post-mutation-params {:target [:user/id id]}})))}
  (comp/fragment
    (view {}
          (input {:value          (or current-id id "")
                  :on-change-text #(fm/set-value! this :ui/current-id %)})
          (button {:href  (str "#/user/" current-id)
                   :title ">"})
          (text "add a friend: ")
          (input {:value          new-friend
                  :on-change-text #(fm/set-value! this :ui/new-friend %)})
          (button {:on-press #(comp/transact! this `[(user/add-friend ~{:user/id         id
                                                                        :user/new-friend new-friend})])
                   :title    "+"}))
    (ui-user current)))

(dr/defrouter RootRouter [_this _props]
  {:router-targets [Home]})

(def ui-root-router (comp/factory RootRouter {}))

(defsc Root [this {:>/keys [root-router]}]
  {:query         [{:>/root-router (comp/get-query RootRouter)}]
   :initial-state (fn [_]
                    {:>/root-router (comp/get-initial-state RootRouter _)})}
  (ui-root-router root-router))


(def ui-root (comp/factory Root))

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
                 (or root (comp/fragment)))))))

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
                                                                            (dr/change-route app (-> (string/split token #"/")
                                                                                                     rest
                                                                                                     vec))))
                                        (.setEnabled true))
                                      #_(js/addEventListener "click" (fn [e]
                                                                       (when-let [pathname (-> e .-target .-pathname)]
                                                                         (.preventDefault e)
                                                                         (.setToken history pathname)))))))
        app (fa/fulcro-app (assoc service :client-did-mount client-did-mount))]
    #?(:cljsrn (.registerComponent rn/AppRegistry appKey (constantly (app->react-component-target app)))
       :cljs   (fa/mount! app Root (gdom/getElement targetId)
                          #_{:hydrate?          initial-db?
                             :initialize-state? (not initial-db?)}))
    (reset! state app)))

(defn after-load
  []
  (fa/force-root-render! @state))
