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

(defn ui-user-card
  [{:keys [ident color]} & child]
  (dom/div
    {:style {:backgroundColor color}}
    (dom/a {:style {:backgroundColor "white"
                    :margin          "1rem"}
            :href  (str "#/user/" ident)}
           ident)
    (when child
      (apply dom/div
             {:style {:backgroundColor "grey"
                      :padding         ".2rem"
                      :borderStyle     "solid"
                      :display         "flex"}}
             child))))

(defsc Friend'sFriend [this props]
  {:query [:user/id
           :user/color]
   :ident :user/id})

(declare ui-friend)

(defsc Friend [this {:user/keys [id friends color]}]
  {:query [:user/id
           :user/color
           {:user/friends (comp/get-query Friend'sFriend)}]
   :ident :user/id}
  (ui-user-card
    {:ident id
     :color color}
    (if friends
      (dom/div
        {:style {:display "flex"}}
        (map ui-friend friends))
      (dom/span "..."))))

(def ui-friend (comp/factory Friend {:keyfn :user/id}))

(defsc User [this {:ui/keys   [new-friend]
                   :user/keys [id color]
                   :as        user}]
  {:query [:user/id
           :user/color
           :ui/new-friend
           {:user/friends (comp/get-query Friend)}]
   :ident :user/id}
  (comp/fragment
    (dom/form
      {:onSubmit (fn [e]
                   (.preventDefault e)
                   (comp/transact! this `[(user/add-friend ~{:user/id         id
                                                             :user/new-friend new-friend})]))}
      (dom/input {:value    new-friend
                  :onChange #(fm/set-value! this :ui/new-friend (-> % .-target .-value))})
      (dom/input {:value "+"
                  :type  "submit"}))
    (dom/form
      {:onSubmit (fn [e]
                   (.preventDefaults e)
                   (comp/transact! this `[(user/set-color ~{:user/id    id
                                                            :user/color color})]))}
      (dom/input {:value    (or color "#ffffff")
                  :type     "color"
                  :onBlur   (fn [e]
                              (.submit (.-form (.-target e))))
                  :onChange #(fm/set-value! this :user/color (-> % .-target .-value))}))
    (ui-friend user)))


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
                   :ui/keys   [current-id]
                   :>/keys    [current]}]
  {:query         [:user/id
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
    (dom/div
      {:style {:display "flex"}}
      (dom/input {:value    (or current-id id "")
                  :onChange #(fm/set-value! this :ui/current-id (-> % .-target .-value))})
      (dom/a {:href (str "#/user/" current-id)}
             ">"))
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
  (let [{:keys [trace targetId initialDb
                remoteUrl appKey]} #?(:cljsrn  {:appKey    "graphdemo"
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
                                     :remotes {:remote (-> {:url                remoteUrl
                                                            :request-middleware (cond-> (fhr/wrap-fulcro-request)
                                                                                        (not (string/blank? trace)) (trace-remote))}
                                                           (fhr/fulcro-http-remote))}}
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
