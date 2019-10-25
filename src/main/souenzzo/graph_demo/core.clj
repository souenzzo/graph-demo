(ns souenzzo.graph-demo.core
  (:require [cljs.compiler :as cljs.comp]
            [clojure.string :as string]
            [cognitect.transit :as transit]
            [com.fulcrologic.fulcro.algorithms.normalize :refer [tree->db]]
            [com.fulcrologic.fulcro.components :as fc]
            [com.fulcrologic.fulcro.dom-server :as dom]
            [com.fulcrologic.fulcro.routing.legacy-ui-routers :as fr]
            [com.wsscode.pathom.connect :as pc]
            [com.wsscode.pathom.core :as p]
            [crux.api :as crux]
            [io.pedestal.http :as http]
            [io.pedestal.http.route :as route]
            [io.pedestal.interceptor :as interceptor]
            [ring.util.mime-type :as mime]
            [souenzzo.graph-demo.client :as client]
            [taoensso.timbre :as log])
  (:import (crux.api ICruxAPI)
           (java.time Duration)
           (java.io ByteArrayOutputStream)))

(set! *warn-on-reflection* true)

(defn pr-transit-str
  [x]
  (let [boas (ByteArrayOutputStream.)
        writer (transit/writer boas :json)]
    (transit/write writer x)
    (str boas)))

(fc/defsc Index [this {:>/keys [#_root]}]
  {:query         [#_{:>/root (fc/get-query client/Root)}]
   :initial-state (fn [_]
                    {#_#_:>/root (fc/get-initial-state client/Root _)})}
  (let [target-id "app"
        onload "souenzzo.graph_demo.client()"
        #_#_initial-db (tree->db client/Root root true)]
    (dom/html
      (dom/head
        (dom/meta {:charset "utf-8"}))
      (dom/body
        {#_#_:data-initial-db (pr-transit-str initial-db)
         :data-target-id  target-id
         :data-remote-url "/api"
         :onload          onload}
        (dom/div
          {:id target-id}
          #_(client/ui-root root))
        (dom/script {:src "/js/main/main.js"})))))

(def ui-index (fc/factory Index))

(pc/defresolver index-explorer [env _]
  {::pc/input  #{:com.wsscode.pathom.viz.index-explorer/id}
   ::pc/output [:com.wsscode.pathom.viz.index-explorer/index]}
  {:com.wsscode.pathom.viz.index-explorer/index
   (p/transduce-maps
     (remove (comp #{::pc/resolve ::pc/mutate} key))
     (get env ::pc/indexes))})

(pc/defresolver ssr-router
  [ctx _]
  {::pc/params [:pathname]
   ::pc/output [::fr/id
                {::fr/current-route {:PAGE/users [:PAGE/ident
                                                  :PAGE/id
                                                  :ui/new-friend
                                                  {:current [:user/id]}
                                                  :ui/current-id]}}]}
  (let [user-id (or (some-> ctx :path-params :path (subs 5))
                    (some-> (p/params ctx)
                            (update :pathname (fn [x]
                                                (when (> (count x) 10)
                                                  x)))
                            :pathname
                            (subs 10))
                    "foo")]
    {::fr/id            :PAGE/root-router
     ::fr/current-route {:PAGE/users    true
                         :PAGE/ident    :PAGE/users
                         :PAGE/id       :PAGE/users
                         :ui/new-friend "bar"
                         :current       {:user/id user-id}
                         :ui/current-id user-id}}))

(pc/defresolver friends
  [{::keys [system timeout]} {:user/keys [id]}]
  {::pc/input  #{:user/id}
   ::pc/output [:user/color
                {:user/friends [:user/id]}]}
  (let [db (crux/db system)
        user-id (keyword "user.id" (str id))
        {:keys [user/friends user/color]} (crux/entity db user-id)]
    {:user/color   (or color "#ffffff")
     :user/friends (for [friend friends]
                     {:user/id (name friend)})}))

(pc/defmutation add-friend
  [{::keys [system timeout]} {:user/keys [id new-friend]}]
  {::pc/sym    `user/add-friend
   ::pc/output [:user/id]
   ::pc/params [:user/id
                :user/new-friend]}
  (let [db (crux/db system)
        user-id (keyword "user.id" (str id))
        new-friend-id (keyword "user.id" (str new-friend))
        {:keys [user/friends]} (crux/entity db (keyword "user.id" (str id)))
        new-friends (into #{new-friend-id} friends)
        tx [[:crux.tx/put
             {:crux.db/id   user-id
              :user/friends (vec new-friends)}]]
        {:crux.tx/keys [tx-time]} (crux/submit-tx system tx)]
    (crux/sync system tx-time timeout)
    {:user/id id}))

(pc/defmutation set-color
  [{::keys [system timeout]} {:user/keys [id color]}]
  {::pc/sym    `user/set-color
   ::pc/output [:user/id]
   ::pc/params [:user/id
                :user/color]}
  (let [db (crux/db system)
        e (crux/entity db (keyword "user.id" (str id)))
        tx [[:crux.tx/put
             (assoc e :user/color color)]]
        {:crux.tx/keys [tx-time]} (crux/submit-tx system tx)]
    (crux/sync system tx-time timeout)
    {:user/id id}))

(def my-app-registry
  [friends add-friend set-color index-explorer ssr-router])

(defn api
  [{::keys [transit-type parser]
    :keys  [body]
    :as    request}]
  (let [query (transit/read (transit/reader body :json))
        result (parser request query)]
    {:body   (fn [out]
               (try
                 (let [writer (transit/writer out transit-type)]
                   (transit/write writer result))
                 (catch Throwable e
                   (log/error e))))
     :status 200}))

(defn index
  [{::keys [parser] :as request}]
  (let [initial-state {} #_(parser request (fc/get-query Index))]
    {:body    (string/join "\n" ["<!DOCTYPE html>"
                                 (dom/render-to-str (ui-index initial-state))])
     :headers {"Content-Security-Policy" ""
               "Cache-Control"           "no-store"
               "Content-Type"            (mime/default-mime-types "html")}
     :status  200}))

(def not-found->app
  {:name  ::not-found->app
   :leave (fn [{:keys [response]
                :as   ctx}]
            (if (http/response? response)
              ctx
              (assoc ctx :response {:headers {"Location"      "/app"
                                              "Cache-Control" "no-store"}
                                    :status  301})))})

(def routes
  `#{["/app/*path" :get index :route-name ::index*]
     ["/app" :get index]
     ["/api" :post api]})

(defn add-service-map
  [{::http/keys [interceptors]
    :as         service-map}]
  (let [+service-map {:name  ::add-service-map
                      :enter (fn [ctx]
                               (update ctx :request (partial merge service-map)))}]
    (assoc service-map
      ::http/interceptors (into (empty interceptors)
                                (comp cat
                                      (map interceptor/interceptor))
                                [[+service-map]
                                 interceptors]))))

(def timeout (Duration/ofSeconds 1))
(defonce ^ICruxAPI system
         (crux/start-standalone-system {:kv-backend    "crux.kv.memdb.MemKv"
                                        :event-log-dir "log/db-dir-1"
                                        :db-dir        "data/db-dir-1"}))

(def parser
  (p/parser
    {::p/mutate  pc/mutate
     ::p/plugins [(pc/connect-plugin {::pc/register my-app-registry})
                  p/error-handler-plugin
                  p/trace-plugin]}))


(def service
  {:env                         :prod
   ::p/reader                   [p/map-reader
                                 pc/reader2
                                 pc/open-ident-reader
                                 pc/index-reader
                                 p/env-placeholder-reader]
   ::p/placeholder-prefixes     #{">"}
   ::parser                     parser
   ::timeout                    timeout
   ::system                     system
   ::transit-type               :json
   ::http/not-found-interceptor not-found->app
   ::http/port                  8080
   ::http/routes                routes
   ::http/mime-types            mime/default-mime-types
   ::http/type                  :jetty})

(defn default-interceptors
  [{:keys [env]
    :as   service-map}]
  (let [dev? (= env :dev)]
    (cond-> service-map
            dev? (update ::http/routes (fn [r]
                                         #(route/expand-routes r)))
            dev? (assoc ::http/join? false
                        ::transit-type :json-verbose)
            :always http/default-interceptors
            dev? http/dev-interceptors
            :always add-service-map)))
