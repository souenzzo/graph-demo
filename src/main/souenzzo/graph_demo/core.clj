(ns souenzzo.graph-demo.core
  (:require [io.pedestal.http :as http]
            [crux.api :as crux]
            [io.pedestal.http.route :as route]
            [ring.util.mime-type :as mime]
            [cognitect.transit :as transit]
            [com.wsscode.pathom.core :as p]
            [souenzzo.graph-demo.client :as client]
            [clojure.core.async :as async]
            [clojure.core.async.impl.protocols :as async.protocols]
            [com.wsscode.pathom.connect :as pc]
            [clojure.string :as string]
            [com.fulcrologic.fulcro.dom-server :as dom]
            [com.fulcrologic.fulcro.components :as fc]
            [taoensso.timbre :as log]
            [cljs.compiler :as cljs.comp]
            [com.fulcrologic.fulcro.algorithms.normalize :refer [tree->db]]
            [io.pedestal.interceptor :as interceptor]
            [com.fulcrologic.fulcro.routing.legacy-ui-routers :as fr]
            [edn-query-language.core :as eql])
  (:import (crux.api ICruxAPI)
           (java.time Duration)
           (java.io ByteArrayOutputStream)))

(set! *warn-on-reflection* true)

(defn read-port?
  [x]
  (satisfies? async.protocols/ReadPort x))

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
        main-fn `client/main
        onload (str (cljs.comp/munge main-fn) "()")
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
  (async/thread
    (let [db (crux/db system)
          user-id (keyword "user.id" (str id))
          {:keys [user/friends user/color]} (crux/entity db user-id)]
      {:user/color   (or color "#ffffff")
       :user/friends (for [friend friends]
                       {:user/id (name friend)})})))

(pc/defmutation add-friend
  [{::keys [system timeout]} {:user/keys [id new-friend]}]
  {::pc/sym    `user/add-friend
   ::pc/output [:user/id]
   ::pc/params [:user/id
                :user/new-friend]}
  (async/thread
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
      {:user/id id})))

(pc/defmutation set-color
  [{::keys [system timeout]} {:user/keys [id color]}]
  {::pc/sym    `user/set-color
   ::pc/output [:user/id]
   ::pc/params [:user/id
                :user/color]}
  (async/thread
    (let [db (crux/db system)
          e (crux/entity db (keyword "user.id" (str id)))
          tx [[:crux.tx/put
               (assoc e :user/color color)]]
          {:crux.tx/keys [tx-time]} (crux/submit-tx system tx)]
      (crux/sync system tx-time timeout)
      {:user/id id})))

(def my-app-registry
  [friends add-friend set-color index-explorer ssr-router])

(defn >params
  [ast]
  (if (and (contains? ast :children)
           (or (= :root (:type ast))
               (and (= :join (:type ast))
                    (not (empty? (:params ast)))
                    (= ">" (namespace (:dispatch-key ast))))))
    (update ast :children (partial mapv (fn [x]
                                          (>params (update x :params (partial merge (:params ast)))))))
    ast))


(def api
  {:name  ::api
   :enter (fn enter-api
            [{{::keys [transit-type parser]
               :keys  [body async-supported?]
               :as    request} :request
              :as              ctx}]
            (let [params (transit/read (transit/reader body :json))
                  query (-> params
                            eql/query->ast
                            >params
                            eql/ast->query)
                  result (parser request query)
                  ->response (fn ->response [data]
                               (assoc ctx :response {:body   (fn [output]
                                                               (try
                                                                 (let [writer (transit/writer output transit-type)]
                                                                   (transit/write writer data))
                                                                 (catch Throwable e
                                                                   (log/error e))))
                                                     :status 200}))]
              (cond
                (and (read-port? result) async-supported?) (async/go
                                                             (->response (async/<! result)))
                (read-port? result) (->response (async/<!! result))
                :else (->response result))))})

(defn index
  [{::keys [parser] :as request}]
  (let [initial-state {} #_(async/<!! (parser request (fc/get-query Index)))]
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
  (p/parallel-parser
    {::p/mutate  pc/mutate-async
     ::p/plugins [(pc/connect-plugin {::pc/register my-app-registry})
                  p/error-handler-plugin
                  p/trace-plugin]}))


(def service
  {:env                         :prod
   ::p/reader                   [p/map-reader
                                 pc/parallel-reader
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
