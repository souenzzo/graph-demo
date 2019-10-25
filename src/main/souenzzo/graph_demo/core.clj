(ns souenzzo.graph-demo.core
  (:gen-class)
  (:require [clojure.string :as string]
            [cognitect.transit :as transit]
            [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
            [com.fulcrologic.fulcro.dom-server :as dom]
            [com.wsscode.pathom.connect :as pc]
            [com.wsscode.pathom.core :as p]
            [crux.api :as crux]
            [io.pedestal.http :as http]
            [io.pedestal.http.route :as route]
            [io.pedestal.interceptor :as interceptor]
            [ring.util.mime-type :as mime]
            [taoensso.timbre :as log])
  (:import (java.time Duration)
           (java.io ByteArrayOutputStream)))

(set! *warn-on-reflection* true)

(defn pr-transit!
  [out type x]
  (let [writer (transit/writer out type)]
    (transit/write writer x)))

(defn pr-transit-str
  [type x]
  (let [boas (ByteArrayOutputStream.)]
    (pr-transit! boas type x)
    (str boas)))


(pc/defresolver resolver-index [env input]
  {::pc/output [::target-id
                ::remote-url
                ::trace
                ::onload]}
  {::target-id  "app"
   ::remote-url "/api"
   ::trace      "on"
   ::onload     "souenzzo.graph_demo.client.main()"})

(defsc Index [this {::keys [target-id onload remote-url trace]}]
  {:query         [::target-id
                   ::remote-url
                   ::trace
                   ::onload]
   :initial-state (fn [_] {})}
  (dom/html
    (dom/head
      (dom/meta {:charset "utf-8"}))
    (dom/body
      {:data-target-id  target-id
       :data-remote-url remote-url
       :data-trace      trace
       :onload          onload}
      (dom/div
        {:id target-id})
      (dom/script {:src "/js/main/main.js"}))))

(def ui-index (comp/factory Index))

(pc/defresolver index-explorer [env _]
  {::pc/input  #{:com.wsscode.pathom.viz.index-explorer/id}
   ::pc/output [:com.wsscode.pathom.viz.index-explorer/index]}
  (let [index (p/transduce-maps
                (remove (comp #{::pc/resolve ::pc/mutate} key))
                (get env ::pc/indexes))]
    {:com.wsscode.pathom.viz.index-explorer/index index}))

(pc/defresolver friends
  [{::keys [system]} {:user/keys [id]}]
  {::pc/input  #{:user/id}
   ::pc/output [:user/color
                {:user/friends [:user/id]}]}
  (let [db (crux/db system)
        user-id (keyword "user.id" (str id))
        {:keys [user/friends user/color]
         :or   {color "#ffffff"}} (crux/entity db user-id)]
    {:user/color   color
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
  [friends add-friend set-color index-explorer resolver-index])

(defn api
  [{::keys [transit-type parser]
    :keys  [body]
    :as    request}]
  (let [query (transit/read (transit/reader body :json))
        result (parser request query)]
    {:body   (fn [out]
               (try
                 (pr-transit! out transit-type result)
                 (catch Throwable e
                   (log/error e))))
     :status 200}))

(defn index
  [{::keys [parser] :as request}]
  (let [index (parser request (comp/get-query Index))]
    {:body    (string/join "\n" ["<!DOCTYPE html>"
                                 (dom/render-to-str (ui-index index))])
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
   ::transit-type               :json
   ::http/not-found-interceptor not-found->app
   ::http/port                  8080
   ::http/routes                routes
   ::http/mime-types            mime/default-mime-types
   ::http/type                  :jetty})

(defn default-interceptors
  [{:keys [env]
    :as   service-map}]
  (let [dev? (= env :dev)
        system (crux/start-standalone-system {:kv-backend    "crux.kv.memdb.MemKv"
                                              :event-log-dir "log/db-dir-1"
                                              :db-dir        "data/db-dir-1"})]
    (cond-> service-map
            :always (assoc ::system system)
            dev? (update ::http/routes (fn [r]
                                         #(route/expand-routes r)))
            dev? (assoc ::http/join? false
                        ::transit-type :json-verbose)
            :always http/default-interceptors
            dev? http/dev-interceptors
            :always add-service-map)))

(defn -main
  [& _]
  (-> service
      (assoc ::http/resource-path "public")
      default-interceptors
      http/create-server
      http/start))
