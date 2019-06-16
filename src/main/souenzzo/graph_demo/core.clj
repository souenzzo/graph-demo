(ns souenzzo.graph-demo.core
  (:require [io.pedestal.http :as http]
            [crux.api :as crux]
            [io.pedestal.http.route :as route]
            [ring.util.mime-type :as mime]
            [cognitect.transit :as transit]
            [com.wsscode.pathom.core :as p]
            [clojure.core.async :as async]
            [com.wsscode.pathom.connect :as pc])
  (:import (crux.api ICruxAPI)
           (java.util UUID)))


(defonce ^ICruxAPI system
         (crux/start-standalone-system {:kv-backend    "crux.kv.memdb.MemKv"
                                        :event-log-dir "log/db-dir-1"
                                        :db-dir        "data/db-dir-1"}))

(defn index-enter
  [{:as ctx}]
  (assoc ctx :response {:body    "<!DOCTYPE html>
<html>
<head>
<meta charset='utf-8'>
</head>
<body>
<div id='app'></div>
<script src='/js/main/main.js'></script>
</body>
</html>
  "
                        :headers {"Content-Security-Policy" ""
                                  "Content-Type"            (mime/default-mime-types "html")}
                        :status  200}))

(def index
  {:name  ::index
   :enter index-enter})



(pc/defresolver friends
  [app {:user/keys [id]}]
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
  [app {:user/keys [id new-friend]}]
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
              :user/friends (vec new-friends)}]]]
    (crux/submit-tx system tx)
    {:user/id id}))

(pc/defmutation set-color
  [app {:user/keys [id color]}]
  {::pc/sym    `user/set-color
   ::pc/output [:user/id]
   ::pc/params [:user/id
                :user/color]}
  (let [db (crux/db system)
        e (crux/entity db (keyword "user.id" (str id)))
        tx [[:crux.tx/put
             (assoc e :user/color color)]]]
    (crux/submit-tx system tx))
  {:user/id id})

(pc/defmutation focus
  [ctx args]
  {::pc/sym    `user/focus
   ::pc/params [:user/id]
   ::pc/output [:user/id]}
  args)


(def my-app-registry
  [friends add-friend focus set-color])

(def parser
  (p/parallel-parser
    {::p/env     {::p/reader               [p/map-reader
                                            pc/parallel-reader
                                            pc/open-ident-reader
                                            pc/index-reader
                                            p/env-placeholder-reader]
                  ::p/placeholder-prefixes #{">"}}
     ::p/mutate  pc/mutate-async
     ::p/plugins [(pc/connect-plugin {::pc/register my-app-registry}) ; setup connect and use our resolvers
                  p/error-handler-plugin
                  p/request-cache-plugin
                  p/trace-plugin]}))

(defn api-enter
  [{{:keys [body]} :request
    :keys          [request]
    :as            ctx}]
  (let [params (transit/read (transit/reader body :json))
        result (async/<!! (parser request params))]
    (assoc ctx
      :response {:body   (fn [w]
                           (let [writer (transit/writer w :json)]
                             (transit/write writer result)))
                 :status 200})))

(def api
  {:name  ::api
   :enter api-enter})

(def routes
  `#{["/" :get index]
     ["/api" :post api]})

(def service
  {:env              :prod
   ::http/port       8080
   ::http/routes     routes
   ::http/mime-types mime/default-mime-types
   ::http/type       :jetty})

(defn default-interceptors
  [{:keys [env]
    :as   service-map}]
  (let [dev? (= env :dev)]
    (cond-> service-map
            dev? (update ::http/routes (fn [r]
                                         #(route/expand-routes r)))
            dev? (assoc ::http/join? false)
            :always http/default-interceptors
            dev? http/dev-interceptors)))
