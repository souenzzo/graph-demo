(ns souenzzo.graph-demo.core-test
  (:require [clojure.test :refer [deftest]]
            [midje.sweet :refer [fact =>]]
            [io.pedestal.test :refer [response-for]]
            [souenzzo.graph-demo.core :as gd]
            [io.pedestal.http :as http]
            [cognitect.transit :as transit]
            [clojure.java.io :as io]
            [crux.api :as crux]
            [com.fulcrologic.fulcro.mutations :as fm]
            [edn-query-language.core :as eql]))

(defn init
  [service]
  (let [id (str (gensym "test-db-"))]
    (-> service
        (assoc ::gd/system
               (crux/start-standalone-system {:kv-backend    "crux.kv.memdb.MemKv"
                                              :event-log-dir (str "log/" id)
                                              :db-dir        (str "data/" id)}))
        gd/default-interceptors
        http/create-servlet)))

(defn api
  [app eql]
  (let [{::http/keys [service-fn]} app
        {:keys [body]} (response-for service-fn :post "/api"
                                     :body (gd/pr-transit-str eql))]
    (transit/read (transit/reader (io/input-stream (.getBytes body))
                                  :json))))

(defn mutation
  [app sym args]
  (let [env {:state (atom nil)
             :ast   (eql/query->ast1 `[(~sym ~args)])}
        eql (-> (fm/mutate env)
                :remote
                (apply [env])
                :ast
                eql/ast->query
                vector)]
    (get (api app eql) sym)))

(deftest add-friend-test
  (let [app (init gd/service)]
    (fact
      (mutation app `user/add-friend {:user/id         "foo"
                                      :user/new-friend "bar"})
      => {:user/color   "#ffffff"
          :user/friends [{:user/color   "#ffffff"
                          :user/friends []
                          :user/id      "bar"}]
          :user/id      "foo"})))
