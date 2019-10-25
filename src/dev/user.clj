(ns user
  (:require [souenzzo.graph-demo.core :as gd]
            [shadow.cljs.devtools.api :as shadow.api]
            [shadow.cljs.devtools.server :as shadow.server]
            [io.pedestal.http :as http]))

(defonce state (atom nil))

(defn -main
  {:shadow/requires-server true}
  [& _]
  (shadow.server/start!)
  (shadow.api/watch :main)
  #_(shadow.api/watch :rn)
  (swap! state (fn [st]
                 (when st
                   (http/stop st))
                 (-> gd/service
                     (assoc :env :dev
                            ::http/file-path "target/public")
                     gd/default-interceptors
                     http/create-server
                     http/start))))
