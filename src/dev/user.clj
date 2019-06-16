(ns user
  (:require [souenzzo.graph-demo.core :as gd]
            [shadow.cljs.devtools.api :as shadow.api]
            [shadow.cljs.devtools.server :as shadow.server]
            [io.pedestal.http :as http]))

(prn (shadow.server/start!))

(defonce state (atom nil))

(defn -main
  [& _]
  (shadow.api/watch :main)
  (swap! state (fn [st]
                 (when st
                   (http/stop st))
                 (-> gd/service
                     (assoc :env :dev
                            ::http/file-path "target/public")
                     gd/default-interceptors
                     http/create-server
                     http/start))))
