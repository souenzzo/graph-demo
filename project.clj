(def deps
  (-> (slurp "deps.edn")
    (clojure.edn/read-string)))

(defn deps->dependencies
  [deps]
  (-> deps
    (->> (keep (fn [kv]
                 (when-let [v (:mvn/version (val kv))]
                   [(key kv) v]))))
    vec))

(defproject br.com.souenzzo/graph-demo "0.1.0-SNAPSHOT"
  :source-paths ~(-> deps :paths)
  :dependencies ~(-> deps :deps deps->dependencies)
  :profiles {:uberjar {:prep-tasks   ["javac" "compile"
                                      ["cljsbuild"]]
                       :uberjar-name "graph-demo.jar"
                       :aot          :all}
             :cljs    {:dependencies ~(-> deps :aliases :dev :extra-deps deps->dependencies)}}
  :aliases {"cljsbuild" ["with-profile" "cljs" "run" "-m" "shadow.cljs.devtools.cli" "release" "main"]}
  :main souenzzo.graph-demo.core)
