(defproject closp-tables "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :source-paths ["src/clj" "src/cljs" "src/cljc"]

  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.10.238"]
                 [reagent "0.8.0"]
                 [mount "0.1.12"]
                 [compojure "1.6.1"]
                 [ring/ring-jetty-adapter "1.6.3"]
                 [org.clojure/tools.namespace "0.2.11"]
                 [org.clojure/tools.nrepl "0.2.13"]
                 [complex/complex "0.1.10"]
                 [binaryage/devtools "0.9.10"]
                 [cljsjs/svg-intersections "0.3.0-0"]
                 [com.rpl/specter "1.1.1"]]


  :plugins [[lein-cljsbuild "1.1.7"]]


  :min-lein-version "2.8.1"

  ; leaving this commented because of: https://github.com/cursiveclojure/cursive/issues/369
  ;:hooks [leiningen.cljsbuild]

  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]

  :cljsbuild
  {:builds {:dev {:source-paths ["src/cljs" "src/cljc" "env/dev/cljs"]
                  :figwheel     {:on-jsload "closp-tables.dev/main"}
                  :compiler     {:main       "closp-tables.dev"
                                 :asset-path "/js/compiled/out"
                                 :output-to  "resources/public/js/compiled/app.js"
                                 :output-dir "resources/public/js/compiled/out"
                                 :source-map      true
                                 :source-map-timestamp true}}
            :adv {:source-paths ["src/cljs" "src/cljc"]
                  :compiler     {:output-to     "resources/public/js/compiled/app.js"
                                 ; leaving this commented because of: https://github.com/cursiveclojure/cursive/issues/369
                                 ;:jar           true
                                 :optimizations :advanced
                                 :pretty-print  false}}}}

  :figwheel {:css-dirs   ["resources/public/css"]} ;; watch and update CSS

  :profiles {:dev     {:repl-options {:init-ns          pickaseat.ver01.user
                                      :nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
                       :plugins      [[lein-ring "0.9.0"]
                                      [lein-figwheel "0.5.15"]]

                       :dependencies [[org.apache.httpcomponents/httpclient "4.5.5"]
                                      [com.cemerick/piggieback "0.2.2"]
                                      [figwheel-sidecar "0.5.15"]]}

             :uberjar {:auto-clean false                    ; not sure about this one
                       :omit-source true
                       :aot         :all}}

  :test-paths ["test/clj" "integtest/clj"]

  :test-selectors {:unit (fn [m] (not (or (:integration m))))
                   :integration :integration
                   :cur :cur                                ; one more selector for, give it freely to run only
                                                            ; the ones you need currently
                   :all (constantly true)}

  :test2junit-output-dir "test-results"

  :main pickaseat.ver01.core

  :uberjar-name "closp-tables.jar"

  :aliases {"rel-jar" ["do" "clean," "cljsbuild" "once" "adv," "uberjar"]
            "unit" ["do" "test" ":unit"]
            "integ" ["do" "test" ":integration"]}


  :test-refresh {:quiet true
                 :changes-only true})
