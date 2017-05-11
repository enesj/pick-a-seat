(ns pickaseat.ver01.core
  (:require [clojure.tools.logging :as log]
            [reloaded.repl :refer [go]]
            [pickaseat.ver01.cljccore :as cljc]
            [pickaseat.ver01.components.components :refer [prod-system]])
  (:gen-class))

(defn -main [& args]
  (reloaded.repl/set-init! prod-system)
  (go)
  (cljc/foo-cljc "hello from cljx")
  (log/info "server started."))
