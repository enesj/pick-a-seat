(ns tables.ver01.core
  (:require [clojure.tools.logging :as log]
            [reloaded.repl :refer [go]]
            [tables.ver01.cljccore :as cljc]
            [tables.ver01.components.components :refer [prod-system]])
  (:gen-class))

(defn -main [& args]
  (reloaded.repl/set-init! prod-system)
  (go)
  (cljc/foo-cljc "hello from cljx")
  (log/info "server started."))
