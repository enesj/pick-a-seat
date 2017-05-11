(ns pickaseat.ver01.components.components
  (:require
    [com.stuartsierra.component :as component]
    (system.components
      [repl-server :refer [new-repl-server]])
    [pickaseat.ver01.components.server :refer [new-web-server]]
    [pickaseat.ver01.components.handler :refer [new-handler]]
    [pickaseat.ver01.components.config :as c]
    [pickaseat.ver01.components.db :refer [new-db]]
    [pickaseat.ver01.components.locale :as l]))


(defn dev-system []
  (component/system-map
    :locale (l/new-locale)
    :config (c/new-config (c/prod-conf-or-dev))
    :db (component/using (new-db) [:config])
    :handler (component/using (new-handler) [:config :locale :db])
    :web (component/using (new-web-server) [:handler :config])))


(defn prod-system []
  (component/system-map
    :locale (l/new-locale)
    :config (c/new-config (c/prod-conf-or-dev))
    :db (component/using (new-db) [:config])
    :handler (component/using (new-handler) [:config :locale :db])
    :web (component/using (new-web-server) [:handler :config])))
