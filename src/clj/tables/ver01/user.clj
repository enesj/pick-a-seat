(ns tables.ver01.user
  (:require [reloaded.repl :refer [go reset stop]]
          [tables.ver01.components.components :refer [dev-system]]
          [schema.core :as s]))

(defn start-dev-system []
  (s/set-fn-validation! true)
  (go))

(reloaded.repl/set-init! dev-system)