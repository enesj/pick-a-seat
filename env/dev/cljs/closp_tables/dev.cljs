(ns closp-tables.dev
  (:require [schema.core :as s]
            [tables.ver01.core :as core]))

(s/set-fn-validation! true)

(enable-console-print!)

(defn main [] (core/main))
