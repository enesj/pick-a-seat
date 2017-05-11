(ns closp-tables.dev
  (:require [schema.core :as s]
            [pickaseat.ver01.core :as core]))

(s/set-fn-validation! true)

(enable-console-print!)

(defn main [] (core/main))
