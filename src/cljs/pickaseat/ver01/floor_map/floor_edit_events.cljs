(ns pickaseat.ver01.floor-map.floor-edit-events
  (:require
    [goog.events :as events]
    [pickaseat.ver01.data.floor-data :as fd]))

(defn mouse-up []
  (fn [e]
    (.preventDefault e)
    ;(js/console.log "up")
    (swap! fd/data assoc-in [:selection :active] false)
    (if  (not-empty (:selected (:selection @fd/data)))
      (swap! fd/data assoc-in [:selection :show] true)
      (swap! fd/data assoc-in [:selection :show] false))))



