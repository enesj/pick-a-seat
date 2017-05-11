(ns pickaseat.ver01.floor-map.edit-events
  (:require
    [goog.events :as events]
    [pickaseat.ver01.floor-map.settings :as s]))

(defn mouse-up []
  (fn [e]
    (.preventDefault e)
    ;(js/console.log "up")
    (swap! s/data assoc-in [:selection :active] false)
    (if  (not-empty (:selected (:selection @s/data)))
      (swap! s/data assoc-in [:selection :show] true)
      (swap! s/data assoc-in [:selection :show] false))))



