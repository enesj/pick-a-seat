(ns pickaseat.ver01.floor-map.floor-edit-events
  (:require
    [pickaseat.ver01.data.floor-data :as floor-data]))

(defn mouse-up []
  (fn [e]
    (.preventDefault e)
    ;(js/console.log "up")
    (swap! floor-data/data assoc-in [:selection :active] false)
    (if  (not-empty (:selected (:selection @floor-data/data)))
      (swap! floor-data/data assoc-in [:selection :show] true)
      (swap! floor-data/data assoc-in [:selection :show] false))))



