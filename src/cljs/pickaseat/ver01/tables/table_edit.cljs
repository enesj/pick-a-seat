(ns pickaseat.ver01.tables.table-edit
  (:require [pickaseat.ver01.data.table-data :as table-data]))


(defn circle [center radius attributes show-center? on-drag]
  (let [[cx cy] center]
    [:g {:key (rand 1000)}
     [:circle (merge {:cx            cx :cy cy :r radius}
                      ;:on-mouse-up   (fn [e]
                      ;                 (when (:id attributes)
                      ;                   (swap! table-data/tables-state assoc-in [:selection :selected] [(:id attributes)])))
                      ;:on-mouse-down (when on-drag (fn [e]
                      ;                               (.stopPropagation e)
                      ;                               (dragging on-drag [(.-clientX e) (.-clientY e)] [(:id attributes) [center radius] nil])))}
                     attributes)]
     (if show-center? (circle center 0 {:r 2 :fill "rgba(0,0,0,0.8)"} false nil))]))
