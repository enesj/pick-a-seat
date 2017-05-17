(ns pickaseat.ver01.floor-map.floor-edit
  (:require
    [pickaseat.ver01.floor-map.floor-components :as comps]
    [pickaseat.ver01.data.floor-data :as fd]
    [reagent.core :as r]
    [pickaseat.ver01.floor-map.floor-draw-events :as de]
    [cljs.core.async :as async :refer [chan]]
    [pickaseat.ver01.data.common-data :as cd]
    [pickaseat.ver01.data.table_data :as td]
    [pickaseat.ver01.tables.tables-components :as c]
    [pickaseat.ver01.floor-map.floor-draw :as draw]))


(defn edit-svg [start-point-style end-point-style connection-point-style opacity turtle figures snap-points line polyline
                pen cut-poly cut-line common-data ui-channel x-bcr y-bcr data]
  [:svg
   {
    :width         (:w common-data)
    :height        (:h common-data)
    :ref           #(when %
                      (reset! x-bcr (.-left (.getBoundingClientRect %)))
                      (reset! y-bcr (.-top (.getBoundingClientRect %))))
    :on-mouse-down (fn [e]
                     (.preventDefault e)
                     (de/run-program ui-channel
                                     (de/draw-start)))
    :on-mouse-up   (fn [e]
                     (.preventDefault e)
                     (de/run-program ui-channel
                                     (de/draw-poly)))

    :on-mouse-move (fn [e]
                     (.preventDefault e)
                     (de/run-program ui-channel
                                     (de/draw-line
                                       (- (.-clientX e) @x-bcr)
                                       (- (.-clientY e) @y-bcr))))}

   cd/filters
   [:g
    (when-not (empty? figures)
      (draw/draw-figures figures polyline opacity))]])

