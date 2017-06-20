(ns pickaseat.ver01.floor-map.floor-draw
  (:require
    [pickaseat.ver01.floor-map.floor-components :as floor-components]
    [reagent.core :as r]
    [pickaseat.ver01.floor-map.floor-draw-events :as floor-draw-events]
    [cljs.core.async :as async :refer [chan]]
    [pickaseat.ver01.data.common-data :as common-data]
    [pickaseat.ver01.tables.tables-components :as tables-components]
    [pickaseat.ver01.floor-map.floor-common :as floor-common]))


(enable-console-print!)

(defn line-color [angle]
  (condp (fn [a b] (zero? (mod b a))) angle
    45 "black"
    30 "gray"
    ;15 "green"
    "lightgray"))

(defn root-preview [ tables ids]
  [:g {:opacity "0.4"}
   (doall (for [id ids]
            ^{:key id} [tables-components/table {:on-drag nil} (r/cursor tables [id])]))])

(defn draw-snap-points [snap-points line connection-point-style]
  [:g
   (floor-components/circle (last line) 0 connection-point-style false nil)
   (for [snap-point snap-points]
     [:g {:key (rand 1000)}
      (floor-components/circle snap-point 1 connection-point-style false nil)
      (floor-components/color-line "orange" [snap-point (last line)] {:stroke-dasharray "5, 5"})])])

(defn draw-svg [new-point-style start-point-style end-point-style connection-point-style circle-point-style circle opacity turtle figures snap-points line
                shadow-raw  shadow-polyline shadow polyline pen cut-poly cut-line common-data ui-channel x-bcr y-bcr data]
  (let [[center r] circle]
    [:svg
     {
      :style {:background-color (:grid-back-color @common-data/data)}
      :width         (:w common-data)
      :height        (:h common-data)
      :ref           #(when %
                        (reset! x-bcr (.-left (.getBoundingClientRect %)))
                        (reset! y-bcr (.-top (.getBoundingClientRect %))))
      :on-mouse-down (fn [e]
                       (.preventDefault e)
                       (floor-draw-events/run-program ui-channel
                                                      (floor-draw-events/draw-start
                                                       (- (.-clientX e) @x-bcr)
                                                       (- (.-clientY e) @y-bcr))))
      :on-mouse-up   (fn [e]
                       (.preventDefault e)
                       (floor-draw-events/run-program ui-channel
                                                      (floor-draw-events/draw-poly)))

      :on-mouse-move (fn [e]
                       (.preventDefault e)
                       (floor-draw-events/run-program ui-channel
                                                      (floor-draw-events/draw-line
                                                       (- (.-clientX e) @x-bcr)
                                                       (- (.-clientY e) @y-bcr))))}
     common-data/filters

     (common-data/snap-lines-horizontal)
     (common-data/snap-lines-vertical)
     [:g
      (when-not (empty? figures)
        (floor-common/draw-figures figures opacity nil))
      (when-not (empty? polyline)
        (apply floor-components/polyline "lines" {:style {:fill "none" :stroke "black"}} polyline))
      (when shadow
        [:g
         (apply floor-components/polyline "lines" {:style {:fill "none" :stroke "blue"}} shadow-raw)
         (apply floor-components/polyline "lines" {:style {:fill "none" :stroke "brown"}} shadow-polyline)])
      (when-not (empty? circle)
        (floor-components/circle center r circle-point-style true nil))
      (when-not (empty? cut-poly)
        (apply floor-components/polyline "lines" {:style {:stroke "red"}} cut-poly))
      (when-not (empty? cut-line)
        (apply floor-components/polyline "lines" {:style {:stroke "red"}} cut-line))
      (if (= pen :down)
        [:g
         (floor-components/color-line (line-color (:line-angle turtle)) line {})
         (when (not-empty snap-points)
           (draw-snap-points snap-points line connection-point-style))
         (when (:end turtle)
           [:g
            (floor-components/circle (first polyline) 0 end-point-style true nil)])]
        [:g
         (floor-components/circle (first polyline) 0 start-point-style true nil)
         (floor-components/circle (last polyline) 0 new-point-style true nil)])]]))


;(defcard-rg floor-plan
;            (fn [app _] [draw-floor])
;            s/data
;            {:component-did-mount (events/listen js/window EventType.MOUSEUP (eev/mouse-up))})
;             ;:inspect-data true})

