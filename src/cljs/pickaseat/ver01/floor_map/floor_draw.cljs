(ns pickaseat.ver01.floor-map.floor-draw
  (:require
    [pickaseat.ver01.floor-map.floor-components :as comps]
    [pickaseat.ver01.data.floor-data :as fd]
    [reagent.core :as r]
    [pickaseat.ver01.floor-map.floor-draw-events :as de]
    [cljs.core.async :as async :refer [chan]]
    [pickaseat.ver01.data.common-data :as cd]
    [pickaseat.ver01.data.table_data :as td]
    [pickaseat.ver01.tables.tables-components :as c]
    [pickaseat.ver01.floor-map.floor-common :as f-common]))


(enable-console-print!)

(defn line-color [angle]
  (condp (fn [a b] (zero? (mod b a))) angle
    45 "black"
    30 "gray"
    ;15 "green"
    "lightgray"))

(defn root-preview [tables ids]
  [:g {:opacity "0.4"}
   (doall
     (for [id ids]
       ^{:key id} [c/table {:on-drag nil} (r/cursor tables [id])]))])

(defn draw-snap-points [snap-points line connection-point-style]
  [:g
   (comps/circle (last line) 0 connection-point-style false nil)
   (for [snap-point snap-points]
     [:g {:key (rand 1000)}
      (comps/circle snap-point 1 connection-point-style false nil)
      (comps/color-line "orange" [snap-point (last line)] {:stroke-dasharray "5, 5"})])])

(defn draw-svg [new-point-style start-point-style end-point-style connection-point-style circle-point-style circle opacity turtle figures snap-points line
                shadow-raw  shadow-polyline shadow polyline pen cut-poly cut-line common-data ui-channel x-bcr y-bcr data]
  (let [[center r] circle]
    [:svg
     {
      :style {:background-color (:grid-back-color @cd/data)}
      :width         (:w common-data)
      :height        (:h common-data)
      :ref           #(when %
                        (reset! x-bcr (.-left (.getBoundingClientRect %)))
                        (reset! y-bcr (.-top (.getBoundingClientRect %))))
      :on-mouse-down (fn [e]
                       (.preventDefault e)
                       (de/run-program ui-channel
                                       (de/draw-start
                                         (- (.-clientX e) @x-bcr)
                                         (- (.-clientY e) @y-bcr))))
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

     (cd/snap-lines-horizontal)
     (cd/snap-lines-vertical)
     [:g
      (when-not (empty? figures)
        (f-common/draw-figures figures opacity nil))
      (when-not (empty? polyline)
        (apply comps/polyline "lines" {:style {:fill "none" :stroke "black"}} polyline))
      (when shadow
        [:g
         (apply comps/polyline "lines" {:style {:fill "none" :stroke "blue"}} shadow-raw)
         (apply comps/polyline "lines" {:style {:fill "none" :stroke "brown"}} shadow-polyline)])
      (when-not (empty? circle)
         (comps/circle center r circle-point-style true nil))
      (when-not (empty? cut-poly)
        (apply comps/polyline "lines" {:style {:stroke "red"}} cut-poly))
      (when-not (empty? cut-line)
        (apply comps/polyline "lines" {:style {:stroke "red"}} cut-line))
      (if (= pen :down)
        [:g
         (comps/color-line (line-color (:line-angle turtle)) line {})
         (when (not-empty snap-points)
           (draw-snap-points snap-points line connection-point-style))
         (when (:end turtle)
           [:g
            (comps/circle (first polyline) 0 end-point-style true nil)])]
        [:g
         (comps/circle (first polyline) 0 start-point-style true nil)
         (comps/circle (last polyline) 0 new-point-style true nil)])]]))


;(defcard-rg floor-plan
;            (fn [app _] [draw-floor])
;            s/data
;            {:component-did-mount (events/listen js/window EventType.MOUSEUP (eev/mouse-up))})
;             ;:inspect-data true})

