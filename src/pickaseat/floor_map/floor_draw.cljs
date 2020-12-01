(ns pickaseat.floor-map.floor-draw
  (:require
    [pickaseat.floor-map.floor-components :as floor-components]
    [reagent.core :as r]
    [pickaseat.floor-map.floor-draw-events :as floor-draw-events]
    [cljs.core.async :as async :refer [chan]]
    [pickaseat.data.common :as common]
    [pickaseat.data.background :as background]
    [pickaseat.tables.tables-components :as tables-components]
    [pickaseat.floor-map.floor-common :as floor-common]
    [pickaseat.data.floor-data :as floor-data]))


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
       ^{:key id} [tables-components/table (r/cursor tables [id]) nil nil]))])

(defn draw-snap-points [snap-points line connection-point-style]
  [:g
   (floor-components/circle (last line) 0 connection-point-style false nil)
   (for [snap-point snap-points]
     [:g {:key (rand 1000)}
      (floor-components/circle snap-point 1 connection-point-style false nil)
      (floor-components/color-line "orange" [snap-point (last line)] {:stroke-dasharray "5, 5"})])])



(defn draw-floor [ turtle figures snap-points line opacity-mode circle
                  shadow-raw  shadow-polyline shadow polyline pen cut-poly cut-line  ui-channel]
  (let [[center r] circle
        commn-data  @common/data
        {:keys [new-point-style start-point-style end-point-style connection-point-style circle-point-style opacity]} floor-data/base-settings]
    [:svg
     {
      :style         {:background-color (:grid-back-color commn-data)}
      :width         (:w commn-data)
      :height        (:h commn-data)
      :ref           #(when %
                        (swap! common/data assoc-in [:bcr-layout] [(.-left (.getBoundingClientRect %)) (.-top (.getBoundingClientRect %))]))
      :on-mouse-down (fn [e]
                       (.preventDefault e)
                       (let [[x-bcr y-bcr] (:bcr-layout @common/data)]
                         (floor-draw-events/run-program ui-channel
                                                        (floor-draw-events/draw-start
                                                          (- (.-clientX e) x-bcr)
                                                          (- (.-clientY e) y-bcr)))))
      :on-mouse-up   (fn [e]
                       (.preventDefault e)
                       (floor-draw-events/run-program ui-channel
                                                      (floor-draw-events/draw-figure)))

      :on-mouse-move (fn [e]
                       (.preventDefault e)
                       (let [[x-bcr y-bcr] (:bcr-layout @common/data)]
                         (floor-draw-events/run-program ui-channel
                                                        (floor-draw-events/draw-line
                                                          (- (.-clientX e) x-bcr)
                                                          (- (.-clientY e) y-bcr)))))}
     ;floor-data/filters

     (background/snap-lines-horizontal :snap-layout)
     (background/snap-lines-vertical :snap-layout)
     [:g
      (when (seq figures) (floor-components/draw-figures figures opacity-mode nil nil))
      (when (seq polyline) (apply floor-components/polyline "lines" {:style {:stroke "black", :fill "none"}} polyline))
      (when shadow
        [:g
         (apply floor-components/polyline "lines" {:style {:fill "none" :stroke "blue"}} shadow-raw)
         (apply floor-components/polyline "lines" {:style {:fill "none" :stroke "brown"}} shadow-polyline)])
      (when (seq circle) (floor-components/circle center r circle-point-style true nil))
      (when (seq cut-poly) (apply floor-components/polyline "lines" {:style {:stroke "red"}} cut-poly))
      (when (seq cut-line) (apply floor-components/polyline "lines" {:style {:stroke "red"}} cut-line))
      (if (= pen :down)
        [:g
         (when (every? pos? (second line))
           (floor-components/color-line (line-color (:line-angle turtle)) line {}))
         (when (not-empty snap-points)
           (draw-snap-points snap-points line connection-point-style))
         (when (:end turtle)
           [:g
            (floor-components/circle (first polyline) 0 end-point-style true nil)])]
        [:g
         (floor-components/circle (first polyline) 0 start-point-style true nil)
         (floor-components/circle (last polyline) 0 new-point-style true nil)])]]))

