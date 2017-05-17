(ns pickaseat.ver01.floor-map.floor-draw
  (:require
    [pickaseat.ver01.floor-map.floor-components :as comps]
    [pickaseat.ver01.data.floor-data :as fd]
    [reagent.core :as r]
    [pickaseat.ver01.floor-map.floor-draw-events :as de]
    [cljs.core.async :as async :refer [chan]]
    [pickaseat.ver01.data.common-data :as cd]
    [pickaseat.ver01.data.table_data :as td]
    [pickaseat.ver01.tables.tables-components :as c]))


(enable-console-print!)

(defn line-color [angle]
  (condp (fn [a b] (zero? (mod b a))) angle
    45 "black"
    30 "gray"
    ;15 "green"
    "lightgray"))

(defn get-bcr [svg-root]
  (-> svg-root
      r/dom-node
      .getBoundingClientRect))

(defn root-preview [ tables ids]
  [:g {:opacity "0.4"}
   (doall (for [id ids]
            ^{:key id} [c/table {:on-drag nil} (r/cursor tables [id])]))])

(defn tables-back []
  (let [full-state @td/tables-state
        tables (:tables full-state)]
     [root-preview (r/cursor td/tables-state [:tables]) (for [table tables] (first table))]))

(defn draw-figures [figures polyline opacity]
  (for [figure (sort-by key figures)]
    (let [fig (first (val figure))
          opacity (if (> (count polyline) 1) (:low opacity) (:high polyline))]
      (case (key fig)
        :polygon (comps/polygon
                   {:key     (key figure)
                    :stroke  "black"
                    :fill    "white"
                    :opacity opacity
                    :filter  "url(#s1)"}
                   ;{:key       (rand 1000)
                   ; :stroke    "black"
                   ; :fill      "white"
                   ; :opacity   opacity
                   ; :transform "translate(0 0)"}
                   (val fig))))))


(defn draw-snap-points [snap-points line connection-point-style]
  [:g
   (comps/circle (last line) 0 connection-point-style)
   (for [snap-point snap-points]
     [:g {:key (rand 1000)}
      (comps/circle snap-point 1 connection-point-style)
      (comps/color-line "orange" [snap-point (last line)] {:stroke-dasharray "5, 5"})])])




(defn draw-svg [start-point-style end-point-style connection-point-style opacity turtle figures snap-points line polyline
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
      (draw-figures figures polyline opacity))
    (when-not (empty? polyline)
      (apply comps/polyline "lines" {:style {:fill "none" :stroke "black"}} polyline))
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
          (comps/circle (first polyline) 0 end-point-style)
          (comps/circle (first polyline) 0 connection-point-style)
          (comps/circle (last line) 0 connection-point-style)])]
      [:g
       (comps/circle (last polyline) 0 start-point-style)
       (comps/circle (last polyline) 0 connection-point-style)])]])
    ;(when (:tables @data) [tables-back])]])




;(defcard-rg floor-plan
;            (fn [app _] [draw-floor])
;            s/data
;            {:component-did-mount (events/listen js/window EventType.MOUSEUP (eev/mouse-up))})
;             ;:inspect-data true})

