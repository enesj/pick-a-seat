(ns pickaseat.ver01.floor-map.draw
  (:require
    ;[devcards.core]
    [pickaseat.ver01.floor-map.pixie :as p]
    [pickaseat.ver01.floor-map.components :as comps]
    [pickaseat.ver01.floor-map.settings :as s]
    [complex.number :as n]
    [complex.geometry :as g]
    [reagent.core :as reagent]
    [devtools.core :as devtools]
    [devtools.toolbox :as toolbox]
    [pickaseat.ver01.floor-map.edit-events :as eev]
    [pickaseat.ver01.floor-map.DrawCommands :as dc]
    [goog.events :as events]
    [cljs.core.async :as async :refer [>! <! put! chan alts! timeout]]
    [pickaseat.ver01.data.common-data :as cd]
    [pickaseat.ver01.tables.tables-core :as tc]
    [pickaseat.ver01.data.table_data :as td]
    [reagent.core :as r]
    [pickaseat.ver01.tables.themes :as t])
  (:import [goog.events EventType])
  (:require-macros
    ;[devcards.core :as dc :refer [defcard deftest defcard-rg defcard-doc]]
    [cljs.core.async.macros :refer [go]]))


(enable-console-print!)
(devtools/install! [:formatters])

(def filters
  [:defs
   [:filter {:id "s1"}
    [:feGaussianBlur {:in "SourceAlpha" :result "blurOut" :stdDeviation "2"}]
    [:feOffset {:in "blurOut" :result "offsetBlurOut" :dx 2 :dy 2}]
    [:feMerge
     [:feMergeNode {:in "offsetBlurOut"}]
     [:feMergeNode {:in "SourceGraphic"}]]]
   [:filter {:id "s2"}
    [:feOffset :dx "20" :dy "20"]]])

(defn line-color [angle]
  (condp (fn [a b] (zero? (mod b a))) angle
    45 "black"
    30 "gray"
    ;15 "green"
    "lightgray"))

(defn get-bcr [svg-root]
  (-> svg-root
      reagent/dom-node
      .getBoundingClientRect))

(defn tables-back []
  (let [full-state @td/tables-state
        tables (:tables full-state)]
     [tc/root-preview full-state (r/cursor td/tables-state [:tables]) (for [table tables] (first table))]))

(defn draw-floor []
  (let [data s/data
        {:keys [start-point-style end-point-style connection-point-style opacity]} s/base-settings
        {:keys [turtle base resolution figures mode selection]} @data
        {:keys [snap-points line-angle line polyline pen cut-poly cut-line]} turtle
        common-data @cd/common-data
        ui-channel (chan)
        _ (dc/process-channel ui-channel data)
        x-bcr (atom 0)
        y-bcr (atom 0)
        xx-bcr @x-bcr]
    [:div {:style {:font-size "20px" :margin-top "-20px"}}
     [:div {:style {:padding-left "5%"}} "Velika Sala"]
     (dc/draw-menu data ui-channel)
     [:svg
      {
       :width         (:w common-data)
       :height        (:h common-data)
       :ref           #(when %
                         (reset! x-bcr (.-left (.getBoundingClientRect %)))
                         (reset! y-bcr (.-top (.getBoundingClientRect %))))
       :on-mouse-down (fn [e]
                        (.preventDefault e)
                        (dc/run-program ui-channel
                                     (dc/draw-start (- (.-clientX e) @x-bcr)
                                                 (- (.-clientY e) @y-bcr))))
       :on-mouse-up   (fn [e]
                        (.preventDefault e)
                        (dc/run-program ui-channel
                                     (dc/draw-poly (- (.-clientX e) @x-bcr)
                                                (- (.-clientY e) @y-bcr))))
       :on-mouse-move (fn [e]
                        (.preventDefault e)
                        (dc/run-program ui-channel
                                     (dc/draw-line
                                       (- (.-clientX e) @x-bcr)
                                       (- (.-clientY e) @y-bcr)
                                       (if (not-empty snap-points) true false))))}
      filters
      [:g
       (when-not (empty? figures)
         (dc/draw-figures figures polyline opacity))
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
            (dc/draw-snap-points snap-points line connection-point-style))
          (when (:end turtle)
            [:g
             (comps/circle (first polyline) 0 end-point-style)
             (comps/circle (first polyline) 0 connection-point-style)
             (comps/circle (last line) 0 connection-point-style)])]
         [:g
          (comps/circle (last polyline) 0 start-point-style)
          (comps/circle (last polyline) 0 connection-point-style)])
       (when (:tables @data) [tables-back])]]]))


;(defcard-rg floor-plan
;            (fn [app _] [draw-floor])
;            s/data
;            {:component-did-mount (events/listen js/window EventType.MOUSEUP (eev/mouse-up))})
;             ;:inspect-data true})

