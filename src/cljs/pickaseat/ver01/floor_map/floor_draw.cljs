(ns pickaseat.ver01.floor-map.floor-draw
  (:require
    ;[devcards.core]
    [pickaseat.ver01.floor-map.floor-components :as comps]
    [pickaseat.ver01.data.floor-data :as fd]
    [reagent.core :as r]
    [pickaseat.ver01.floor-map.floor-draw-events :as de]
    [cljs.core.async :as async :refer [>! <! put! chan alts! timeout]]
    [pickaseat.ver01.data.common-data :as cd]
    ;[pickaseat.ver01.tables.tables-core :as tc]
    [pickaseat.ver01.data.table_data :as td]
    [pickaseat.ver01.tables.tables-components :as c])
  (:require-macros
    [cljs.core.async.macros :refer [go]]))



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

(defn draw-menu [app-state ui-channel]
  (let [{:keys [mode ]}  @app-state
        {:keys [tables performed recalled]} @fd/history
        undo? (not-empty (rest performed))
        redo? (not-empty recalled)]
    [:svg {:width "400px" :height "30px" :font-family "Courier New" :fill "blue" :font-size "15"}
     [:text {:opacity       0.8

             :on-mouse-down (fn [e] (.preventDefault e)
                              (swap! app-state assoc-in [:mode]
                                     (if (= mode :drawing) :editing :drawing)))
             :x             10 :y 20}
      (name mode)]
     [:text {:opacity     (if undo? 0.8 0.1)
             :on-mouse-up (fn [e]
                            (when undo?
                              (.preventDefault e)
                              (.stopPropagation e)
                              (de/run-program ui-channel (de/undo))))
             :x           90 :y 20} (str "Undo " (count performed))]
     [:text {:opacity     (if redo? 0.8 0.1)
             :on-mouse-up (fn [e]
                            (when redo?
                              (.preventDefault e)
                              (.stopPropagation e)
                              (de/run-program ui-channel (de/redo))))
             :x           160 :y 20} (str "Redo " (count recalled))]
     [:text {:on-mouse-down (fn [e] (.preventDefault e)
                              (swap! app-state update-in [:tables] not))
             :x             240 :y 20} (if tables "hide(tables)" "show(tables)")]]))

(defn draw-floor []
  (let [data fd/data
        {:keys [start-point-style end-point-style connection-point-style opacity]} fd/base-settings
        {:keys [turtle figures]} @data
        {:keys [snap-points line polyline pen cut-poly cut-line]} turtle
        common-data @cd/common-data
        ui-channel (chan)
        _ (de/process-channel ui-channel data)
        x-bcr (atom 0)
        y-bcr (atom 0)]
    [:div {:style {:font-size "20px" :margin-top "-20px"}}
     (draw-menu data ui-channel)
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
          (comps/circle (last polyline) 0 connection-point-style)])
       (when (:tables @data) [tables-back])]]]))


;(defcard-rg floor-plan
;            (fn [app _] [draw-floor])
;            s/data
;            {:component-did-mount (events/listen js/window EventType.MOUSEUP (eev/mouse-up))})
;             ;:inspect-data true})

