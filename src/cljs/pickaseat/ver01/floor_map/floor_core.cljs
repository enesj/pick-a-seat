(ns pickaseat.ver01.floor-map.floor-core
  (:require
    [reagent.core :as r]
    [pickaseat.ver01.data.floor-data :as floor-data]
    [pickaseat.ver01.data.common-data :as common-data]
    [cljs.core.async :as async :refer [chan]]
    [pickaseat.ver01.floor-map.floor-draw-events :as draw-events]
    [pickaseat.ver01.floor-map.floor-draw :as floor-draw]
    [pickaseat.ver01.floor-map.floor-edit :as floor-edit]
    [pickaseat.ver01.data.table-data :as table-data]
    [pickaseat.ver01.tables.tables-components :as tables-components]))


(enable-console-print!)

(defn root-preview [ tables ids]
  [:g {:opacity "0.4"}
   (doall (for [id ids]
            ^{:key id} [tables-components/table nil (r/cursor tables [id])]))])

(defn tables-back []
  (let [full-state @table-data/tables-state
        tables (:tables full-state)]
    [root-preview (r/cursor table-data/tables-state [:tables]) (for [table tables] (first table))]))

(defn draw-menu [app-state ui-channel mode draw-circle?]
  (let [{:keys [ tables]}  @floor-data/floor-state
        {:keys [ performed recalled]} @floor-data/floor-states-data
        undo? (not-empty (rest performed))
        redo? (not-empty recalled)]
    ;(js/console.log mode  @(mode fd/floor-state))
    [:svg {:width "600px" :height "30px" :font-family "Courier New" :fill "blue" :font-size "15"}
     [:text {:opacity       0.8
             :on-mouse-down (fn [e] (.preventDefault e)
                              (swap! app-state assoc-in [:mode]
                                     (if (= mode :drawing) :editing :drawing)))
                              ;(reset! (:history fd/floor-state) {:performed [@app-state] :recalled []}))
             :x             10 :y 20}
      (name mode)]
     [:text {:opacity     (if undo? 0.8 0.1)
             :on-mouse-up (fn [e]
                            (when undo?
                              (.preventDefault e)
                              (.stopPropagation e)
                              (draw-events/run-program ui-channel (draw-events/undo))))
             :x           90 :y 20} (str "Undo " (count performed))]
     [:text {:opacity     (if redo? 0.8 0.1)
             :on-mouse-up (fn [e]
                            (when redo?
                              (.preventDefault e)
                              (.stopPropagation e)
                              (draw-events/run-program ui-channel (draw-events/redo))))
             :x           160 :y 20} (str "Redo " (count recalled))]
     [:text {:on-mouse-down (fn [e] (.preventDefault e)
                              (swap!  floor-data/floor-state update-in [:tables] not))
             :x            240 :y 20} (if tables "hide(tables)" "show(tables)")]
     (when (= mode :drawing)
       [:text {:opacity       0.8
               :on-mouse-down (fn [e] (.preventDefault e)
                                (swap! app-state update-in [:turtle :draw-circle?] not)
                                (swap! app-state assoc-in [:turtle :circle] [])
                                (swap! app-state assoc-in [:turtle :poly] [])
                                (swap! app-state assoc-in [:turtle :line] []))
               :x             380 :y 20}
        (if draw-circle? "circle" "poly")])]))


(defn draw-floor []
  (let [data floor-data/floor-state
        {:keys [new-point-style start-point-style end-point-style connection-point-style circle-point-style opacity]} floor-data/base-settings
        {:keys [mode turtle figures]} @data
        {:keys [snap-points line shadow-raw shadow-polyline shadow? polyline circle pen cut-poly cut-line draw-circle?]} turtle
        opacity (if (and (> (count polyline) 1) (= mode :drawing) ) (:low opacity) (:high opacity))
        common-data @common-data/data
        {:keys [w h]} common-data
        ui-channel (chan)
        _ (draw-events/process-channel ui-channel data)
        x-bcr (atom 0)
        y-bcr (atom 0)
        svg (if (= mode :drawing)
              (floor-draw/draw-svg new-point-style start-point-style end-point-style connection-point-style circle-point-style circle opacity turtle figures snap-points line
                                   shadow-raw shadow-polyline shadow? polyline pen cut-poly cut-line common-data ui-channel x-bcr y-bcr data)
              (floor-edit/edit-svg figures common-data opacity))]
    ;(js/console.log opacity)
    ;svg (conj svg-1 [:rect {:x 5 :y 5 :width (- w 10) :height (- h 10) :filter  "url(#s1)" :style {:stroke "black" :fill "none"}}])]
    [:div {:style {:font-size "20px" :margin-top "-20px"}}
     (draw-menu data ui-channel mode draw-circle?)
     (if (:tables  @floor-data/floor-state) (conj svg (tables-back)) svg)]))