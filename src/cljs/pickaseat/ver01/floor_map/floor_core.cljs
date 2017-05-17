(ns pickaseat.ver01.floor-map.floor-core
  (:require
    [reagent.core :as r]
    [pickaseat.ver01.data.floor-data :as fd]
    [pickaseat.ver01.data.common-data :as cd]
    [cljs.core.async :as async :refer [chan]]
    [pickaseat.ver01.floor-map.floor-draw-events :as de]
    [pickaseat.ver01.floor-map.floor-draw :as draw]
    [pickaseat.ver01.floor-map.floor-edit :as edit]
    [pickaseat.ver01.data.table_data :as td]
    [pickaseat.ver01.tables.tables-components :as c]))


(enable-console-print!)

(defn root-preview [ tables ids]
  [:g {:opacity "0.4"}
   (doall (for [id ids]
            ^{:key id} [c/table {:on-drag nil} (r/cursor tables [id])]))])

(defn tables-back []
  (let [full-state @td/tables-state
        tables (:tables full-state)]
    [root-preview (r/cursor td/tables-state [:tables]) (for [table tables] (first table))]))

(defn draw-menu [app-state ui-channel mode]
  (let [{:keys [tables performed recalled]} @fd/history
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
                              (swap! fd/history update-in [:tables] not))
             :x             240 :y 20} (if tables "hide(tables)" "show(tables)")]]))

(defn draw-floor []
  (let [data fd/data
        {:keys [start-point-style end-point-style connection-point-style opacity]} fd/base-settings
        {:keys [mode turtle figures]} @data
        {:keys [snap-points line polyline pen cut-poly cut-line]} turtle
        common-data @cd/common-data
        ui-channel (chan)
        _ (de/process-channel ui-channel data)
        x-bcr (atom 0)
        y-bcr (atom 0)
        svg (if (= mode :drawing)
              (draw/draw-svg start-point-style end-point-style connection-point-style opacity turtle figures snap-points line polyline
                             pen cut-poly cut-line common-data ui-channel x-bcr y-bcr data)
              (edit/edit-svg start-point-style end-point-style connection-point-style opacity turtle figures snap-points line polyline
                             pen cut-poly cut-line common-data ui-channel x-bcr y-bcr data))]
    [:div {:style {:font-size "20px" :margin-top "-20px"}}
     (draw-menu data ui-channel mode)
     (if (:tables @fd/history) (conj svg (tables-back)) svg)]))