(ns pickaseat.ver01.tables.tables-core
  (:require
    [reagent.core :as r]
    [pickaseat.ver01.tables.tables-components :as tables-components]
    [pickaseat.ver01.data.table_data :as table-data]
    [pickaseat.ver01.tables.table-utils :as table-utils]
    [pickaseat.ver01.tables.table-events :as table-events]
    [pickaseat.ver01.tables.table-actions :as table-actions]
    [pickaseat.ver01.data.common :as common]
    [pickaseat.ver01.data.themes :as themes]
    [pickaseat.ver01.data.floor-data :as floor-data]
    [pickaseat.ver01.floor-map.floor-components :as floor-components]
    [pickaseat.ver01.data.background :as background]))


(defn tables-menu []
  (let [{:keys [layout performed recalled]} @table-data/history
        undo? (not-empty (rest performed))
        redo? (not-empty recalled)]
    [:svg {:width "400px" :height "30px" :font-family "Courier New" :fill "blue" :font-size "15"}
     [:text {:opacity     (if undo? 0.8 0.1)
             :on-mouse-up (fn [e]
                            (when undo?
                              (.preventDefault e)
                              (.stopPropagation e)
                              (table-events/undo)))
             :x           90 :y 20} (str "Undo " (count performed))]
     [:text {:opacity     (if redo? 0.8 0.1)
             :on-mouse-up (fn [e]
                            (when redo?
                              (.preventDefault e)
                              (.stopPropagation e)
                              (table-events/redo)))
             :x           160 :y 20} (str "Redo " (count recalled))]
     [:text {:on-mouse-down (fn [e] (.preventDefault e)
                              (swap! table-data/history update-in [:layout] not))
             :x             240 :y 20} (if layout "hide(layout)" "show(layout)")]]))

(defn draw-tables [state tables ids svg-root]
  (let [move-tables (fn [selected-tables] (table-actions/move-table selected-tables))]
    [:g
     (doall (for [id ids]
              ^{:key id} [tables-components/table (r/cursor tables [id]) move-tables nil]))
     (if (:show (:selection state))
       [(tables-components/selection-rect move-tables state)])]))

(defn tables-svg []
  (let [tables-state @table-data/tables-state
        {:keys [tables selection]} tables-state
        common-data @common/data
        {:keys [w h]} common-data
        {:keys [key-down key-up mouse-down mouse-move]} (table-events/table-events selection tables (r/current-component))
        tables-root [draw-tables tables-state (r/cursor table-data/tables-state [:tables]) (for [table tables] (first table)) (r/current-component)]]
    [:svg
     {:fill          (:text themes/palete)
      :width         w
      :height        h
      :on-key-down   key-down
      :on-key-up     key-up
      :on-mouse-down mouse-down
      :on-mouse-move mouse-move
      :style         {:background-color "rgb(235,242,230)"}}
     ;floor-data/filters
     (background/snap-lines-horizontal :snap-tables)
     (background/snap-lines-vertical :snap-tables)
     (if (:layout @table-data/history)
       (floor-components/draw-figures (:figures @floor-data/floor-state) :low nil nil))
     ;[:rect {:x 5 :y 5 :width (- w 10) :height (- h 10) :filter  "url(#s1)" :style {:stroke "black" :fill "none"}}]
     tables-root]))

(defn tables []
  (let []
    [:div {:style {:font-size "20px" :margin-top "-20px"}}
     [tables-menu]
     [tables-svg]]))
