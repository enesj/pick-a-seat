(ns pickaseat.ver01.tables.tables-core
  (:require
    [reagent.core :as r]
    [pickaseat.ver01.tables.tables-components :as tables-components]
    [pickaseat.ver01.data.table_data :as table-data]
    [pickaseat.ver01.tables.table-utils :as table-utils]
    [pickaseat.ver01.tables.table-events :as table-events]
    [pickaseat.ver01.tables.table-actions :as table-actions]
    [pickaseat.ver01.data.common-data :as common-data]
    [pickaseat.ver01.data.themes :as themes]
    [pickaseat.ver01.data.floor-data :as floor-data]
    [pickaseat.ver01.floor-map.floor-common :as floor-common]
    [pickaseat.ver01.floor-map.floor-components :as floor-components]))



(defn root [state tables ids]
  (let [move-tables (fn [sel-top-lefts] (table-actions/move-table sel-top-lefts))]
    [:g
     (doall (for [id ids]
              ^{:key id} [tables-components/table move-tables (r/cursor tables [id])]))
     (if (:show (:selection state))
       [(tables-components/selection-rect move-tables state)])]))


(defn undo []
  (let [history @table-data/history
        {:keys [performed recalled]} history
        butlast-performed (vec (butlast performed))]
    (when (pos? (count performed))
      (reset! table-data/history {:recalled (vec (conj recalled (last performed))), :performed butlast-performed, :layout (:layout (deref table-data/history))})
      (reset! table-data/tables-state (last butlast-performed)) (table-data/settings-pos (* (/ (.-innerWidth js/window) 1000) (.-devicePixelRatio js/window)) false))))


(defn redo []
  (let [history @table-data/history
        {:keys [performed recalled]} history
        butlast-recalled (vec (butlast recalled))]
    (when (pos? (count recalled))
      (reset! table-data/history {:recalled butlast-recalled, :layout (:layout (deref table-data/history)), :performed (vec (conj performed (last recalled)))})
      (reset! table-data/tables-state (last recalled)) (table-data/settings-pos (* (/ (.-innerWidth js/window) 1000) (.-devicePixelRatio js/window)) false))))


(defn draw-menu []
  (let [{:keys [layout performed recalled]} @table-data/history
        undo? (not-empty (rest performed))
        redo? (not-empty recalled)]
    [:svg {:width "400px" :height "30px" :font-family "Courier New" :fill "blue" :font-size "15"}
     [:text {:opacity     (if undo? 0.8 0.1)
             :on-mouse-up (fn [e]
                            (when undo?
                              (.preventDefault e)
                              (.stopPropagation e)
                              (undo)))
             :x           90 :y 20} (str "Undo " (count performed))]
     [:text {:opacity     (if redo? 0.8 0.1)
             :on-mouse-up (fn [e]
                            (when redo?
                              (.preventDefault e)
                              (.stopPropagation e)
                              (redo)))
             :x           160 :y 20} (str "Redo " (count recalled))]
     [:text {:on-mouse-down (fn [e] (.preventDefault e)
                              (swap! table-data/history update-in [:layout] not))
             :x             240 :y 20} (if layout "hide(layout)" "show(layout)")]]))



(defn draw-tables []
  (let [tables-state @table-data/tables-state
        {:keys [tables selection]} tables-state
        common-data @common-data/data
        [x y] (mapv - (:svg common-data))
        {:keys [w h]} common-data
        [[x-sel-s y-sel-s] [x-sel-e y-sel-e]] (table-utils/start-end (:start selection) (:end selection))
        {:keys [key-down key-up mouse-down mouse-move]} (table-events/table-events selection tables x y x-sel-s y-sel-s x-sel-e y-sel-e)
        tables-root [root tables-state (r/cursor table-data/tables-state [:tables]) (for [table tables] (first table))]]

    [:div {:style {:font-size "20px" :margin-top "-20px"}}
     [draw-menu]
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
      (if (:layout @table-data/history)
        (floor-components/draw-figures (:figures @floor-data/floor-state) :low nil nil))
      ;[:rect {:x 5 :y 5 :width (- w 10) :height (- h 10) :filter  "url(#s1)" :style {:stroke "black" :fill "none"}}]
      tables-root]]))
