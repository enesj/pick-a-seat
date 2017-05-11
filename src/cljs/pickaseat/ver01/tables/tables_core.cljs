(ns pickaseat.ver01.tables.tables-core
  (:require
    [reagent.core :as r]
    [pickaseat.ver01.tables.tables-components :as c]
    [pickaseat.ver01.data.table_data :as td]
    [pickaseat.ver01.tables.util :as u]
    [pickaseat.ver01.tables.table-events :as tev]
    [pickaseat.ver01.tables.table-actions :as ta]
    [pickaseat.ver01.data.common-data :as cd]))

(defn move-tables []
  (fn [sel-top-lefts] (ta/move-table sel-top-lefts)))

(defn root [state tables ids]
  [:g
   (doall (for [id ids]
            ^{:key id} [c/table {:on-drag (move-tables)} (r/cursor tables [id])]))
   (if (:show (:selection state))
     [(c/selection-rect (move-tables) state)])])

(defn table-mode []
  (let [tables-state @td/tables-state
        selection (:selection tables-state)
        tables (:tables tables-state)
        [x y] (mapv - (:svg @cd/common-data))
        [[x-sel-s y-sel-s] [x-sel-e y-sel-e]] (u/start-end (:start selection) (:end selection))
        root [root tables-state (r/cursor td/tables-state [:tables]) (for [table tables] (first table))]]
        ;{:keys [w h]} (:window @td/settings-base)]
    {:events (tev/table-events tables-state selection tables x y x-sel-s y-sel-s x-sel-e y-sel-e)
     :root root}))