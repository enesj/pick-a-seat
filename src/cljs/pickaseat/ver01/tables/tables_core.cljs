(ns pickaseat.ver01.tables.tables-core
  (:require
    [reagent.core :as r]
    [pickaseat.ver01.tables.tables-components :as c]
    [pickaseat.ver01.data.table_data :as td]
    [pickaseat.ver01.tables.util :as u]
    [pickaseat.ver01.tables.table-events :as tev]
    [pickaseat.ver01.tables.table-actions :as ta]
    [pickaseat.ver01.data.common-data :as cd]
    [pickaseat.ver01.tables.themes :as t]))

(defn move-tables []
  (fn [sel-top-lefts] (ta/move-table sel-top-lefts)))

(defn root [state tables ids]
  [:g
   (doall (for [id ids]
            ^{:key id} [c/table {:on-drag (move-tables)} (r/cursor tables [id])]))
   (if (:show (:selection state))
     [(c/selection-rect (move-tables) state)])])

(defn root-preview [state tables ids]
  [:g {:opacity "0.4"}
   (doall (for [id ids]
            ^{:key id} [c/table {:on-drag nil} (r/cursor tables [id])]))])



(defn draw-tables []
  (let [tables-state @td/tables-state
        selection (:selection tables-state)
        tables (:tables tables-state)
        common-data @cd/common-data
        [x y] (mapv - (:svg common-data))
        [[x-sel-s y-sel-s] [x-sel-e y-sel-e]] (u/start-end (:start selection) (:end selection))
        {:keys [key-down key-up mouse-down mouse-move]} (tev/table-events tables-state selection tables x y x-sel-s y-sel-s x-sel-e y-sel-e)
        root [root tables-state (r/cursor td/tables-state [:tables]) (for [table tables] (first table))]]
    ;{:keys [w h]} (:window @td/settings-base)]
    [:div {:style {:font-size "20px" :margin-top "-20px"}}
     [:div {:style {:padding-left "5%"}} "Velika Sala"]
     [:svg
      {:fill          (:text t/palete)
       :width         (:w common-data)
       :height        (:h common-data)
       :on-key-down   key-down
       :on-key-up     key-up
       :on-mouse-down mouse-down
       :on-mouse-move mouse-move}
      root]]))