(ns pickaseat.ver01.tables.tables-core
  (:require
    [reagent.core :as r]
    [pickaseat.ver01.tables.tables-components :as c]
    [pickaseat.ver01.data.table_data :as td]
    [pickaseat.ver01.tables.table-utils :as u]
    [pickaseat.ver01.tables.table-events :as tev]
    [pickaseat.ver01.tables.table-actions :as ta]
    [pickaseat.ver01.data.common-data :as cd]
    [pickaseat.ver01.data.themes :as t]
    ;[pickaseat.ver01.floor-map.floor-draw :as floor]
    [pickaseat.ver01.floor-map.floor-components :as comps]
    [pickaseat.ver01.data.floor-data :as fd]
    [pickaseat.ver01.helper :as h]
    [pickaseat.ver01.floor-map.floor-common :as f-common]))



(defn root [state tables ids]
  (let [move-tables (fn [sel-top-lefts] (ta/move-table sel-top-lefts))]
    [:g
     (doall (for [id ids]
              ^{:key id} [c/table move-tables (r/cursor tables [id])]))
     (if (:show (:selection state))
       [(c/selection-rect move-tables state)])]))


(defn undo []
  (let [history @td/history
        {:keys [performed recalled]} history
        butlast-performed (vec (butlast performed))]
    (if (> (count performed) 0)
      (do
        (reset! td/history {:performed butlast-performed :recalled (vec (conj recalled (last performed))) :layout (:layout @td/history)})
        (reset! td/tables-state (last butlast-performed))
        (td/settings-pos (* (/ (.-innerWidth js/window) 1000) (.-devicePixelRatio js/window)) false)))))


(defn redo []
  (let [history @td/history
        {:keys [performed recalled]} history
        butlast-recalled (vec (butlast recalled))]
    (if (> (count recalled) 0)
      (do
        (reset! td/history {:performed (vec (conj performed (last recalled))) :recalled butlast-recalled :layout (:layout @td/history)})
        (reset! td/tables-state (last recalled))
        (td/settings-pos (* (/ (.-innerWidth js/window) 1000) (.-devicePixelRatio js/window)) false)))))


(defn draw-menu []
  (let [{:keys [layout performed recalled]} @td/history
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
                              (swap! td/history update-in [:layout] not))
             :x             240 :y 20} (if layout "hide(layout)" "show(layout)")]]))

(defn draw-figures []
  (for [figure (sort-by key (:figures @fd/data))]
    (let [fig (first (val figure))]
      (case (key fig)
        :polygon (comps/polygon
                   {:key     (key figure)
                    :stroke  "black"
                    :fill    "white"
                    :opacity 0.4
                    :filter  "url(#s1)"}
                   (val fig)
                   nil)))))


(defn draw-tables []
  (let [tables-state @td/tables-state
        {:keys [tables selection ]} tables-state
        common-data @cd/common-data
        [x y] (mapv - (:svg common-data))
        [[x-sel-s y-sel-s] [x-sel-e y-sel-e]] (u/start-end (:start selection) (:end selection))
        {:keys [key-down key-up mouse-down mouse-move]} (tev/table-events selection tables x y x-sel-s y-sel-s x-sel-e y-sel-e)
        root [root tables-state (r/cursor td/tables-state [:tables]) (for [table tables] (first table))]]
    [:div {:style {:font-size "20px" :margin-top "-20px"}}
     [draw-menu]
     [:svg
      {:fill          (:text t/palete)
       :width         (:w common-data)
       :height        (:h common-data)
       :on-key-down   key-down
       :on-key-up     key-up
       :on-mouse-down mouse-down
       :on-mouse-move mouse-move}
      cd/filters
      (if (:layout @td/history) (f-common/draw-figures (:figures @fd/data) (:low (:opacity @fd/data)) nil))
      root]]))
