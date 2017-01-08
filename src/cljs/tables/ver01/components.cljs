(ns tables.ver01.components
  (:use [com.rpl.specter :only [select transform setval FIRST LAST ALL keypath]])
  (:require [reagent.core :as r]
            [goog.events :as events]
            [tables.ver01.table_data :as td]
            [tables.ver01.selection-utils :as su])
  (:import [goog.events EventType]))

(defn drag-move-fn [on-drag start]
  (fn [evt]
    ;pageY (aget evt "event_" "pageY")  XY poyicija misa na strani
    ;pageX (aget evt "event_" "pageX")
    (on-drag (.-clientX evt) (.-clientY evt) start (:tables @td/tables-state) (.-ctrlKey evt))))

(defn drag-end-fn [drag-move]
  (fn [evt]
    (events/unlisten js/window EventType.MOUSEMOVE drag-move)
    (swap! td/tables-state update-in [:tables] (fn [x] (transform [ALL LAST] #(dissoc % :block) x)))
    (swap! td/tables-state update-in [:tables] (fn [x] (transform [ALL LAST] #(assoc % :hide-stools false) x)))
    (aset js/document "body" "style" "cursor" "default")))


(defn dragging
  ([on-drag start sel-top-lefts]
   (let [drag-move (drag-move-fn (on-drag sel-top-lefts) start)
         drag-end (drag-end-fn drag-move)]
     (events/listen js/window EventType.MOUSEMOVE drag-move)
     (events/listen js/window EventType.MOUSEUP drag-end))))

(defn table [{:keys [on-drag]} table-data-atom]
  (let [table-data @table-data-atom
        {:keys [id rs selected pos block stools stroke class hide-stools]} table-data
        [x y] pos
        rs-dir (vals rs)
        [width height] (td/table-dims stools)]
    [:g
     (if-not hide-stools
       (doall (for [stool (td/stool-maps x y width height rs-dir stools)
                    :let [stool-data (val stool)
                          id (:id stool-data)]]
                ^{:key id} [:g stool])))
     [:rect (merge td/table-defaults {:class            (if class class (td/table-defaults :class))
                                      :id               id
                                      :x                x
                                      :y                y
                                      :width            width
                                      :height           height
                                      :rx               (* width 0.2)
                                      :ry               (* height 0.2)
                                      :stroke           (if stroke stroke (td/table-defaults :stroke))
                                      :stroke-dasharray (if selected "5,5")
                                      :d                (if selected "M5 20 l215 0")
                                      :on-mouse-down    (fn [e] (dragging on-drag [(.-clientX e) (.-clientY e)] [[(:id table-data) (:pos table-data)]]))})]
     (if block [:rect (merge td/sel-defaults {:x      (first block)
                                              :y      (second block)
                                              :rx     (* x 0.01)
                                              :ry     (* y 0.01)
                                              :width  width
                                              :height height})])]))

(defn selection-rect [on-drag spoints]
  (let [{:keys [selection tables]} spoints
        selected (:selected selection)
        sel-top-lefts (mapv #(vector % (:pos (tables %))) selected)
        sel-start-end (merge (:start selection) (:end selection))
        {:keys [x y x1 y1]} sel-start-end
        width (- x1 x)
        height (- y1 y)
        [x width] (if (neg? width) [(+ x width) (- width)] [x width])
        [y height] (if (neg? height) [(+ y height) (- height)] [y height])]
    (r/create-class
      {:reagent-render
       (fn []
         [:g [:rect (merge td/sel-defaults
                           {:x             x
                            :y             y
                            :width         width
                            :height        height
                            :on-mouse-down (fn [e] (dragging on-drag [(.-clientX e) (.-clientY e)] sel-top-lefts))})]
          (if-not (:active selection)
            (su/sel-menu x y width height (su/sel-menu-tabs)))])})))



