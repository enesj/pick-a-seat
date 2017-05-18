(ns pickaseat.ver01.tables.tables-components
  (:use [com.rpl.specter :only [select transform setval FIRST LAST ALL keypath]])
  (:require [reagent.core :as r]
            [goog.events :as events]
            [pickaseat.ver01.data.table_data :as td]
            [pickaseat.ver01.tables.table-utils :as u]
            [pickaseat.ver01.tables.tables-analize :as  an]
            [pickaseat.ver01.tables.selection-utils :as su]
            [pickaseat.ver01.tables.table-svg :as svg]
            [debux.cs.core :refer-macros [clog dbg break]])
  (:import [goog.events EventType]))

(defn drag-move-fn [on-drag start]
  (fn [evt]
    ;pageY (aget evt "event_" "pageY")  XY poyicija misa na strani
    ;pageX (aget evt "event_" "pageX")
    (.preventDefault evt)
    (on-drag (.-clientX evt) (.-clientY evt) start (:tables @td/tables-state) (.-ctrlKey evt))))


(defn drag-end-fn [drag-move]
  (fn [evt]
    (.preventDefault evt)
    (events/unlisten js/window EventType.MOUSEMOVE drag-move)
    (swap! td/tables-state #(-> %
                                (update-in [:tables] (fn [x] (transform [ALL LAST] (fn [y] (assoc-in y [:block] nil)) x)))
                                (update-in [:tables] (fn [x] (transform [ALL LAST] (fn [y] (assoc-in y [:hide-stools] false)) x)))))
    (aset js/document "body" "style" "cursor" "default")))


(defn dragging
  ([on-drag start sel-top-lefts]
   (let [drag-move (drag-move-fn (on-drag sel-top-lefts) start)
         drag-end (drag-end-fn drag-move)]
     (events/listen js/window EventType.MOUSEMOVE drag-move)
     (events/listen js/window EventType.MOUSEUP drag-end))))

(defn merege-path [x]
  [:path (merge td/menu-defaults x)])

(defn stool-data [stool dir id t1 t2]
  (conj [:rect] (assoc stool :dir dir :id id :transform (str " translate(" t1 ", " t2 ")"))))


(defn stool-maps [x y w h rs stools]
  (let [dims (:stool-dims @td/base-settings)
        {{n-h :h  n-w :w } :normal {s-h :h s-w :w } :small } dims
        wr2 (/ n-h 2)
        h-wr (+ h n-h)
        h-wr2 (+ h wr2)
        w-wr (+ w n-h)
        w-wr2 (+ w wr2)
        h2 (/ h 2)
        w2 (/ w 2)
        stool-h (merge td/stool-defaults-normal  {:width n-h :height n-w} {:x ( - x n-h)        :y (+ y h2 ( - n-h))})
        stool-v (merge td/stool-defaults-normal  {:width n-w :height n-h} {:x (+ x w2 ( - n-h)) :y ( - y n-h)})
        stool-h-s (merge td/stool-defaults-small {:width s-h :height s-w} {:x ( - x wr2)        :y (+ y h2 ( - wr2))})
        stool-v-s (merge td/stool-defaults-small {:width s-w :height s-h} {:x (+ x w2 ( - wr2)) :y ( - y wr2)})
        all-seats-new (->>  (doall (for [side (range 0 4)
                                         :let [stools-side (inc (stools side))]]
                                     (for [i (range 1 stools-side)
                                           :let [id1 (keyword (str side "-" (+ (* i 2) 1)))
                                                 id1s (keyword (str id1 "-s"))
                                                 id2 (keyword (str side "-" (+ (* i 2) 2)))
                                                 id2s (keyword (str id1 "-s"))
                                                 t-h (- (* i (/ w stools-side)) w2)
                                                 t-v (- (* i (/ h stools-side)) h2)]]
                                       (case side
                                         0
                                         [(stool-data stool-v :t id1 t-h 0)
                                          (stool-data stool-v-s :t id1s t-h 0)]
                                         1
                                         [(stool-data stool-v :d id2 t-h h-wr)
                                          (stool-data stool-v-s :d id2s t-h h-wr2)]
                                         2
                                         [(stool-data stool-h :l id1 0 t-v)
                                          (stool-data stool-h-s :l id1s 0 t-v)]
                                         3
                                         [(stool-data stool-h :r id2 w-wr t-v)
                                          (stool-data stool-h-s :r id2s w-wr2 t-v)]))))
                            flatten
                            (partition 2)
                            (mapv vec)
                            (into []))]
    (loop [rs rs
           all-seats all-seats-new]
      (if (> (count rs) 0)
        (recur (next rs) (into [] (remove (fn [x] (some #(= (:dir (second x)) %) (first rs))) all-seats)))
        all-seats))))


(defn table [move-tables table-data-atom]
  (let [table-data @table-data-atom
        {:keys [x y id rs selected block stools stroke class hide-stools fill-opacity del]} table-data
        rs-dir (vals rs)
        [width height] (td/table-dims stools)]
    [:g
     (if-not hide-stools
       (doall (for [stool (stool-maps x y width height rs-dir stools)
                    :let [stool-data (val stool)
                          id (:id stool-data)]]
                ^{:key id} [:g stool])))
     [:text {:x (+ x 10) :y (+ y 20) :font-size 11 }  id]
     [:rect (merge td/table-defaults {:class            (or class (td/table-defaults :class))
                                      :id               id
                                      :x                x
                                      :y                y
                                      :width            width
                                      :height           height
                                      :rx               (* width 0.2)
                                      :ry               (* height 0.2)
                                      :fill-opacity     (or fill-opacity 0.3)
                                      :stroke           (or stroke (td/table-defaults :stroke))
                                      :stroke-dasharray (when selected "5,5")
                                      :d                (when selected "M5 20 l215 0")
                                      :on-mouse-down    (if (= move-tables nil) nil
                                                                            (fn [e] (dragging move-tables [(.-clientX e) (.-clientY e)] [[(:id table-data) ((juxt :x :y) table-data)]])))})]
     (if del (svg/delete-tables x y width height))
     (when (and  block move-tables)
       [:rect (merge td/sel-defaults {:x      (first block)
                                      :y      (second block)
                                      :rx     (* x 0.01)
                                      :ry     (* y 0.01)
                                      :width  width
                                      :height height})])]))


(defn selection-rect [on-drag full-state]
  (let [{:keys [selection tables]} full-state
        selected (:selected selection)
        sel-top-lefts (mapv #(vector % ((juxt :x :y) (tables %))) selected)
        [[x-s y-s] [x-e y-e]] (u/start-end (:start selection) (:end selection))
        width (- x-e x-s)
        height (- y-e y-s)
        selected-current @an/selected-current
        selection-state (:current-state selected-current)
        ids (:ids selected-current)]
    (r/create-class
      {:reagent-render
       (fn []
         [:g
          (if (seq ids)
              (doall (for [id ids
                           :when (not-empty ((:tables selected-current) id))]
                       ^{:key id} [table {:on-drag nil} (r/cursor an/selected-current [:tables id])])))
          [:rect (merge td/sel-defaults
                           {:x             x-s
                            :y             y-s
                            :width         width
                            :height        height
                            :on-mouse-down (fn [e] (dragging on-drag [(.-clientX e) (.-clientY e)] sel-top-lefts))
                            :on-mouse-up   (fn [e]
                                             ;(.preventDefault e)
                                             (let [all-states (an/test-all)
                                                   new-state  (if (and (or
                                                                         (= (:start selection) (:start  selected-current))
                                                                         (= (:start  selected-current) {}))
                                                                       (< selection-state (- (count all-states) 1)))
                                                                  (inc selection-state) 0)]
                                               (when (not (:active selection))
                                                 (su/preview-state new-state full-state all-states)
                                                 (swap! an/selected-current #(-> %
                                                                                 (assoc-in [:current-state] new-state)
                                                                                 (assoc-in [:start] (:start (:selection @td/tables-state))) ;; mora citati najnovije podatke jer ih mijenja
                                                                                 (assoc-in [:end] (:end (:selection @td/tables-state))))))))})] ;; funkcija preview-state
          (if (and (not (:active selection)) (> (count ids) 0) (not= selection-state 0))
            (su/sel-menu x-s y-s width height full-state))])})))




