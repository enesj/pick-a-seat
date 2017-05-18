(ns pickaseat.ver01.floor-map.floor-edit
  (:use [com.rpl.specter :only [select transform setval FIRST LAST ALL keypath filterer
                                srange comp-paths compiled-select collect-one compiled-setval]])
  (:require
    [reagent.core :as r]
    [pickaseat.ver01.floor-map.floor-components :as comps]
    [pickaseat.ver01.floor-map.floor-draw-events :as de]
    [pickaseat.ver01.data.floor-data :as fd]
    [pickaseat.ver01.floor-map.floor-common :as f-common]
    [cljs.core.async :as async :refer [chan]]
    [pickaseat.ver01.data.common-data :as cd]
    [pickaseat.ver01.data.table_data :as td]
    [pickaseat.ver01.tables.tables-components :as c]
    [pickaseat.ver01.floor-map.floor-draw :as draw]))


(defn move-figure [fig-selected]
  (fn [x-current y-current start-xy]
    (let [id (first fig-selected)
          points (second fig-selected)
          [x-start y-start] start-xy
          x-offset (- x-current x-start)
          y-offset (- y-current y-start)
          polygon  (map #(map + [x-offset y-offset] %) points)]
      (swap! fd/data (fn [x] (assoc-in x [:figures id :polygon] polygon))))))


(defn edit-svg [figures common-data opacity ui-channel x-bcr y-bcr data]
  (let [{:keys [selected selection-offset selection-end selection-start]} fd/specter-paths
        move-figures (fn [fig-selected] (move-figure fig-selected))]
    [:svg
     {
      :width         (:w common-data)
      :height        (:h common-data)
      :ref           #(when %
                        (reset! x-bcr (.-left (.getBoundingClientRect %)))
                        (reset! y-bcr (.-top (.getBoundingClientRect %))))
      :on-mouse-down (fn [e]
                       (.preventDefault e)
                       (let [x-current (+ (.-clientX e) (.-pageXOffset js/window) x-bcr)
                             y-current (+ (.-clientY e) (.-pageYOffset js/window) y-bcr)
                             start {:x x-current :y y-current}
                             end {:x1 x-current :y1 y-current}]
                         (swap! data #(->> %
                                           (compiled-setval selection-start start)
                                           (compiled-setval selection-end end)
                                           (compiled-setval selected [])))))


      :on-mouse-up   (fn [e]
                       (.preventDefault e))


      :on-mouse-move (fn [e]
                       (.preventDefault e))}
                       ;(let [x-current (+ (.-clientX e) (.-pageXOffset js/window) x-bcr)
                       ;      y-current (+ (.-clientY e) (.-pageYOffset js/window) y-bcr)
                       ;      end {:x1 x-current :y1 y-current}]
                       ;  (swap! data #(->> % (compiled-setval selection-end end)))))}

     cd/filters
     [:g
      (when-not (empty? figures)
        (f-common/draw-figures figures opacity move-figures))]]))

