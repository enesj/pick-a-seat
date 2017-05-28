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
    [pickaseat.ver01.floor-map.floor-draw :as draw]
    [pickaseat.ver01.intersections :as intersections]))


(defn move-poly [fig-selected]
  (fn [x-current y-current start-xy]
    (let [id (first fig-selected)
          points (second fig-selected)
          [x-start y-start] start-xy
          x-offset (- x-current x-start)
          y-offset (- y-current y-start)
          polygon  (map #(map + [x-offset y-offset] %) points)
          polygon-rounded  (map #(map cd/snap-round %) polygon)
          no-borders-intersection (= 0 (intersections/line-rect-intersections (flatten polygon-rounded ) [5 5 1990 1990]))]
         ;(js/console.log no-borders-intersection [x-current y-current] (flatten polygon))
      (when no-borders-intersection
        (swap! fd/data (fn [x] (assoc-in x [:figures id :polygon] polygon-rounded)))))))

(defn move-circle [fig-selected]
  (fn [x-current y-current start-xy]
    (let [id (first fig-selected)
          circle (second fig-selected)
          [center radius]circle
          [x-start y-start] start-xy
          x-offset (- x-current x-start)
          y-offset (- y-current y-start)
          center-new (mapv + [x-offset y-offset]  center)
          center-rounded  (mapv cd/snap-round center-new)
          circle-new [center-rounded radius]
          no-borders-intersection (= 0 (intersections/circle-rect-intersections circle-new [5 5 1990 1990]))]
      (when no-borders-intersection
        (swap! fd/data (fn [x] (assoc-in x [:figures id :circle] circle-new)))))))


(defn edit-svg [figures common-data opacity  x-bcr y-bcr data]
  (let [{:keys [selected selection-offset selection-end selection-start]} fd/specter-paths
        move-poly(fn [fig-selected] (move-poly fig-selected))
        move-circle (fn [fig-selected] (move-circle fig-selected))]
    [:svg
     {:style {:background-color "rgb(235,242,230)"}
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
     cd/filters
     (cd/snap-lines-horizontal)
     (cd/snap-lines-vertical)
     [:g
      (when-not (empty? figures)
        (f-common/draw-figures figures opacity {:polygon move-poly :circle move-circle}))]]))

