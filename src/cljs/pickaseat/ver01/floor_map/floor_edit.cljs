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
    [pickaseat.ver01.intersections :as intersections]
    [complex.number :as n]))


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

(defn average
  [numbers]
  (/ (apply + numbers) (count numbers)))

(defn resize-poly-by-midpoint [figure-id point-id points]
  (fn [x-current y-current start-xy]
    (let [data @fd/data
          ;points (:polygon ((:figures data) figure-id))
          mid-point [(average (mapv first points)) (average (mapv second points))]
          central-coords (mapv #(mapv - % mid-point) points)
          [x-start y-start] start-xy
          x-offset (- x-current 20)
          y-offset (- y-current 180)
          zoom-start (n/distance (n/c start-xy) (n/c mid-point))
          zoom-new (n/distance (n/c [x-offset y-offset]) (n/c mid-point))
          zoom (/ zoom-new zoom-start)
          new-central-coords (mapv #(mapv * [zoom zoom] %)  central-coords)
          coords (mapv #(mapv + % mid-point) new-central-coords)]
      (js/console.log mid-point points x-offset y-offset)
      (swap! fd/data assoc-in [:figures figure-id :polygon] coords))))


(defn resize-points []
  (let [data @fd/data
        poly-id (first (:selected (:selection data)))
        points (:polygon ((:figures data) poly-id))
        indexed-points (map-indexed (fn [idx itm] [idx itm]) points)]
    ;(println poly-id points)
    (into [:g](mapv #(comps/circle  (second %) 0 (:resize-point-style fd/base-settings) false
                                    (fn [fig-selected](resize-poly-by-midpoint poly-id (first %) points)))
                    indexed-points))))

(defn edit-svg [figures common-data opacity  x-bcr y-bcr data]
  (let [move-poly (fn [fig-selected] (move-poly fig-selected))
        move-circle (fn [fig-selected] (move-circle fig-selected))]
    [:svg
     {:style {:background-color (:grid-back-color @cd/data)}
      :width         (:w common-data)
      :height        (:h common-data)
      :ref           #(when %
                        (reset! x-bcr (.-left (.getBoundingClientRect %)))
                        (reset! y-bcr (.-top (.getBoundingClientRect %))))
      :on-mouse-down (fn [e]
                       (.preventDefault e))

      :on-mouse-up   (fn [e]
                       (.preventDefault e))


      :on-mouse-move (fn [e]
                       (.preventDefault e))}
     cd/filters
     (cd/snap-lines-horizontal)
     (cd/snap-lines-vertical)
     [:g
      (when-not (empty? figures)
        (f-common/draw-figures figures opacity {:polygon move-poly :circle move-circle}))]
     [resize-points]]))

