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

(defn move-poly-point [figure-id point-id points  bcr]
  (fn [x-current y-current start-xy]
    (let [x-bcr (.-left (.getBoundingClientRect @bcr))
          y-bcr (.-top (.getBoundingClientRect @bcr))
          x-offset (- x-current x-bcr)
          y-offset (- y-current y-bcr)
          new-poly (assoc-in (vec points) [point-id] (mapv cd/snap-round [x-offset y-offset]))
          no-borders-intersection (= 0 (intersections/line-rect-intersections (flatten new-poly) [5 5 1990 1990]))
          all-self-intersections (intersections/self-poly-intersections new-poly)
          no-self-intersection (> 2 (count (remove false? all-self-intersections)))]
      ;(js/console.log  (remove false? all-self-intersections))
      (when (and no-borders-intersection no-self-intersection)
        (swap! fd/data assoc-in [:figures figure-id :polygon ] new-poly)))))

(defn resize-poly-by-midpoint [figure-id points mid-point  control-points bcr]
  (fn [x-current y-current start-xy]
    (let [
          x-bcr (.-left (.getBoundingClientRect @bcr))
          y-bcr (.-top (.getBoundingClientRect @bcr))
          central-coords (mapv #(mapv - % mid-point) points)
          start-xy (map - start-xy [ x-bcr y-bcr])
          xy-offset (map - [x-current y-current] [ x-bcr y-bcr])
          zoom-start (n/distance (n/c start-xy) (n/c mid-point))
          zoom-new (n/distance (n/c xy-offset) (n/c mid-point))
          zoom (/ zoom-new zoom-start)
          new-central-coords (mapv #(mapv * [zoom zoom] %)  central-coords)
          coords (mapv #(mapv + % mid-point) new-central-coords)
          ;rounded-coords (mapv #(mapv cd/snap-round %) coords)
          no-borders-intersection (= 0 (intersections/line-rect-intersections (flatten coords) [5 5 1990 1990]))]
      ;(js/console.log coords )
      (when no-borders-intersection
        (swap! fd/data assoc-in [:figures figure-id :polygon] coords)))))

(defn resize-circle [figure-id center r bcr]
  (fn [x-current y-current start-xy]
    (let [x-bcr (.-left (.getBoundingClientRect @bcr))
          y-bcr (.-top (.getBoundingClientRect @bcr))
          x-offset (- x-current x-bcr)
          y-offset (- y-current y-bcr)
          [center-x center-y] center
          new-r (- x-offset center-x)]
      (swap! fd/data assoc-in [:figures figure-id :circle] [center new-r]))))


(defn resize-points [bcr]
  (let [data @fd/data
        poly-id (first (:selected (:selection data)))]
    (if (= (ffirst ((:figures data) poly-id)) :polygon)
      (let [points (:polygon ((:figures data) poly-id))
            resize-ponts-offset 50
            x-points (mapv first points)
            y-points (mapv second points)
            x-min (- (apply min x-points) resize-ponts-offset)
            x-max (+ (apply max x-points) resize-ponts-offset)
            y-min (- (apply min y-points) resize-ponts-offset)
            y-max (+ (apply max y-points))
            control-points [[x-min y-min] [x-max y-min] [x-min y-max] [x-max y-max]]
            indexed-points (map-indexed (fn [idx itm] [idx itm]) points)
            mid-point [(average (mapv first points)) (average (mapv second points))]]
        ;(println control-points)
        (if (= (ffirst ((:figures data) poly-id)) :polygon)
          (vec (concat [:g
                        (comps/circle mid-point 0 (:connection-point-style fd/base-settings) false nil)]
                       (mapv #(comps/circle % 0 (:resize-point-style fd/base-settings) false
                                            (fn [] (resize-poly-by-midpoint poly-id (vec points) mid-point control-points bcr)))
                             control-points)
                       (mapv #(comps/circle (second %) 0 (:poly-points-style fd/base-settings) false
                                            (fn [] (move-poly-point poly-id (first %) points bcr)))
                             indexed-points)))))
      (let [[center r] (:circle ((:figures data) poly-id))
            [center-x center-y] center]
        (comps/circle [(+ center-x r) center-y] 0 (:resize-point-style fd/base-settings) false (fn [] (resize-circle poly-id center r bcr)))))))



(defn edit-svg [figures common-data opacity]
  (let [move-poly (fn [fig-selected] (move-poly fig-selected))
        move-circle (fn [fig-selected] (move-circle fig-selected))
        bcr (atom nil)]
    [:svg
     {:style {:background-color (:grid-back-color @cd/data)}
      :width         (:w common-data)
      :height        (:h common-data)
      :ref           #(when %
                        (reset! bcr %))
      :on-mouse-down (fn [e]
                       (.preventDefault e)
                       (swap! fd/data assoc-in [:selection :selected] []))
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
     [resize-points bcr]]))

