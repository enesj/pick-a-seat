(ns pickaseat.ver01.floor-map.floor-edit
  (:use [com.rpl.specter :only [select transform setval FIRST LAST ALL keypath filterer
                                srange comp-paths compiled-select collect-one compiled-setval]])
  (:require
    [pickaseat.ver01.floor-map.floor-components :as floor-components]
    [pickaseat.ver01.data.floor-data :as floor-data]
    [pickaseat.ver01.floor-map.floor-common :as floor-common]
    [cljs.core.async :as async :refer [chan]]
    [pickaseat.ver01.data.common-data :as common-data]
    [pickaseat.ver01.intersections :as intersections]
    [complex.number :as complex-number]))


(defn move-poly [fig-selected]
  (fn [x-current y-current start-xy]
    (let [[id points] fig-selected
          [x-start y-start] start-xy
          x-offset (- x-current x-start)
          y-offset (- y-current y-start)
          polygon  (mapv #(map + [x-offset y-offset] %) points)
          polygon-rounded  (mapv #(map common-data/snap-round %) polygon)
          no-borders-intersection (zero? (intersections/line-rect-intersections (flatten polygon-rounded) [5 5 1990 1990]))]
      ;(js/console.log no-borders-intersection [x-current y-current] (flatten polygon))
      (when no-borders-intersection
        (swap! floor-data/data (fn [x] (assoc-in x [:figures id :polygon] polygon-rounded)))))))

(defn move-circle [fig-selected]
  (fn [x-current y-current start-xy]
    (let [[id [center radius]] fig-selected
          [x-start y-start] start-xy
          x-offset (- x-current x-start)
          y-offset (- y-current y-start)
          center-new (mapv + [x-offset y-offset]  center)
          center-rounded  (mapv common-data/snap-round center-new)
          circle-new [center-rounded radius]
          no-borders-intersection (zero? (intersections/circle-rect-intersections circle-new [5 5 1990 1990]))]
      (when no-borders-intersection
        (swap! floor-data/data (fn [x] (assoc-in x [:figures id :circle] circle-new)))))))

(defn average
  [numbers]
  (/ (apply + numbers) (count numbers)))

(defn move-poly-point [figure-id point-id points  bcr]
  (fn [x-current y-current start-xy]
    (let [x-bcr (.-left (.getBoundingClientRect @bcr))
          y-bcr (.-top (.getBoundingClientRect @bcr))
          x-offset (- x-current x-bcr)
          y-offset (- y-current y-bcr)
          new-poly (assoc-in (vec points) [point-id] (mapv common-data/snap-round [x-offset y-offset]))
          no-borders-intersection (zero? (intersections/line-rect-intersections (flatten new-poly) [5 5 1990 1990]))
          all-self-intersections (intersections/self-poly-intersections new-poly)
          no-self-intersection (> 2 (count (remove false? all-self-intersections)))]
      ;(js/console.log  (remove false? all-self-intersections))
      (when (and no-borders-intersection no-self-intersection)
        (swap! floor-data/data assoc-in [:figures figure-id :polygon ] new-poly)))))

(defn max-resize [figure-id]
  (let [points (mapv vec (:polygon ((:figures @floor-data/data) figure-id)))
        x-points (mapv first points)
        y-points (mapv second points)
        mid-point [(average x-points) (average y-points)]
        x-min-index (first (apply min-key second (map-indexed vector x-points)))
        y-min-index (first (apply min-key second (map-indexed vector y-points)))
        central-coords (mapv #(mapv - % mid-point) points)
        central-coords-x (central-coords x-min-index)
        central-coords-y (central-coords y-min-index)
        zoom-new-x (Math/abs (/ (first mid-point) (- (first central-coords-x) 5)))
        zoom-new-y (Math/abs (/ (second mid-point) (- (second central-coords-y) 5)))
        zoom (min zoom-new-x zoom-new-y)
        new-central-coords (mapv #(mapv * [zoom zoom] %) central-coords)
        coords (mapv #(mapv + % mid-point) new-central-coords)]
    ;no-borders-intersection (= 0 (intersections/line-rect-intersections (flatten coords) [5 5 1990 1990]))]
    ;(println x-min-index coords)
    (swap! floor-data/data assoc-in [:figures figure-id :polygon] coords)))

(defn resize-poly-by-midpoint [figure-id points mid-point  control-points bcr]
  (fn [x-current y-current start-xy]
    (let [
          x-bcr (.-left (.getBoundingClientRect @bcr))
          y-bcr (.-top (.getBoundingClientRect @bcr))
          central-coords (mapv #(mapv - % mid-point) points)
          start-xy (map - start-xy [ x-bcr y-bcr])
          xy-offset (map - [x-current y-current] [ x-bcr y-bcr])
          zoom-start (complex-number/distance (complex-number/c start-xy) (complex-number/c mid-point))
          zoom-new (complex-number/distance (complex-number/c xy-offset) (complex-number/c mid-point))
          zoom (/ zoom-new zoom-start)
          new-central-coords (mapv #(mapv * [zoom zoom] %)  central-coords)
          coords (mapv #(mapv + % mid-point) new-central-coords)
          ;rounded-coords (mapv #(mapv cd/snap-round %) coords)
          no-borders-intersection (zero? (intersections/line-rect-intersections (flatten coords) [5 5 1990 1990]))]
      ;(js/console.log coords )
      (if no-borders-intersection
        (swap! floor-data/data assoc-in [:figures figure-id :polygon] coords)
        (do (max-resize figure-id) (max-resize figure-id))))))


(defn resize-circle [figure-id center r bcr]
  (fn [x-current y-current start-xy]
    (let [x-bcr (.-left (.getBoundingClientRect @bcr))
          y-bcr (.-top (.getBoundingClientRect @bcr))
          x-offset (- x-current x-bcr)
          y-offset (- y-current y-bcr)
          [center-x center-y] center
          new-r (Math/abs (- x-offset center-x))]
      (swap! floor-data/data assoc-in [:figures figure-id :circle] [center new-r]))))


(defn resize-points [data bcr]
  (let [poly-id (first (:selected (:selection data)))]
    (if (= (ffirst ((:figures data) poly-id)) :polygon)
      (let [points (:polygon ((:figures data) poly-id))
            resize-ponts-offset 50
            x-points (mapv first points)
            y-points (mapv second points)
            x-min (- (apply min x-points) resize-ponts-offset)
            x-max (+ (apply max x-points) resize-ponts-offset)
            y-min (- (apply min y-points) resize-ponts-offset)
            y-max (+ (apply max y-points) resize-ponts-offset)
            control-points [[x-min y-min] [x-max y-min] [x-max y-max] [x-min y-max]]
            indexed-points (map-indexed (fn [idx itm] [idx itm]) points)
            mid-point [(average x-points) (average y-points)]]
        ;(println control-points)
        (if (= (ffirst ((:figures data) poly-id)) :polygon)
          (vec (concat [:g
                        (floor-components/circle mid-point 0 (:connection-point-style floor-data/base-settings) false nil)]
                        ;(comps/polygon
                        ;  {:stroke  "black"
                        ;   :stroke-width 2
                        ;   :fill    "none"
                        ;   :opacity 0.2}
                        ;  ;:filter  "url(#s1)"}
                        ;  control-points
                        ;  nil)]
                       (mapv #(floor-components/circle % 0 (:resize-point-style floor-data/base-settings) false
                                                       (fn [] (resize-poly-by-midpoint poly-id (vec points) mid-point control-points bcr)))
                             control-points)

                       (mapv #(floor-components/circle (second %) 0 (:poly-points-style floor-data/base-settings) false
                                                       (fn [] (move-poly-point poly-id (first %) points bcr)))
                             indexed-points)))))
      (let [[center r] (:circle ((:figures data) poly-id))
            [center-x center-y] center]
        (floor-components/circle [(+ center-x r) center-y] 0 (:resize-point-style floor-data/base-settings) false (fn [] (resize-circle poly-id center r bcr)))))))



(defn edit-svg [figures common-data opacity]
  (let [data @floor-data/data
        move-poly move-poly
        move-circle move-circle
        bcr (atom nil)]
    [:svg
     {:style {:background-color (:grid-back-color @common-data/data)}
      :width         (:w common-data)
      :height        (:h common-data)
      :ref           #(when %
                        (reset! bcr %))
      :on-mouse-down (fn [e]
                       (.preventDefault e)
                       (swap! floor-data/data assoc-in [:selection :selected] []))
      :on-mouse-up   (fn [e]
                       (.preventDefault e)
                       (let [history @(:history floor-data/floor-state)
                             {:keys [performed]} history
                             shift-performed (if (= (count performed) (:history-length floor-data/base-settings))
                                                 (vec (rest performed)) performed)]
                         (reset! (:history floor-data/floor-state) {:performed (conj shift-performed data) :recalled []})))

      :on-mouse-move (fn [e]
                       (.preventDefault e))}
     common-data/filters
     (common-data/snap-lines-horizontal)
     (common-data/snap-lines-vertical)
     [:g
      (when (seq figures) (floor-common/draw-figures figures opacity {:circle move-circle, :polygon move-poly}))]
     [resize-points data bcr]]))

