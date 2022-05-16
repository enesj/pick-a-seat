(ns pickaseat.floor-map.floor-edit
  #:ghostwheel.core {:check     true
                     :num-tests 10}
  (:use [com.rpl.specter :only [select transform setval FIRST LAST ALL keypath filterer
                                srange comp-paths compiled-select collect-one compiled-setval]])
  (:require
    [pickaseat.floor-map.floor-components :as floor-components]
    [pickaseat.data.floor-data :as floor-data]
    [pickaseat.floor-map.floor-common :as floor-common]
    [cljs.core.async :as async :refer [chan]]
    [pickaseat.data.common :as common]
    [pickaseat.intersections :as intersections]
    [pickaseat.complex.number :as complex-number]
    [pickaseat.data.background :as background]
    [clojure.spec.alpha :as s]
    [ghostwheel.core :as g
     :refer [>defn >defn- >fdef => | <- ?]]))


(defn move-poly-calc [points x-current y-current start-xy]
    (let [[x-start y-start] start-xy
          x-offset (- x-current x-start)
          y-offset (- y-current y-start)
          polygon  (mapv #(map + [x-offset y-offset] %) points)
          polygon-rounded  (mapv #(map common/layout-snap %) polygon)
          no-borders-intersection (zero? (intersections/line-rect-intersections (flatten polygon-rounded) [5 5 1990 1990]))]
      [no-borders-intersection polygon-rounded]))

(defn move-poly! [fig-selected]
  (fn [x-current y-current start-xy]
    (let [[id points] fig-selected
          [no-borders-intersection polygon-rounded] (move-poly-calc points x-current y-current start-xy)]
      (when no-borders-intersection
        (swap! floor-data/floor-state (fn [x] (assoc-in x [:figures id :polygon] polygon-rounded)))))))


(defn move-circle-calc [x-current y-current start-xy center radius]
    (let [[x-start y-start] start-xy
          x-offset (- x-current x-start)
          y-offset (- y-current y-start)
          center-new (mapv + [x-offset y-offset]  center)
          center-rounded  (mapv common/layout-snap center-new)
          circle-new [center-rounded radius]
          no-borders-intersection (zero? (intersections/circle-rect-intersections circle-new [5 5 1990 1990]))]
      [no-borders-intersection circle-new]))

(defn move-circle! [fig-selected]
  (fn [x-current y-current start-xy]
    (let [[id [center radius]] fig-selected
          [no-borders-intersection circle-new] (move-circle-calc x-current y-current start-xy center radius)]
      (when no-borders-intersection
        (swap! floor-data/floor-state (fn [x] (assoc-in x [:figures id :circle] circle-new)))))))

(defn average
  [numbers]
  (/ (apply + numbers) (count numbers)))

(defn move-poly-point-calc [ point-id points x-current y-current]
    (let [[x-bcr y-bcr] (:bcr-layout @common/data)
          x-offset (- x-current x-bcr)
          y-offset (- y-current y-bcr)
          new-poly (assoc-in (vec points) [point-id] (mapv common/layout-snap [x-offset y-offset]))
          no-borders-intersection (zero? (intersections/line-rect-intersections (flatten new-poly) [5 5 1990 1990]))
          all-self-intersections (intersections/self-poly-intersections new-poly)
          no-self-intersection (> 2 (count (remove false? all-self-intersections)))]
         [no-borders-intersection no-self-intersection new-poly]))

(defn move-poly-point! [figure-id point-id points]
  (fn [x-current y-current]
    (let [[no-borders-intersection no-self-intersection new-poly]
          (move-poly-point-calc point-id points x-current y-current)]
      (when (and no-borders-intersection no-self-intersection)
        (swap! floor-data/floor-state assoc-in [:figures figure-id :polygon ] new-poly)))))

(defn max-resize-calc [figure-id]
  (let [points (mapv vec (:polygon ((:figures @floor-data/floor-state) figure-id)))
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
    (js/console.log "mr" coords)
    [coords]))

(defn max-resize! [figure-id]
  (swap! floor-data/floor-state assoc-in [:figures figure-id :polygon]  (max-resize-calc figure-id)))


(defn resize-poly-by-midpoint [figure-id points mid-point]
  (fn [x-current y-current start-xy]
    (let [[x-bcr y-bcr] (:bcr-layout @common/data)
          central-coords (mapv #(mapv - % mid-point) points)
          start-xy (map - start-xy [ x-bcr y-bcr])
          xy-offset (map - [x-current y-current] [ x-bcr y-bcr])
          zoom-start (complex-number/distance (complex-number/c start-xy) (complex-number/c mid-point))
          zoom-new (complex-number/distance (complex-number/c xy-offset) (complex-number/c mid-point))
          zoom (/ zoom-new zoom-start)
          new-central-coords (mapv #(mapv (comp common/layout-snap  *) [zoom zoom] %)  central-coords)
          coords (mapv #(mapv (comp common/layout-snap  +) % mid-point) new-central-coords)
          no-borders-intersection (zero? (intersections/line-rect-intersections (flatten coords) [5 5 1990 1990]))]
      (if no-borders-intersection
        (swap! floor-data/floor-state assoc-in [:figures figure-id :polygon]  coords)
        (do (max-resize! figure-id)
            (max-resize! figure-id))))))


(defn resize-circle [figure-id center]
  (fn [x-current _]
    (let [[x-bcr y-bcr] (:bcr-layout @common/data)
          x-offset (- x-current x-bcr)
          ;y-offset (- y-current y-bcr)
          [center-x ] center
          new-r (Math/abs (- x-offset center-x))]
      (swap! floor-data/floor-state assoc-in [:figures figure-id :circle] [center new-r]))))

(defn resize-points [data]
  (let [poly-id (first (:selected (:selection data)))]
    (if (= (ffirst ((:figures data) poly-id)) :polygon)
      (let [points (:polygon ((:figures data) poly-id))
            resize-points-offset (:resize-ponts-offset floor-data/base-settings)
            x-points (mapv first points)
            y-points (mapv second points)
            x-min (- (apply min x-points) resize-points-offset)
            x-max (+ (apply max x-points) resize-points-offset)
            y-min (- (apply min y-points) resize-points-offset)
            y-max (+ (apply max y-points) resize-points-offset)
            control-points [[x-min y-min] [x-max y-min] [x-max y-max] [x-min y-max]]
            indexed-points (map-indexed (fn [idx itm] [idx itm]) points)
            mid-point [(average x-points) (average y-points)]]
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
                                                     (fn [] (resize-poly-by-midpoint poly-id (vec points) mid-point)))
                           control-points)

                     (mapv #(floor-components/circle (second %) 0 (:poly-points-style floor-data/base-settings) false
                                                     (fn [] (move-poly-point! poly-id (first %) points)))
                           indexed-points))))
      (let [[center r] (:circle ((:figures data) poly-id))
            [center-x center-y] center]
        (floor-components/circle [(+ center-x r) center-y] 0 (:resize-point-style floor-data/base-settings) false
                                 (fn [] (resize-circle poly-id center)))))))



(defn edit-floor [figures]
  (let [data @floor-data/floor-state
        comm-data @common/data]
    [:svg
     {:style         {:background-color (:grid-back-color common/data)}
      :width         (:w comm-data)
      :height        (:h comm-data)
      :on-mouse-down (fn [e]
                       (.preventDefault e)
                       (swap! floor-data/floor-state assoc-in [:selection :selected] []))
      :on-mouse-up   (fn [e]
                       (.preventDefault e)
                       (let [
                             {:keys [performed]} @floor-data/floor-states-data
                             shift-performed (if (= (count performed) (:history-length floor-data/base-settings))
                                               (vec (rest performed)) performed)]
                         (reset!  floor-data/floor-states-data {:performed (conj shift-performed data) :recalled []})))

      :on-mouse-move (fn [e]
                       (.preventDefault e))}
     ;floor-data/filters
     (background/snap-lines-horizontal :snap-layout)
     (background/snap-lines-vertical :snap-layout)
     [:g
      (when (seq figures) (floor-components/draw-figures figures nil
                                                     {:circle move-circle!, :polygon move-poly!}
                                                     (first (:selected (:selection data)))))]
     [resize-points data]]))

