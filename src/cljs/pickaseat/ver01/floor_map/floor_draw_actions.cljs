(ns pickaseat.ver01.floor-map.floor-draw-actions
  (:require
    [complex.vector :as v]
    [complex.number :as n]
    [complex.geometry :as g]
    [cljsjs.svg-intersections]
    [cljs.core.match :refer-macros [match]]
    [pickaseat.ver01.intersections :as intersections]
    [pickaseat.ver01.data.common-data :as cd]))
    ;[cuerdas.core :as str]))



(defn segment-collision [new-line polyline]
  (let [test-polylines (butlast polyline)]
    (if (not-empty polyline)
      (last
        (sort-by #(n/distance (n/c (last %)) (n/c (last test-polylines)))
                 (filter #(true? (first %))
                         (doall (for [poly test-polylines]
                                  (let [coll-point (map Math/round (g/intersection new-line (vec poly)))
                                        poly-x (->> poly (map first)  (map Math/round) sort)
                                        new-line-x (->> new-line (map first)  (map Math/round) sort)
                                        poly-y (->> poly (map second)  (map Math/round) sort)
                                        new-line-y (->> new-line (map second)  (map Math/round) sort)
                                        coll (and (and (apply <= (interpose (first coll-point) poly-x))
                                                       (apply <= (interpose (first coll-point) new-line-x)))
                                                  (and (apply <= (interpose (second coll-point ) poly-y))
                                                       (apply <= (interpose (second coll-point ) new-line-y))))]
                                    [coll poly coll-point])))))))))


(defn round-x [num den] (let [q (quot num den)
                              r (rem num den)
                              q (if (>= r (/ den 2)) (inc q) q)]
                          (* q den)))

(defn angle-snap [angle c-angle]
  (condp (fn [a b] (>= (+ a c-angle) b (- a c-angle))) angle
    0 0
    45 45
    90 90
    135 135
    180 180
    -45 -45
    -90 -90
    -135 -135
    -180 -180
    angle))

(defn line-angle [line c-angle]
  (let [angle (v/angle (apply map - line))
        angle-deg (n/rad->deg angle)]
    (angle-snap angle-deg c-angle)))

(defn two-lines-angle-degree [line1-angle line2-angle]
  (- 180 (-> (- line1-angle line2-angle ) (Math/abs ) (- 180)  (Math/abs))))

(defn constrain-line [mouse-possition polyline line position]
  (if (and (not= mouse-possition (first line)) (not-empty line))
    (let [
          new-line [mouse-possition (first line)]
          new-line-radius (v/len (apply map - new-line))
          new-line-angle (line-angle new-line 5)
          last-poly-angle (when (> (count polyline) 0) (line-angle (vec (last polyline)) 1))
          last-poly-test (if (not-empty polyline)
                           (< (two-lines-angle-degree new-line-angle last-poly-angle) 15)
                           false)
          new-position  (->> (n/complex-polar new-line-radius new-line-angle) vec (mapv #(int (second %))) (mapv + (second new-line)))
          segment-collison-test (segment-collision (assoc-in line [1] new-position) polyline)
          [segment-collison? collision-line collison-point] (when-not last-poly-test segment-collison-test)
          cut-polylne (if segment-collison? (as-> (drop-while #(not= collision-line %) polyline) $
                                                  (into (mapv first (butlast $)) (last $))
                                                  (assoc-in $ [0] collison-point)))
          cut-line (if cut-polylne (assoc-in line [1] collison-point))]
      (if-not (or segment-collison? last-poly-test)
        [new-position {:angle new-line-angle}]
        [position {:cut-poly cut-polylne :cut-line cut-line}]))
    [mouse-possition nil]))


(defn draw-shadow [position mouse-possition polyline shadow-polyline app]
  (let [
        poly (partition 2 1 polyline)
        angle-first (v/angle (map - (second (first poly)) (ffirst poly)))
        shadow-width (* (n/distance (n/c mouse-possition) (n/c (ffirst poly)))
                        (Math/sin (- (v/angle (mapv - mouse-possition (ffirst poly))) angle-first)))
        polyline (partition 2 (flatten poly))
        move #(n/polar->rect shadow-width (+ (v/angle (map - (second %) (first %))) (/ n/PI 2)))
        new-poly (partition 2 (flatten (map #(repeat 2 %) (mapv move poly))))
        raw-shadow (mapv #(mapv + %1 %2) new-poly polyline)
        first-shadow (mapv vec (partition 2 1 (mapv vec (partition 2 raw-shadow))))
        intersections (mapv #(g/intersection (first %) (second %)) first-shadow)
        shadow (mapv #(mapv cd/snap-round %) (concat (vector (first polyline)) (vector (first raw-shadow)) intersections
                                                      (vector (last raw-shadow)) (vector (last polyline))))
        all-self-intersections (intersections/self-poly-intersections shadow)
        no-self-intersection (> 3 (count (remove false? all-self-intersections)))
        border-test (intersections/line-rect-intersections (flatten shadow) [5 5 1990 1990])
        app (assoc-in app [:position] mouse-possition)]
    ;(js/console.log "sh" shadow)
    (if (and no-self-intersection (= border-test 0) (> 3 (intersections/poly-poly-intersection shadow poly)))
      (-> app
          (assoc-in [:shadow-polyline] shadow))
          ;(assoc-in  [:shadow-raw] raw-shadow))
      app)))

(defn snap-test [polyline position]
  (if (> (count polyline) 2)
    (let [polyline-nolast (map #(map Math/round %)  (butlast polyline))
          x-match (filter #(= (Math/round (first position)) (first %)) polyline-nolast)
          y-match (filter #(= (Math/round (second position)) (second %)) polyline-nolast)]
      (into x-match y-match))))

(defn draw-poly [app mouse-possition]
  (let [{:keys [position polyline shadow-polyline shadow? line pen cut-poly cut-line]} app]
    (if shadow?
      (draw-shadow position mouse-possition polyline shadow-polyline app)
      (let [constrain-line (if (= pen :down) (constrain-line mouse-possition (partition 2 1 polyline) line position) [mouse-possition nil])
            ;constrain-line (mapv #(mapv Math/round %) constrain-line)
            [constrain-x constrain-y] (first constrain-line)
            constrain-params (second constrain-line)
            constrain-angle (:angle constrain-params)
            cut-poly-new (:cut-poly constrain-params)
            cut-line-new (:cut-line constrain-params)
            snap-xs (mapv first polyline)
            snap-ys (mapv second polyline)
            snap-x (some #(if (<= (- % 10) constrain-x (+ % 10)) %) snap-xs)
            snap-y (some #(if (<= (- % 10) constrain-y (+ % 10)) %) snap-ys)
            snap-points (when (or snap-x snap-y) (snap-test polyline position))
            constrain-snap [(if snap-x snap-x constrain-x)
                            (if snap-y snap-y constrain-y)]]
            ;no-borders-intersection (= 0 (intersections/line-rect-intersections (flatten [(last polyline) constrain-snap])  [5 5 1990 1990]))]
        ;(js/console.log   "cl" app)
        (as-> app $
              (if (or (not cut-poly) (not cut-poly-new)) (assoc-in $ [:cut-poly] cut-poly-new) $)
              (if (or (not cut-line) (not cut-line-new)) (assoc-in $ [:cut-line] cut-line-new) $)
              (if (and constrain-angle (= pen :down))
               (assoc-in $ [:line-angle] constrain-angle)
               $)
              (assoc-in $ [:snap-points] snap-points)
              (assoc-in $ [:position] (mapv cd/snap-round constrain-snap)) $)))))

(defn draw-circle [app mouse-possition]
  (let [{:keys [circle pen position ]} app
        center (first circle)]
    (if (and (= pen :down) center) (assoc-in app [:circle]
                                             [(mapv cd/snap-round center) (cd/snap-round (n/distance (n/c center) (n/c mouse-possition)))])
                                   app)))



(defrecord Poly [d])

(defprotocol Command
  (process-command [command app-state]))

(defn draw [app mouse-possition]
  (let [{:keys [draw-circle? ]} app]
    (if draw-circle? (draw-circle app mouse-possition) (draw-poly app mouse-possition))))

(extend-protocol Command
  Poly
  (process-command [{d :d} app]
    (draw app d)))
    ;(draw-circle app d)))




