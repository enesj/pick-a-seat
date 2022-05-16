(ns pickaseat.floor-map.floor-draw-actions
  (:require
    [pickaseat.complex.vector :as complex-vector]
    [pickaseat.complex.number :as complex-number]
    [pickaseat.complex.geometry :as complex-geometry]
    [cljsjs.svg-intersections]
    [cljs.core.match :refer-macros [match]]
    [pickaseat.intersections :as intersections]
    [pickaseat.data.common :as common]))



(defn segment-collision [new-line polyline]
  (let [test-polylines (butlast polyline)]
    (when (not-empty polyline)
      ((comp peek vec)
       (sort-by #(complex-number/distance (complex-number/c (last %)) (complex-number/c (last test-polylines)))
                 (filter #(true? (first %))
                         (mapv (fn [poly]
                                 (let [coll-point (map Math/round (complex-geometry/intersection new-line (vec poly)))
                                       poly-x (->> poly (map first) (map Math/round) sort)
                                       new-line-x (->> new-line (map first) (map Math/round) sort)
                                       poly-y (->> poly (map second) (map Math/round) sort)
                                       new-line-y (->> new-line (map second) (map Math/round) sort)
                                       coll (and (and (apply <= (interpose (first coll-point) poly-x))
                                                      (apply <= (interpose (first coll-point) new-line-x)))
                                                 (and (apply <= (interpose (second coll-point) poly-y))
                                                      (apply <= (interpose (second coll-point) new-line-y))))]
                                   [coll poly coll-point]))
                               test-polylines)))))))


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
  (let [angle (complex-vector/angle (apply map - line))
        angle-deg (complex-number/rad->deg angle)]
    (angle-snap angle-deg c-angle)))

(defn two-lines-angle-degree [line1-angle line2-angle]
  (- 180 (-> (- line1-angle line2-angle ) (Math/abs ) (- 180)  (Math/abs))))

(defn constrain-line [mouse-possition polyline line position]
  (if (and (not= mouse-possition (first line)) (not-empty line))
    (let [
          new-line [mouse-possition (first line)]
          new-line-radius (complex-vector/len (apply map - new-line))
          new-line-angle (line-angle new-line 5)
          last-poly-angle (when (pos? (count polyline)) (line-angle (vec (last polyline)) 1))
          last-poly-test (if (not-empty polyline)
                           (< (two-lines-angle-degree new-line-angle last-poly-angle) 15)
                           false)
          new-position  (->> (complex-number/complex-polar new-line-radius new-line-angle) vec (mapv #(int (second %))) (mapv + (second new-line)))
          segment-collison-test (segment-collision (assoc-in line [1] new-position) polyline)
          [segment-collison? collision-line collison-point] (when-not last-poly-test segment-collison-test)
          cut-polylne (if segment-collison? (as-> (drop-while #(not= collision-line %) polyline) $
                                                  (into (mapv first (butlast $)) (last $))
                                                  (assoc-in $ [0] collison-point)))
          cut-line (if cut-polylne (assoc-in line [1] collison-point))]
      (if-not (or segment-collison? last-poly-test)
        [new-position {:angle new-line-angle}]
        [position {:cut-poly-new cut-polylne :cut-line-new cut-line}]))
    [mouse-possition nil]))


(defn draw-shadow [mouse-possition polyline app]
  (let [
        poly (partition 2 1 polyline)
        angle-first (complex-vector/angle (map - (second (first poly)) (ffirst poly)))
        shadow-width (* (complex-number/distance (complex-number/c mouse-possition) (complex-number/c (ffirst poly)))
                        (Math/sin (- (complex-vector/angle (mapv - mouse-possition (ffirst poly))) angle-first)))
        new-polyline (partition 2 (flatten poly))
        move #(complex-number/polar->rect shadow-width (+ (complex-vector/angle (map - (second %) (first %))) (/ complex-number/PI 2)))
        new-poly (partition 2 (flatten (map #(repeat 2 %) (mapv move poly))))
        raw-shadow (mapv #(mapv + %1 %2) new-poly new-polyline)
        first-shadow (mapv vec (partition 2 1 (mapv vec (partition 2 raw-shadow))))
        intersections-first (mapv #(complex-geometry/intersection (first %) (second %)) first-shadow)
        intersections-second (vec (remove (fn [x](some #(not= % %) x ))  intersections-first))
        shadow (mapv #(mapv common/layout-snap %)
                     (concat (vector (first new-polyline)) (vector (first raw-shadow)) intersections-second
                             (vector (last raw-shadow)) (vector (last new-polyline))))
        all-self-intersections (intersections/self-poly-intersections shadow)
        no-self-intersection (> 4 (count (remove false? all-self-intersections)))
        border-test (intersections/line-rect-intersections (flatten shadow) [5 5 1990 1990])
        app (assoc-in app [:position] mouse-possition)]
    ;(js/console.log no-self-intersection (zero? border-test) (> 3 (intersections/poly-poly-intersection shadow poly)))
    (if (and no-self-intersection (zero? border-test) (> 3 (intersections/poly-poly-intersection shadow poly)))
      (assoc-in app [:shadow-polyline] shadow)
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
      (draw-shadow mouse-possition polyline  app)
      (let [constrain-line (if (= pen :down) (constrain-line mouse-possition (partition 2 1 polyline) line position) [mouse-possition nil])
            [[constrain-x constrain-y] {:keys [angle cut-poly-new  cut-line-new ]}] constrain-line
            snap-xs (mapv first polyline)
            snap-ys (mapv second polyline)
            snap-x (some #(if (<= (- % 10) constrain-x (+ % 10)) %) snap-xs)
            snap-y (some #(if (<= (- % 10) constrain-y (+ % 10)) %) snap-ys)
            snap-points (when (or snap-x snap-y) (snap-test polyline position))
            constrain-snap [(or snap-x constrain-x)
                            (or snap-y constrain-y)]]
        ;no-borders-intersection (= 0 (intersections/line-rect-intersections (flatten [(last polyline) constrain-snap])  [5 5 1990 1990]))]
        ;(js/console.log   "cl" app)
        (as-> app $
              (if (or (not cut-poly) (not cut-poly-new)) (assoc-in $ [:cut-poly] cut-poly-new) $)
              (if (or (not cut-line) (not cut-line-new)) (assoc-in $ [:cut-line] cut-line-new) $)
              (if (and angle (= pen :down))
                (assoc-in $ [:line-angle] angle)
                $)
              (assoc-in $ [:snap-points] snap-points)
              (assoc-in $ [:position] (mapv common/layout-snap constrain-snap)) $)))))

(defn draw-circle [app mouse-possition]
  (let [{:keys [circle pen position ]} app
        center (first circle)]
    (if (and (= pen :down) center) (assoc-in app [:circle]
                                             [(mapv common/layout-snap center) (common/layout-snap (complex-number/distance (complex-number/c center) (complex-number/c mouse-possition)))])
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




