(ns pickaseat.ver01.floor-map.pixie
  (:require
    [complex.vector :as v]
    [complex.number :as n]
    [complex.geometry :as g]
    [cljs.core.match :refer-macros [match]]))


(defrecord Forward [d])


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


(defprotocol Command
  (process-command [command app-state]))


(defn snap-test [polyline possition]
  (if (> (count polyline) 2)
    (let [polyline-nolast (map #(map Math/round %)  (butlast polyline))
          x-match (filter #(= (Math/round (first possition)) (first %)) polyline-nolast)
          y-match (filter #(= (Math/round (second possition)) (second %)) polyline-nolast)]
      [(into x-match y-match) [(first x-match) (first y-match)]])))


(defn move [app mouse-possition]
  (let [{:keys [position heading scale polyline line pen snap-xs snap-ys cut-poly cut-line]} app
        constrain (if (= pen :down) (constrain-line mouse-possition (partition 2 1 polyline) line position ) [mouse-possition nil])
        [constrain-x constrain-y] (first constrain)
        constrain-params (second constrain)
        constrain-angle (:angle constrain-params)
        cut-poly-new (:cut-poly constrain-params)
        cut-line-new (:cut-line constrain-params)
        snap-test (snap-test polyline position)
        snap-points (first snap-test)
        snap-x  (some #(if (<= (- % 5)  constrain-x (+ % 5)) %) snap-xs)
        snap-y  (some #(if (<= (- % 5)  constrain-y  (+ % 5)) %) snap-ys)
        constrain-position  [(if snap-x snap-x constrain-x)
                             (if snap-y snap-y constrain-y)]]
    (match heading
           :east (update-in app [:position] #(v/sum % [(* scale mouse-possition) 0]))
           :west (update-in app [:position] #(v/sum % [(* scale mouse-possition -1) 0]))
           :north (update-in app [:position] #(v/sum % [0 (* scale mouse-possition -1)]))
           :south (update-in app [:position] #(v/sum % [0 (* scale mouse-possition)]))
           :drawing (as-> app $
                          (if (or (not cut-poly) (not cut-poly-new)) (assoc-in $ [:cut-poly] cut-poly-new) $)
                          (if (or (not cut-line) (not cut-line-new)) (assoc-in $ [:cut-line] cut-line-new) $)
                          (if (and constrain-angle (= pen :down))
                            (assoc-in $ [:line-angle] constrain-angle)
                            $)
                          (assoc-in $ [:snap-points]  snap-points)
                          (assoc-in $ [:position] constrain-position)))))


(extend-protocol Command
  Forward
  (process-command [{d :d} app]
    (move app d)))




