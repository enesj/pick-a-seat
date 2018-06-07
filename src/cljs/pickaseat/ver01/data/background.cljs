(ns pickaseat.ver01.data.background
  (:require [pickaseat.ver01.data.common-data :refer [data]]))

(defn snap-positions [snap limit]
  (let [positions (iterate (partial + snap) snap)]
    ( take-while #(< % limit ) positions)))

(defn snap-lines-h [snap h-limit v-limit stroke-width]
  (let [data @data
        {:keys [grid-color wide-snap-faktor]} data]
    (into [:g]
          (mapv (fn [y]
                  [:line (merge {:x1 0 :y1 y :x2 h-limit :y2 y :key y}
                                {:style {:stroke grid-color :stroke-width (if (zero? (mod y (* wide-snap-faktor snap))) (* 2 stroke-width) stroke-width)}})])
                (snap-positions snap v-limit)))))

(defn snap-lines-v [snap h-limit v-limit stroke-width]
  (let [data @data
        {:keys [grid-color wide-snap-faktor]} data]
    (into [:g]
          (mapv (fn [x]
                  [:line (merge {:x1 x :y1 0 :x2 x :y2 v-limit :key x}
                                {:style {:stroke grid-color :stroke-width (if (zero? (mod x (* wide-snap-faktor snap))) (* 2 stroke-width) stroke-width)}})])
                (snap-positions snap h-limit)))))

(defn snap-lines-horizontal []
  (let [data @data]
    (snap-lines-h (Math/round (:snap data)) (:w data) (:h data) (:snap-width data))))

(defn snap-lines-vertical []
  (let [data @data]
    (snap-lines-v (Math/round (:snap data)) (:w data) (:h data) (:snap-width data))))