(ns pickaseat.ver01.data.common-data
  (:require [reagent.core :as r]))

(def data
  (r/atom {:svg []
           :w   2000
           :h   2000
           :snap 5
           :snap-width 0.5
           :restaurant "Mala pivnica"
           :hall "Velika sala"}))

(defn snap-round [x]
  (let [snap (:snap @data)]
    (* (Math/round (/ x snap)) snap)))

(defn snap-positions [snap limit]
  (let [positions (iterate (partial + snap) snap)]
    ( take-while #(< % limit ) positions)))

(defn snap-lines-h [snap h-limit v-limit stroke-width]
  (into [:g]
     (for [y (snap-positions snap v-limit)]
       [:line (merge {:x1 0 :y1 y :x2 h-limit :y2 y :key y}
                     {:style {:stroke "rgb(255,255,255)" :stroke-width (if (= (mod y (* 5 snap)) 0) (* 2 stroke-width) stroke-width)}})])))

(defn snap-lines-v [snap h-limit v-limit stroke-width]
  (into [:g]
     (for [x (snap-positions snap h-limit)]
       [:line (merge {:x1 x :y1 0 :x2 x :y2 v-limit :key x}
                     {:style {:stroke "rgb(255,255,255)" :stroke-width (if (= (mod x (* 5 snap)) 0) (* 2 stroke-width) stroke-width)}})])))

(defn snap-lines-horizontal []
  (let [data @data]
    (snap-lines-h (Math/round (:snap data)) (:w data) (:h data) (:snap-width data))))

(defn snap-lines-vertical []
  (let [data @data]
    (snap-lines-v (Math/round (:snap data)) (:w data) (:h data) (:snap-width data))))

(def filters
  [:defs
   [:filter {:id "s1"}
    [:feGaussianBlur {:in "SourceAlpha" :result "blurOut" :stdDeviation "2"}]
    [:feOffset {:in "blurOut" :result "offsetBlurOut" :dx 2 :dy 2}]
    [:feMerge
     [:feMergeNode {:in "offsetBlurOut"}]
     [:feMergeNode {:in "SourceGraphic"}]]]
   [:filter {:id "s2"}
    [:feOffset :dx "20" :dy "20"]]])