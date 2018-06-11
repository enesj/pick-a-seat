(ns pickaseat.ver01.data.common
  (:require [reagent.core :as r]))
;"rgb(225,222,210)"

(def data
  (r/atom {:bcr-tables       []
           :w                2000
           :h                2000
           :snap-layout      5
           :snap-tables      10
           :wide-snap-faktor 10
           :grid-back-color  "rgba(255,255,255,1)"
           :grid-color       "rgba(200,200,200,1)"
           :snap-width       0.7
           :restaurant       "Mala pivnica"
           :hall             "Velika sala"}))



(defn layout-snap [x]
  (let [snap (:snap-layout @data)]
    (* (Math/round (/ x snap)) snap)))

(defn tables-snap [snap x]
    (* (Math/round (/ x snap)) snap))

(defn get-bcr [svg-root]
  (-> svg-root
      r/dom-node
      .getBoundingClientRect))