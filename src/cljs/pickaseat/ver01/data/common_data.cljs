(ns pickaseat.ver01.data.common-data
  (:require [reagent.core :as r]))

(def common-data
  (r/atom {:svg []
           :w   2000
           :h   2000
           :restaurant "Mala pivnica"
           :hall "Velika sala"}))


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