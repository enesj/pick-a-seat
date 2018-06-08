(ns pickaseat.ver01.data.common-data
  (:require [reagent.core :as r]))
;"rgb(225,222,210)"

(def data
  (r/atom {:svg []
           :w   2000
           :h   2000
           :snap 5
           :wide-snap-faktor 10
           :grid-back-color "rgba(255,255,255,1)"
           :grid-color "rgba(200,200,200,1)"
           :snap-width 0.7
           :restaurant "Mala pivnica"
           :hall "Velika sala"}))

