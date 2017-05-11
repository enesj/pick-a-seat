(ns pickaseat.ver01.floor-map.core
  (:require
   [devcards.core]
   [complex.number :as n :refer [zero one i negative-one negative-i infinity add sub mult div]]
   [pickaseat.ver01.floor-map.testboard :as floor]
   [reagent.core :as reagent])
  (:require-macros
   [reagent.ratom :as ratom :refer [reaction]]
   [devcards.core :as dc :refer [defcard deftest defcard-rg defcard-doc]]))

(enable-console-print!)

(defn floor-mode []
  (let [root [floor/draw-floor]]
    {:events nil
     :root root}))