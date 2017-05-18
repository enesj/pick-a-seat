(ns pickaseat.ver01.data.floor-data
  (:use [com.rpl.specter :only [FIRST LAST ALL  filterer  comp-paths]])
  (:require
    [reagent.core :as r]))

(def base-settings
  {:end-point-style        {:r 15 :fill "rgba(255,255,255,0.1)" :stroke "orange" :stroke-dasharray "5, 5"}
   :start-point-style      {:r 15 :fill "rgba(255,255,255,0.1)" :stroke "green" :stroke-dasharray "5, 5"}
   :connection-point-style {:r 2 :fill "rgba(0,0,0,0.8)"}
   :opacity                {:high 0.9 :low 0.1}
   :history-length         15})

(def init-turtle
  {:position    [0 0]
   :heading     :drawing
   :pen         :up
   :polyline    []
   :line        [[0 0] [0 0]]
   :line-angle  0
   :snap-points []
   :snap-xs     []
   :snap-ys     []
   :cut-poly    []
   :cut-line    []
   :end         false
   :scale       1})

(defn initial-app-state []
  {:mode      :drawing
   :tables    false
   :turtle    init-turtle
   :figures   {}
   :start     []
   :opacity   {:high 0.9 :low 0.1}
   ;:base       base
   ;:resolution resolution
   :selection {:start    {:x 0, :y 0}
               :end      {:x1 0, :y1 0}
               :selected []
               :offset   {}}})

(def init-floor-state
  (initial-app-state))

(def data (r/atom init-floor-state))

(def history {:drawing (r/atom {:performed [init-floor-state] :recalled [] :tables  false})
              :editing (r/atom {:performed [init-floor-state] :recalled [] :tables  false})})

(def specter-paths
  {:sel-end          (comp-paths :selection :end ALL LAST)
   :sel-start        (comp-paths :selection :start ALL LAST)
   :selected         (comp-paths :selection :selected)
   :selection-offset (comp-paths :selection :offset)
   :selection-end    (comp-paths :selection :end)
   :selection-start  (comp-paths :selection :start)
   :polygon          (comp-paths :figures ALL LAST :polygon ALL ALL)
   :zoom             (comp-paths :scale :zoom)
   :all              (comp-paths ALL ALL)
   :all-last         (comp-paths ALL LAST)})