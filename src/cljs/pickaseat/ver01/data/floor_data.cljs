(ns pickaseat.ver01.data.floor-data
  (:use [com.rpl.specter :only [FIRST LAST ALL filterer comp-paths]])
  (:require
    [reagent.core :as r]))

(def base-settings
  {:end-point-style        {:r 15 :fill "rgba(255,255,255,0.1)" :stroke "orange" :stroke-dasharray "5, 5"}
   :new-point-style        {:r 15 :fill "rgba(255,255,255,0.1)" :stroke "green" :stroke-dasharray "5, 5"}
   :start-point-style      {:r 15 :fill "rgba(255,255,255,0.1)" :stroke "violet" :stroke-dasharray "5, 5"}
   :circle-point-style     {:fill "rgba(255,255,255,0.1)" :stroke "black"}
   :resize-point-style     {:r 5 :fill "rgba(255,255,255,0.1)" :stroke "blue"}
   :poly-points-style      {:r 5 :fill "rgba(255,255,255,0.1)" :stroke "green"}
   :connection-point-style {:r 2 :fill "rgba(0,0,0,0.8)"}
   :opacity                {:high 0.5 :low 0.1}
   :history-length         15})

(def init-turtle
  {:start-time      nil
   :draw-circle?    false
   :position        [-10 -10]
   :pen             :up
   :polyline        [[-10 -10] [-10 -10]]
   :circle          []
   :shadow?         false
   :shadow-polyline []
   :shadow-raw      []
   :line            [[-10 -10] [-10 -10]]
   :line-angle      0
   :snap-points     []
   :snap-xs         []
   :snap-ys         []
   :cut-poly        []
   :cut-line        []
   :end             false
   :scale           1})


(def floor-init-data
  {:mode         :drawing
   :tables       false
   :draw-circle? false
   :turtle       init-turtle
   :figures      {}
   :start        []
   ;:opacity   {:high 0.5 :low 0.1}
   ;:base       base
   ;:resolution resolution
   :selection    {:start    {:x 0, :y 0}
                  :end      {:x 0, :y 0}
                  :selected []
                  :offset   {}}})

(def floor-state (r/atom floor-init-data))


(def floor-states-data  (r/atom {:performed [floor-init-data] :recalled []}))


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