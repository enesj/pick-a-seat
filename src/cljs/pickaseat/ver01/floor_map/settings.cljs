(ns pickaseat.ver01.floor-map.settings
  (:require
    [reagent.core :as reagent]))

(def base-settings
  {:end-point-style        {:r 15 :fill "rgba(255,255,255,0.1)" :stroke "orange" :stroke-dasharray "5, 5"}
   :start-point-style      {:r 15 :fill "rgba(255,255,255,0.1)" :stroke "green" :stroke-dasharray "5, 5"}
   :connection-point-style {:r 2 :fill "rgba(0,0,0,0.8)"}
   :opacity                {:high 0.9 :low 0.1}
   :history-length         15})

(def init-turtle
  {:position           [0 0]
   :heading            :drawing
   :pen                :up
   :polyline           []
   :line               [[0 0] [0 0]]
   :line-angle         0
   :snap-points        []
   :snap-xs            []
   :snap-ys            []
   :cut-poly           []
   :cut-line           []
   :end                false
   :scale              1})

(defn initial-app-state [base resolution]
  {:mode       :drawing
   :turtle     init-turtle
   :figures    {}
   :start      []
   :opacity    {:high 0.9 :low 0.1}
   :base       base
   :resolution resolution
   :selection {:active false
               :show false
               :stop false
               :start  {:x 0, :y 0}
               :end    {:x1 0, :y1 0}
               :selected []
               :offset {}}})

(def init-floor-state
  (initial-app-state 0 800))

(def data (reagent/atom init-floor-state))

(def history (atom {:performed [] :recalled []}))