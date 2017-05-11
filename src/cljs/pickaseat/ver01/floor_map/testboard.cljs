(ns pickaseat.ver01.floor-map.testboard
  (:require
    [devcards.core]
    [pickaseat.ver01.floor-map.pixie :as p]
    [pickaseat.ver01.floor-map.components :as svg]
    [pickaseat.ver01.floor-map.settings :as s]
    [complex.number :as n]
    [complex.geometry :as g]
    [reagent.core :as reagent]
    [devtools.core :as devtools]
    [devtools.toolbox :as toolbox]
    [pickaseat.ver01.floor-map.edit-events :as eev]
    [pickaseat.ver01.floor-map.DrawCommand :as dc]
    [goog.events :as events]
    [cljs.core.async :as async :refer [>! <! put! chan alts! timeout]])
  (:import [goog.events EventType])
  (:require-macros
    [devcards.core :as dc :refer [defcard deftest defcard-rg defcard-doc]]
    [cljs.core.async.macros :refer [go]]))


(enable-console-print!)
(devtools/install! [:formatters])

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

(defn line-color [angle]
  (condp (fn [a b] (zero? (mod b a))) angle
    45 "black"
    30 "gray"
    ;15 "green"
    "lightgray"))

(defn get-bcr [svg-root]
  (-> svg-root
      reagent/dom-node
      .getBoundingClientRect))

(defn pixie? [command]
  (satisfies? p/Command command))

(defn exec [commands]
  (-> list (apply commands) flatten vec))

(defn undo []
  (exec [(dc/->Undo)]))

(defn redo []
  (exec [(dc/->Redo)]))

(defn draw-poly [x y]
  (exec [(dc/->Penup)]))

(defn draw-start [x y]
  (exec [(dc/->Pendown)]))

(defn draw-line [x y snap]
  (exec [(dc/->Drawline) (p/->Forward [x y])]))


(defn run-program [chan program]
  (go
    (doseq [command program]
      (cond
        (instance? dc/Pause command) (<! (timeout (:delay command)))
        :else (>! chan command)))))


(defn update-state
  "return new state for given command"
  [state command]
  (let [{:keys [turtle polyline squares pen line]} state]
    (if (pixie? command)
      (let [new-turtle (p/process-command command turtle)]
        (assoc-in state [:turtle] new-turtle))
      (dc/process-command command state))))

(defn process-channel [turtle-channel app-state]
  (go (loop []
        (let [command (<! turtle-channel)]
          (swap! app-state #(update-state % command))
          (recur)))))

(defn move-point [svg-root p]
  (fn [x y]
    (let [bcr (get-bcr svg-root)]
      nil)))

(defn draw-menu [app-state ui-channel]
  (let [mode (:mode @app-state)
        history @s/history
        undo? (not-empty (rest (:performed history)))
        redo? (not-empty (:recalled history))]
    [:svg {:width "400px" :height "30px" :font-family "Courier New" :fill "blue"}
     [:text {:opacity       0.8
             :on-mouse-down (fn [e] (.preventDefault e)
                              (swap! app-state assoc-in [:mode]
                                     (if (= mode :drawing) :editing :drawing)))
             :x             10 :y 20}
      (name mode)]
     [:text {:opacity       (if undo? 0.8 0.1)
             :on-mouse-down (fn [e] (.preventDefault e)
                              (run-program ui-channel (undo)))
             :x             90 :y 20} "Undo"]
     [:text {:opacity       (if redo? 0.8 0.1)
             :on-mouse-down (fn [e] (.preventDefault e)
                              (run-program ui-channel (redo)))
             :x             140 :y 20} "Redo"]]))

(defn draw-figures [figures polyline opacity]
  (for [figure (sort-by key figures)]
    (let [fig (first (val figure))
          opacity (if (> (count polyline) 1) (:low opacity) (:high polyline))]
      (case (key fig)
        :polygon (svg/polygon
                   {:key     (key figure)
                    :stroke  "black"
                    :fill    "white"
                    :opacity opacity
                    :filter  (if (odd? (first figure)) "url(#s1)" "url(#s2)")}
                   {:key       (rand 1000)
                    :stroke    "black"
                    :fill      "white"
                    :opacity   opacity
                    :transform "translate(0 0)"}
                   (val fig))))))

(defn draw-snap-points [snap-points line connection-point-style]
  [:g
   (svg/circle (last line) 0 connection-point-style)
   (for [snap-point snap-points]
     [:g {:key (rand 1000)}
      (svg/circle snap-point 1 connection-point-style)
      (svg/color-line "orange" [snap-point (last line)] {:stroke-dasharray "5, 5"})])])


(defn draw-floor []
  (let [data s/data
        {:keys [start-point-style end-point-style connection-point-style opacity]} s/base-settings
        {:keys [turtle base resolution figures mode selection]} @data
        {:keys [snap-points line-angle line polyline pen cut-poly cut-line]} turtle
        ui-channel (chan)
        _ (process-channel ui-channel data)
        x-bcr (atom 0)
        y-bcr (atom 0)
        xx-bcr @x-bcr]
    [:div
     (draw-menu data ui-channel)
     [:svg
      {
       :width         resolution
       :height        resolution
       :ref           #(when %
                         (reset! x-bcr (.-left (.getBoundingClientRect %)))
                         (reset! y-bcr (.-top (.getBoundingClientRect %))))
       :stroke        "black"
       :on-mouse-down (fn [e]
                        (.preventDefault e)
                        (if (= mode :drawing)
                          (run-program ui-channel
                                       (draw-start (- (.-clientX e) @x-bcr)
                                                   (- (.-clientY e) @y-bcr)))
                          (swap! data assoc-in [:selection :start] {:x (- (.-clientX e) @x-bcr) :y (- (.-clientY e) @y-bcr)})))
       :on-mouse-up   (fn [e]
                        (.preventDefault e)
                        (if (= mode :drawing)
                          (run-program ui-channel
                                       (draw-poly (- (.-clientX e) @x-bcr)
                                                  (- (.-clientY e) @y-bcr)))
                          (swap! data assoc-in [:selection :start] {:x 0 :y 0})))
       :on-mouse-move (fn [e]
                        (.preventDefault e)
                        (if (= mode :drawing)
                          (run-program ui-channel
                                       (draw-line
                                         (- (.-clientX e) @x-bcr)
                                         (- (.-clientY e) @y-bcr)
                                         (if (not-empty snap-points) true false)))))}
      filters
      [:g
       (when-not (empty? figures)
         (draw-figures figures polyline opacity))
       (when-not (empty? polyline)
         (apply svg/polyline "lines" {} polyline))
       (when-not (empty? cut-poly)
         (apply svg/polyline "lines" {:style {:stroke "red"}} cut-poly))
       (when-not (empty? cut-line)
         (apply svg/polyline "lines" {:style {:stroke "red"}} cut-line))
       (if (= pen :down)
         [:g
          (svg/color-line (line-color (:line-angle turtle)) line {})
          (when (not-empty snap-points)
            (draw-snap-points snap-points line connection-point-style))
          (when (:end turtle)
            [:g
             (svg/circle (first polyline) 0 end-point-style)
             (svg/circle (first polyline) 0 connection-point-style)
             (svg/circle (last line) 0 connection-point-style)])]
         [:g
          (svg/circle (last polyline) 0 start-point-style)
          (svg/circle (last polyline) 0 connection-point-style)])]]]))


;(defcard-rg floor-plan
;            (fn [app _] [draw-floor])
;            s/data
;            {:component-did-mount (events/listen js/window EventType.MOUSEUP (eev/mouse-up))})
;             ;:inspect-data true})

