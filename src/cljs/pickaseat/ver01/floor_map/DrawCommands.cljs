(ns pickaseat.ver01.floor-map.DrawCommands
  (:require
    [complex.number :as n]
    [pickaseat.ver01.floor-map.settings :as s]
    [pickaseat.ver01.floor-map.components :as comps]
    [cljs.core.async :as async :refer [>! <! put! chan alts! timeout]]
    [pickaseat.ver01.floor-map.pixie :as p])
  (:require-macros
    [cljs.core.async.macros :refer [go]]))

(defrecord Penup [])
(defrecord Pendown [])
(defrecord Pause [delay])
(defrecord Drawline [])
(defrecord Undo [])
(defrecord Redo [])


(defprotocol DrawCommand
  (process-command [command app-state]))

(extend-protocol DrawCommand
  Penup
  (process-command [_ app]
    (let [{:keys [turtle]} app
          history @s/history
          {:keys [pen polyline position cut-poly ]} turtle
          {:keys [performed recalled]} history
          near (n/distance (n/c position) (n/c (last polyline)))
          end (n/distance (n/c position) (n/c (first polyline)))
          end-test (:r (:end-point-style s/base-settings))
          performed (if (= (count performed) 0) (conj performed app) performed)
          shift-performed (if (= (count performed) (:history-length s/base-settings))
                            (vec (rest performed)) performed)]
      (if (= pen :down)
        (do
          (reset! s/history {:performed (conj shift-performed (dissoc app :history)) :recalled []})
          (as-> app $
                (assoc-in $ [:turtle :pen] :up)
                (if cut-poly
                  (-> $
                      (assoc-in [:turtle :line] [position position])
                      (assoc-in [:figures (inc (count (:figures app)))] {:polygon cut-poly})
                      (assoc-in [:turtle :polyline] []))
                  (if (and (< end end-test) (> (count polyline) 2))
                    (-> $
                        (assoc-in [:turtle :line] [position position])
                        ;(assoc-in [:turtle :candidate-line] [position position])
                        (assoc-in [:figures (inc (count (:figures app)))] {:polygon polyline})
                        (assoc-in [:turtle :polyline] []))
                    (if (> near 10)
                      (-> $
                          (assoc-in [:turtle :line] [position position])
                          (update-in [:turtle :polyline] #(conj % position))
                          (update-in [:turtle :snap-xs] #(conj % (first position)))
                          (update-in [:turtle :snap-ys] #(conj % (second position))))

                      (-> $
                          (assoc-in [:turtle :line] [])
                          (assoc-in [:turtle :polyline] [])))))))
        app)))
  Pendown
  (process-command [_ app]
    (let [{:keys [turtle]} app
          {:keys [pen polyline position line]} turtle
          near (n/distance (n/c position) (n/c (last polyline)))
          start-test (:r (:start-point-style s/base-settings))]
      (if (= pen :up)
        (as-> app $
              (assoc-in $ [:turtle :pen] :down)
              (if (> near start-test)
                (-> $
                    (assoc-in [:turtle :line] [position position])
                    (assoc-in [:turtle :polyline] [position]))
                (-> $
                    (assoc-in [:turtle :line] [(last polyline) (last polyline)]))))
        app)))
  Drawline
  (process-command [_ app]
    (let [{:keys [turtle]} app
          {:keys [pen polyline position line ]} turtle
          end (n/distance (n/c position) (n/c (first polyline)))
          end-test (:r (:end-point-style s/base-settings))]
      (if (= pen :down)
        (-> (if (< end end-test)
              (assoc-in app [:turtle :end] true)
              (assoc-in app [:turtle :end] false))
            (assoc-in [:turtle :line 1] position))
        app)))
  Undo
  (process-command [_ app]
    (let [history @s/history
          {:keys [performed recalled]} history
          butlast-performed (vec (butlast performed))]
      (if (> (count performed) 1)
        (do
          (reset! s/history {:performed butlast-performed :recalled (vec (conj recalled (last performed)))})
          (-> (last butlast-performed)
              (assoc-in [:turtle :pen] :up)))
        app)))
  Redo
  (process-command [_ app]
    (let [history @s/history
          {:keys [performed recalled]} history
          butlast-recalled (vec (butlast recalled))]
      (if (> (count recalled) 0)
        (do
          (reset! s/history {:performed (vec (conj performed (last recalled))) :recalled butlast-recalled})
          (-> (last recalled)
              (assoc-in [:turtle :pen] :up)))
        app))))



(defn exec [commands]
  (-> list (apply commands) flatten vec))

(defn undo []
  (exec [(->Undo)]))

(defn redo []
  (exec [(->Redo)]))

(defn draw-poly [x y]
  (exec [(->Penup)]))

(defn draw-start [x y]
  (exec [(->Pendown)]))

(defn draw-line [x y snap]
  (exec [(->Drawline) (p/->Forward [x y])]))


(defn run-program [chan program]
  (go
    (doseq [command program]
      (cond
        (instance? Pause command) (<! (timeout (:delay command)))
        :else (>! chan command)))))


(defn pixie? [command]
  (satisfies? p/Command command))

(defn update-state
  "return new state for given command"
  [state command]
  (let [{:keys [turtle polyline squares pen line]} state]
    (if (pixie? command)
      (let [new-turtle (p/process-command command turtle)]
        (assoc-in state [:turtle] new-turtle))
      (process-command command state))))

(defn process-channel [turtle-channel app-state]
  (go (loop []
        (let [command (<! turtle-channel)]
          (swap! app-state #(update-state % command))
          (recur)))))

(defn draw-menu [app-state ui-channel]
  (let [{:keys [mode tables]}  @app-state
        history @s/history
        undo? (not-empty (rest (:performed history)))
        redo? (not-empty (:recalled history))]
    [:svg {:width "400px" :height "30px" :font-family "Courier New" :fill "blue" :font-size "15"}
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
             :x             140 :y 20} "Redo"]
     [:text {:on-mouse-down (fn [e] (.preventDefault e)
                              (swap! app-state update-in [:tables] not))
             :x             200 :y 20} (if tables "hide(tables)" "show(tables)")]]))


(defn draw-figures [figures polyline opacity]
  (for [figure (sort-by key figures)]
    (let [fig (first (val figure))
          opacity (if (> (count polyline) 1) (:low opacity) (:high polyline))]
      (case (key fig)
        :polygon (comps/polygon
                   {:key     (key figure)
                    :stroke  "black"
                    :fill    "white"
                    :opacity opacity
                    :filter  "url(#s1)"}
                   ;{:key       (rand 1000)
                   ; :stroke    "black"
                   ; :fill      "white"
                   ; :opacity   opacity
                   ; :transform "translate(0 0)"}
                   (val fig))))))


(defn draw-snap-points [snap-points line connection-point-style]
  [:g
   (comps/circle (last line) 0 connection-point-style)
   (for [snap-point snap-points]
     [:g {:key (rand 1000)}
      (comps/circle snap-point 1 connection-point-style)
      (comps/color-line "orange" [snap-point (last line)] {:stroke-dasharray "5, 5"})])])