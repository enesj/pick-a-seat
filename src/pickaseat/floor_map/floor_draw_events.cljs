(ns pickaseat.floor-map.floor-draw-events
  (:require
    [pickaseat.complex.number :as complex-number]
    [pickaseat.data.floor-data :as floor-data]
    [cljs.core.async :as async :refer [>! <! put! chan alts! timeout]]
    [pickaseat.floor-map.floor-draw-actions :as floor-draw-actions])
  (:require-macros
    [cljs.core.async.macros :refer [go]]))

(defrecord Penup [])
(defrecord Pendown [d])
(defrecord Pause [delay])
(defrecord Draw [])
(defrecord Undo [])
(defrecord Redo [])


(defprotocol DrawCommand
  (process-command [command app-state]))

(extend-protocol DrawCommand
  Penup
  (process-command [_ app]
    (let [{:keys [turtle ]} app
          {:keys [pen polyline position cut-poly shadow? shadow-polyline circle draw-circle? start-time]} turtle
          {:keys [performed]} @floor-data/floor-states-data
          start (complex-number/distance (complex-number/c position) (complex-number/c (last polyline)))
          end (complex-number/distance (complex-number/c position) (complex-number/c (first polyline)))
          test-r (:r (:end-point-style floor-data/base-settings))
          shift-performed (if (= (count performed) (:history-length floor-data/base-settings))
                            (vec (rest performed))
                            performed)]
      (if (= pen :down)
        (as-> app $
              (assoc-in $ [:turtle :pen] :up)
              (if draw-circle?
                (-> $
                  (assoc-in [:figures (inc (count (:figures app)))] {:circle circle})
                  (assoc-in [:turtle :circle] []))
                (if shadow?
                  (-> $
                      (assoc-in [:turtle :shadow?] false)
                      (assoc-in [:figures (inc (count (:figures app)))]  {:polygon (into polyline (next (reverse (next shadow-polyline))))})
                      (assoc-in [:turtle :line] [])
                      (assoc-in [:turtle :polyline] [])
                      (assoc-in [:turtle :shadow-polyline] []))
                  (if (seq cut-poly)
                    (-> $
                        (assoc-in [:turtle :line] [position position])
                        (assoc-in [:figures (inc (count (:figures app)))] {:polygon cut-poly})
                        (assoc-in [:turtle :polyline] []))
                    (if (and (< end test-r) (> (count polyline) 2))
                      (-> $
                          (assoc-in [:turtle :line] [position position])
                          (assoc-in [:figures (inc (count (:figures app)))] {:polygon polyline})
                          (assoc-in [:turtle :polyline] []))
                      (if (> start test-r)
                        (update-in $ [:turtle :polyline] #(conj % position))
                        (if (and (= 1 (count polyline)) (> (- (.getTime (js/Date.)) start-time) 500))
                          (assoc-in $ [:mode] :editing)
                          $))))))
              (do
                (reset!  floor-data/floor-states-data {:performed (conj shift-performed $) :recalled []})
                $))
        app)))
  Pendown
  (process-command [{d :d} app]
    (let [{:keys [turtle]} app
          {:keys [pen polyline position draw-circle?]} turtle
          start (complex-number/distance (complex-number/c position) (complex-number/c (last polyline)))
          end (complex-number/distance (complex-number/c position) (complex-number/c (first polyline)))
          test-r (:r (:new-point-style floor-data/base-settings))]
      (if (= pen :up)
        (as-> app $
              (assoc-in $ [:turtle :pen] :down)
              (if draw-circle?
                (assoc-in $ [:turtle :circle] (if draw-circle? [d 0] []))
                (if (< start test-r)
                  (-> $
                      (assoc-in [:turtle :circle] (if draw-circle? [d 0] []))
                      (assoc-in [:turtle :shadow?] false)
                      (assoc-in [:turtle :line] [(last polyline) (last polyline)]))
                  (if (< end test-r)
                    (-> $
                        (assoc-in [:turtle :shadow?] true)
                        (assoc-in [:turtle :snap-points] []))
                    (-> $
                        (assoc-in [:turtle :shadow?] false)

                        (assoc-in [:turtle :line] [position [0 0]])
                        (assoc-in [:turtle :polyline] [position]))))))
        app)))
  Draw
  (process-command [_ app]
    (let [{:keys [turtle]} app
          {:keys [pen polyline position shadow? draw-circle?]} turtle
          end (complex-number/distance (complex-number/c position) (complex-number/c (first polyline)))
          test-r (:r (:end-point-style floor-data/base-settings))]
      (if (and (= pen :down) (not draw-circle?))
        (as-> app $
              (if (and (< end test-r) (not shadow?))
               (assoc-in $  [:turtle :end] true)
               (assoc-in $ [:turtle :end] false))
              (if-not shadow? (assoc-in $ [:turtle :line 1] position) $))
        app)))
  Undo
  (process-command [_ app]
    (let [
          ;{:keys [mode]} app
          {:keys [performed recalled ]} @floor-data/floor-states-data
          butlast-performed (vec (butlast performed))]
      (if (pos? (count performed))
        (do
          (reset!  floor-data/floor-states-data {:performed butlast-performed :recalled (vec (conj recalled (last performed)))})
          (assoc-in (last butlast-performed) [:turtle :pen] :up))
        app)))
  Redo
  (process-command [_ app]
    (let [
          ;{:keys [mode]} app
          {:keys [performed recalled ]} @floor-data/floor-states-data
          butlast-recalled (vec (butlast recalled))]
      (if (pos? (count recalled))
        (do
          (reset!  floor-data/floor-states-data {:performed (vec (conj performed (last recalled))) :recalled butlast-recalled})
          (assoc-in (last recalled) [:turtle :pen] :up))
        app))))


(defn exec [commands]
  (-> list (apply commands) flatten vec))

(defn undo []
  (exec [(->Undo)]))

(defn redo []
  (exec [(->Redo)]))

(defn draw-figure []
  (exec [(->Penup)]))

(defn draw-start [x y]
  (exec [(->Pendown [x y])]))

(defn draw-line [x y]
  (exec [(->Draw) (floor-draw-actions/->Poly [x y])]))


(defn run-program [chan program]
  (go
    (doseq [command program]
      (cond
        (instance? Pause command) (<! (timeout (:delay command)))
        :else (>! chan command)))))


(defn command? [command]
  (satisfies? floor-draw-actions/Command command))

(defn update-state
  "return new state for given command"
  [state command]
  (let [{:keys [turtle]} state]
    (if (command? command)
      (let [new-turtle (floor-draw-actions/process-command command turtle)]
        (assoc-in state [:turtle] new-turtle))
      (process-command command state))))

(defn process-channel [turtle-channel app-state]
  (go (loop []
        (let [command (<! turtle-channel)]
          (swap! app-state #(update-state % command))
          (recur)))))


