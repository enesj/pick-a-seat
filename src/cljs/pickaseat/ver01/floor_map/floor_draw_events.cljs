(ns pickaseat.ver01.floor-map.floor-draw-events
  (:require
    [complex.number :as n]
    [pickaseat.ver01.data.floor-data :as fd]
    [pickaseat.ver01.floor-map.floor-components :as comps]
    [cljs.core.async :as async :refer [>! <! put! chan alts! timeout]]
    [pickaseat.ver01.floor-map.floor-draw-actions :as da])
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
    (let [{:keys [turtle mode]} app
          history @(:history fd/floor-state)
          {:keys [pen polyline position cut-poly shadow? shadow-polyline circle draw-circle?]} turtle
          {:keys [performed]} history
          start (n/distance (n/c position) (n/c (last polyline)))
          end (n/distance (n/c position) (n/c (first polyline)))
          test-r (:r (:end-point-style fd/base-settings))
          shift-performed (if (= (count performed) (:history-length fd/base-settings))
                            (vec (rest performed)) performed)]
      ;(js/console.log circle)
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
                        (-> $
                            (assoc-in [:turtle :line] [position position])
                            (update-in [:turtle :polyline] #(conj % position)))
                        $)))))
              (do
                ;(println "up" shift-performed)
                (reset! (:history fd/floor-state) {:performed (conj shift-performed $) :recalled [] }) $))
        app)))
  Pendown
  (process-command [{d :d} app]
    (let [{:keys [turtle]} app
          {:keys [pen polyline position line circle draw-circle?]} turtle
          start (n/distance (n/c position) (n/c (last polyline)))
          end (n/distance (n/c position) (n/c (first polyline)))
          test-r (:r (:new-point-style fd/base-settings))]
      (if (= pen :up)
        (as-> app $
              (assoc-in $ [:turtle :pen] :down)
              (if draw-circle?
                (-> $ (assoc-in [:turtle :circle] (if draw-circle? [d 0] [])))
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

                        (assoc-in [:turtle :line] [position position])
                        (assoc-in [:turtle :polyline] [position]))))))
        app)))
  Draw
  (process-command [_ app]
    (let [{:keys [turtle]} app
          {:keys [pen polyline position shadow? draw-circle?]} turtle
          end (n/distance (n/c position) (n/c (first polyline)))
          test-r (:r (:end-point-style fd/base-settings))]
      (if (and (= pen :down) (not draw-circle?))
        (as-> app $
              (if (and (< end test-r) (not shadow?))
               (assoc-in $  [:turtle :end] true)
               (assoc-in $ [:turtle :end] false))
              (if-not shadow? (assoc-in $ [:turtle :line 1] position) $))
        app)))
  Undo
  (process-command [_ app]
    (let [{:keys [mode]} app
          history @(:history fd/floor-state)
          {:keys [performed recalled ]} history
          butlast-performed (vec (butlast performed))]
      (if (> (count performed) 0)
        (do
          (reset! (:history fd/floor-state) {:performed butlast-performed :recalled (vec (conj recalled (last performed)))})
          (-> (last butlast-performed)
              (assoc-in [:turtle :pen] :up)))
        app)))
  Redo
  (process-command [_ app]
    (let [{:keys [mode]} app
          history @(:history fd/floor-state)
          {:keys [performed recalled tables]} history
          butlast-recalled (vec (butlast recalled))]
      (if (> (count recalled) 0)
        (do
          (reset! (:history fd/floor-state) {:performed (vec (conj performed (last recalled))) :recalled butlast-recalled})
          (-> (last recalled)
              (assoc-in [:turtle :pen] :up)))
        app))))


(defn exec [commands]
  (-> list (apply commands) flatten vec))

(defn undo []
  (exec [(->Undo)]))

(defn redo []
  (exec [(->Redo)]))

(defn draw-poly []
  (exec [(->Penup)]))

(defn draw-start [x y]
  (exec [(->Pendown [x y])]))

(defn draw-line [x y]
  (exec [(->Draw) (da/->Poly [x y])]))


(defn run-program [chan program]
  (go
    (doseq [command program]
      (cond
        (instance? Pause command) (<! (timeout (:delay command)))
        :else (>! chan command)))))


(defn command? [command]
  (satisfies? da/Command command))

(defn update-state
  "return new state for given command"
  [state command]
  (let [{:keys [turtle]} state]
    (if (command? command)
      (let [new-turtle (da/process-command command turtle)]
        (assoc-in state [:turtle] new-turtle))
      (process-command command state))))

(defn process-channel [turtle-channel app-state]
  (go (loop []
        (let [command (<! turtle-channel)]
          (swap! app-state #(update-state % command))
          (recur)))))


