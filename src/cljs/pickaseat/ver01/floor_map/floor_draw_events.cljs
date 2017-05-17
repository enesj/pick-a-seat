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
          history @fd/history
          {:keys [pen polyline position cut-poly]} turtle
          {:keys [performed recalled tables]} history
          near (n/distance (n/c position) (n/c (last polyline)))
          end (n/distance (n/c position) (n/c (first polyline)))
          end-test (:r (:end-point-style fd/base-settings))
          shift-performed (if (= (count performed) (:history-length fd/base-settings))
                            (vec (rest performed)) performed)]
      (if (= pen :down)
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
                        (assoc-in [:turtle :polyline] [])))))
              (do (reset! fd/history {:performed (conj shift-performed $) :recalled [] :tables tables}) $))
        app)))
  Pendown
  (process-command [_ app]
    (let [{:keys [turtle]} app
          {:keys [pen polyline position line]} turtle
          near (n/distance (n/c position) (n/c (last polyline)))
          start-test (:r (:start-point-style fd/base-settings))]
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
          end-test (:r (:end-point-style fd/base-settings))]
      (if (= pen :down)
        (-> (if (< end end-test)
              (assoc-in app [:turtle :end] true)
              (assoc-in app [:turtle :end] false))
            (assoc-in [:turtle :line 1] position))
        app)))
  Undo
  (process-command [_ app]
    (let [history @fd/history
          {:keys [performed recalled tables]} history
          butlast-performed (vec (butlast performed))]
      (if (> (count performed) 0)
        (do
          (reset! fd/history {:performed butlast-performed :recalled (vec (conj recalled (last performed))) :tables tables})
          (-> (last butlast-performed)
              (assoc-in [:turtle :pen] :up)))
        app)))
  Redo
  (process-command [_ app]
    (let [history @fd/history
          {:keys [performed recalled tables]} history
          butlast-recalled (vec (butlast recalled))]
      (if (> (count recalled) 0)
        (do
          (reset! fd/history {:performed (vec (conj performed (last recalled))) :recalled butlast-recalled :tables tables})
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

(defn draw-start []
  (exec [(->Pendown)]))

(defn draw-line [x y]
  (exec [(->Drawline) (da/->Forward [x y])]))


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
  (let [{:keys [turtle polyline squares pen line]} state]
    (if (command? command)
      (let [new-turtle (da/process-command command turtle)]
        (assoc-in state [:turtle] new-turtle))
      (process-command command state))))

(defn process-channel [turtle-channel app-state]
  (go (loop []
        (let [command (<! turtle-channel)]
          (swap! app-state #(update-state % command))
          (recur)))))


