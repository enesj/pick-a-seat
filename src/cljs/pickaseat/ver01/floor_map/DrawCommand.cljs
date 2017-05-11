(ns pickaseat.ver01.floor-map.DrawCommand
  (:require
    [complex.number :as n]
    [pickaseat.ver01.floor-map.settings :as s]))

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