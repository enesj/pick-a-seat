(ns pickaseat.ver01.tables.table-events
  (:use [com.rpl.specter :only [select transform setval FIRST LAST ALL keypath filterer srange comp-paths compiled-select collect-one compiled-setval]])
  (:require
    [pickaseat.ver01.data.table-data :as table-data]
    [pickaseat.ver01.tables.table-utils :as table-utils]
    [pickaseat.ver01.tables.tables-analize :as tables-analize]))

(defn mouse-up []
  (fn [e]
    (.preventDefault e)
    (let [history @table-data/history
          {:keys [performed recalled]} history
           shift-performed (if (= (count performed) (:history-length @table-data/base-settings))
                             (vec (rest performed)) performed)]

      (swap! table-data/tables-state assoc-in [:selection :active] false)
      (if  (or (not-empty (:selected (:selection @table-data/tables-state))) (not-empty (:ids @tables-analize/selected-current)))
        (swap! table-data/tables-state assoc-in [:selection :show] true)
        (swap! table-data/tables-state assoc-in [:selection :show] false))
      (when (not= (:tables @table-data/tables-state) (:tables (last shift-performed)))
        ;(js/console.log (:tables @td/tables-state) (:tables (last shift-performed)))
        (reset! table-data/history {:performed (conj shift-performed (compiled-setval (:hide-stools table-data/specter-paths) false @table-data/tables-state))
                                    :recalled          [] :layout (:layout @table-data/history)})))))

(defn table-events [ selection tables x y x-sel-s y-sel-s x-sel-e y-sel-e]
  (let [{:keys [tabale-selected selected selection-active selection-offset selection-end selection-start selection-show hide-stools]} table-data/specter-paths]
    {:mouse-down (fn [e]
                   (.preventDefault e)
                   (let [x-current (+ (.-clientX e) (.-pageXOffset js/window) x)
                         y-current (+ (.-clientY e) (.-pageYOffset js/window) y)
                         start {:x x-current :y y-current}
                         end {:x1 x-current :y1 y-current}
                         direction (filterv boolean (doall (for [table
                                                                 (conj (into (vals tables) (:borders @table-data/base-settings))
                                                                       {:id         :1 :x x-sel-s :y y-sel-s :width (- x-sel-e x-sel-s (- 0)) :height (- y-sel-e y-sel-s (- 0))
                                                                        :rect-right (+ x-sel-e 0) :rect-bottom (+ y-sel-e 0)})
                                                                 :let [dir (table-utils/collides-sel table {:id 1 :x x-current :y y-current :width 1 :height 1
                                                                                                            :rect-right   (+ x-current 1) :rect-bottom (+ y-current 1)} 0)]
                                                                 :when (not= false dir)]
                                                             dir)))]
                     (if (empty? direction)
                       (do
                         (reset! tables-analize/selected-current {:current-state 0 :ids [] :tables {} :start {} :end {} :del false})
                         (swap! table-data/tables-state #(->> %
                                                              (compiled-setval selection-start start)
                                                              (compiled-setval selection-end end)
                                                              (compiled-setval selection-active true)
                                                              (compiled-setval selected nil)
                                                              (compiled-setval tabale-selected false))))

                       (if (some #(= :1 %) direction)
                         (swap! table-data/tables-state #(->> %
                                                              (compiled-setval selection-offset {:x  (- x-current (:x (:start selection)))
                                                                                                 :y  (- y-current (:y (:start selection)))
                                                                                                 :x1 (- x-current (:x1 (:end selection)))
                                                                                                 :y1 (- y-current (:y1 (:end selection)))})
                                                              (compiled-setval selection-active false)))
                         (do (reset! tables-analize/selected-current {:current-state 0 :ids [] :tables {} :start {} :end {} :del false})
                             (swap! table-data/tables-state
                                    #(compiled-setval tabale-selected false
                                                      (->> %
                                                           (compiled-setval selection-show false)
                                                           (compiled-setval selected nil)
                                                           (compiled-setval selection-end nil)
                                                           (compiled-setval selection-start nil)))))))))
     :mouse-move (fn [e]
                   (.preventDefault e)
                   (let [x-current (+ (.-clientX e) (.-pageXOffset js/window) x)
                         y-current (+ (.-clientY e) (.-pageYOffset js/window) y)
                         start (:start selection)
                         end {:x1 x-current :y1 y-current}
                         [[x y] [x1 y1]] (table-utils/start-end start end)]

                     (when (:active selection)
                       (let [sel (filterv boolean (doall (for [table (vals tables)]
                                                           (table-utils/collides-sel-active table {:id 1 :x x :y y
                                                                                                   :width        (- x1 x) :height (- y1 y)
                                                                                                   :rect-right   x1 :rect-bottom y1} 0))))
                             select-true (comp-paths :tables ALL LAST #(some (set sel) [(:id %)]) :selected)]
                         (swap! table-data/tables-state #(->> % (compiled-setval selection-show true)
                                                              (compiled-setval tabale-selected false)
                                                              (compiled-setval select-true true)
                                                              (compiled-setval selection-end end)
                                                              (compiled-setval selected sel)))))

                     (when (and (not (:show selection)) (seq (:selected selection)))
                       (if (not= @tables-analize/selected-current {:current-state 0 :ids [] :tables {}})
                         (reset! tables-analize/selected-current {:current-state 0 :ids [] :tables {} :start {} :end {} :del false})))))
     :key-down   (fn [e]
                   (.preventDefault e)
                   (case (.-which e)
                     7 (swap! table-data/tables-state assoc-in [:snap] true)
                     nil))

     :key-up     (fn [e]
                   ;(.preventDefault e)
                   (case (.-which e)
                     7 (swap! table-data/tables-state assoc-in [:snap] false)
                     nil))}))

