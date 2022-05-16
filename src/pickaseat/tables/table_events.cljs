(ns pickaseat.tables.table-events
  (:use [com.rpl.specter :only [select transform setval FIRST LAST ALL keypath filterer srange comp-paths compiled-select collect-one compiled-setval]])
	(:require
		[pickaseat.tables.table-utils :as table-utils]
		[pickaseat.tables.selecetion-analize :as tables-analize]
		[pickaseat.data.table-data :as table-data]
		[pickaseat.data.common :as common]))



(defn undo []
  (let [history @table-data/history
        {:keys [performed recalled]} history
        butlast-performed (vec (butlast performed))]
    (when (pos? (count performed))
      (reset! table-data/history {:recalled  (->> (last performed)
																									(conj recalled)
																									vec)
																	:performed butlast-performed,
																	:layout    (:layout (deref table-data/history))})
      (reset! table-data/tables-state (last butlast-performed))
			(table-data/settings-pos (* (/ (.-innerWidth js/window) 1000) (.-devicePixelRatio js/window)) false))))


(defn redo []
  (let [history @table-data/history
        {:keys [performed recalled]} history
        butlast-recalled (vec (butlast recalled))]
    (when (pos? (count recalled))
      (reset! table-data/history {:recalled butlast-recalled,
																	:performed (->> (last recalled)
																								  (conj performed)
																								  vec ),
																	:layout (:layout (deref table-data/history))})
      (reset! table-data/tables-state (last recalled))
			(table-data/settings-pos (* (/ (.-innerWidth js/window) 1000) (.-devicePixelRatio js/window)) false))))



(defn mouse-up []
  (fn [e]
    (js/console.log "UPP")
    (.preventDefault e)
    (let [history @table-data/history
          {:keys [performed ]} history
           shift-performed (if (= (count performed) (:history-length @table-data/base-settings))
                             (vec (rest performed))
														 performed)]

      (swap! table-data/tables-state assoc-in [:selection :active] false)
      (if  (or (not-empty (:selected (:selection @table-data/tables-state)))
							 (not-empty (:ids @tables-analize/selected-current)))
        (swap! table-data/tables-state assoc-in [:selection :show] true)
        (swap! table-data/tables-state assoc-in [:selection :show] true))
      (when (not= (:tables @table-data/tables-state)
						   (:tables (last shift-performed)))
        (reset! table-data/history {:performed (conj shift-performed (compiled-setval (:hide-stools table-data/specter-paths)
																																											false
																																											@table-data/tables-state))
                                    :recalled  []
                                    :layout    (:layout history)})))))

(defn table-events [selection tables svg-root]
  (let [{:keys [tabale-selected selected selection-active selection-offset selection-end selection-start selection-show]} table-data/specter-paths
        [[x-sel-start y-sel-start] [x-sel-end y-sel-end]] (table-utils/start-end (:start selection) (:end selection))]
    {:mouse-down (fn [e]
                   (.preventDefault e)
                   ;(js/console.log e)
                   (let [
                         bcr (common/get-bcr svg-root)
                         [x-bcr y-bcr] [(.-left bcr) (.-top bcr)]
                         x-current (- (.-clientX e) x-bcr)
                         y-current (- (.-clientY e) y-bcr)
                         start {:x x-current :y y-current}
                         direction (filterv boolean (doall (for [table
                                                                 (conj (into (vals tables) (:borders @table-data/base-settings))
                                                                   {:id         :1
                                                                    :x          x-sel-start :y y-sel-start
                                                                    :width      (- x-sel-end x-sel-start (- 0))
                                                                    :height     (- y-sel-end y-sel-start (- 0))
                                                                    :rect-right x-sel-end :rect-bottom y-sel-end})
                                                                 :let [dir (table-utils/collides-sel table {:id          1
                                                                                                            :x           x-current
                                                                                                            :y           y-current
                                                                                                            :width       1
                                                                                                            :height      1
                                                                                                            :rect-right  (inc x-current)
                                                                                                            :rect-bottom (inc y-current)} 0)]
                                                                 :when (not= false dir)]
                                                             dir)))]
                     (swap! common/data assoc-in [:bcr-tables] [x-bcr y-bcr])
                     (if (empty? direction)
                       (do
                         (reset! tables-analize/selected-current tables-analize/selected-init)
                         (swap! table-data/tables-state #(->> %
                                                           (compiled-setval selection-start start)
                                                           (compiled-setval selection-end start)
                                                           (compiled-setval selection-active true)
                                                           (compiled-setval selected nil)
                                                           (compiled-setval tabale-selected false))))

                       (if (some #(= :1 %) direction)
                         (swap! table-data/tables-state #(->> %
                                                           (compiled-setval selection-offset {:x  (- x-current (:x (:start selection)))
                                                                                              :y  (- y-current (:y (:start selection)))
                                                                                              :x1 (- x-current (:x (:end selection)))
                                                                                              :y1 (- y-current (:y (:end selection)))})
                                                           (compiled-setval selection-active false)))
                         (do (reset! tables-analize/selected-current tables-analize/selected-init)
                             (swap! table-data/tables-state
                               #(compiled-setval tabale-selected false
                                  (->> %
                                    ;(compiled-setval selection-show false)
                                    (compiled-setval selected nil)
                                    (compiled-setval selection-end nil)
                                    (compiled-setval selection-start nil)))))))))
     :mouse-move (fn [e]
                   (.preventDefault e)
                   (let [
                         [x-bcr y-bcr] (:bcr-tables @common/data)
                         x-current (- (.-clientX e) x-bcr)
                         y-current (- (.-clientY e) y-bcr)
                         start (:start selection)
                         end {:x x-current :y y-current}
                         [[x y] [x1 y1]] (table-utils/start-end start end)]
                     (when (:active selection)
                       (let [sel (filterv boolean (mapv (fn [table]
                                                          (table-utils/collides-sel-active table {:id         1 :x x :y y
                                                                                                  :width      (- x1 x) :height (- y1 y)
                                                                                                  :rect-right x1 :rect-bottom y1} 0))
                                                    (vals tables)))
                             select-true (comp-paths :tables ALL LAST #(some (set sel) [(:id %)]) :selected)]
                         (swap! table-data/tables-state #(->> %
                                                           (compiled-setval selection-show true)
                                                           (compiled-setval tabale-selected false)
                                                           (compiled-setval select-true true)
                                                           (compiled-setval selection-end end)
                                                           (compiled-setval selected sel)))))

                     (when (and (not (:show selection)) (seq (:selected selection)))
                       (if (not= @tables-analize/selected-current {:current-state 0 :ids [] :tables {}})
                         (reset! tables-analize/selected-current tables-analize/selected-init)))))
     :key-down   (fn [e]
                   (js/console.log e)
                   (.preventDefault e)
                   (case (.-which e)
                     7 (swap! table-data/tables-state assoc-in [:snap] true)
                     nil))

     :key-up     (fn [e]
                   (.preventDefault e)
                   (case (.-which e)
                     7 (swap! table-data/tables-state assoc-in [:snap] false)
                     nil))}))

