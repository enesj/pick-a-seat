(ns pickaseat.ver01.tables.table-actions
  (:use [com.rpl.specter :only [select transform setval FIRST LAST ALL keypath filterer srange comp-paths compiled-select collect-one compiled-setval]])
  (:require
    [pickaseat.ver01.tables.table-utils :as table-utils]
    [pickaseat.ver01.data.common :as common]
    [pickaseat.ver01.data.table_data :as table-data]))

(defn reset-seats [id tables]
  (let [rs (:rs (tables id))]
    (swap! table-data/tables-state assoc-in [:tables id :rs] nil)
    (doseq [idrs (mapv key rs)]
      (let [ids (filterv #(= % id)
                         (select [(keypath idrs) :rs FIRST FIRST] tables))]
        (doseq [idm ids]
          (swap! table-data/tables-state update-in [:tables idrs :rs] #(dissoc % idm)))))))

(defn remove-seats [close id]
  (let [cs (->> (partition 4 close) (mapv #(zipmap [:w :h :id :dir] %)) (group-by :id) (map #(first (val %))))]
    (doseq [c cs]
      (let [id1 (:id c)
            d (rest (name (:dir c)))
            d1 (keyword (first d))
            d2 (keyword (second d))]
        (swap! table-data/tables-state update-in [:tables id :rs] #(update-in % [id1] (fn [x] (set (conj x d2)))))
        (swap! table-data/tables-state update-in [:tables id1 :rs] #(update-in % [id] (fn [x] (set (conj x d1)))))))))


(defn move-table [selected-tables svg-root]
  (fn [x-current y-current start-xy tables-data ctrl]
    (let [tables-collection (into (vals tables-data) (:borders @table-data/base-settings))
          common-data @common/data
          {:keys [selected show active offset]} (:selection @table-data/tables-state)
          bcr (common/get-bcr svg-root)
          [x-bcr y-bcr] [(.-left bcr) (.-top bcr)]
          snap (:snap-tables common-data)
          [x-start y-start] start-xy
          sel? (> (count selected-tables) 1)
          result (mapv (fn [ids]
                         (let [id (first ids)
                               [xp yp] (second ids)
                               x-new-corr (- x-current (- x-start xp))
                               y-new-corr (- y-current (- y-start yp))
                               table-my (first (filterv #(= (:id %) id) tables-collection))
                               tables-other (filterv #(not= (:id %) id) tables-collection)
                               {:keys [x y width height hide-stools block]} table-my
                               x-new (if (pos? x-new-corr) x-new-corr 0)
                               y-new (if (pos? y-new-corr) y-new-corr 0)
                               table-x (assoc table-my :x x-new :rect-right (+ x-new width))
                               table-y (assoc table-my :y y-new :rect-bottom (+ y-new height))
                               table-xy (assoc table-my :x x-new :y y-new :rect-right (+ x-new width) :rect-bottom (+ y-new height))
                               tables-collision (if sel? (into [] (remove #((set selected) (:id %))) tables-other) tables-other)
                               direction-xy (doall
                                              (for [table tables-collision
                                                    :let [dir (table-utils/collides-with table table-xy)]
                                                    :when dir]
                                                dir))
                               direction1 (when (and (not sel?) direction-xy)
                                            (doall
                                              (for [table tables-collision
                                                    :let [dir (table-utils/collides-with table table-x table-y)]
                                                    :when dir]
                                                dir)))
                               direction (when direction1 (if (or (and (some #(= :x %) direction1)
                                                                       (some #(= :y %) direction1))
                                                                  (some #(= :xy %) direction1))
                                                            :xy
                                                            (first direction1)))
                               x-move (if (= :x direction) x x-new)
                               y-move (if (= :y direction) y y-new)]
                           {:id          id
                            :dir         direction
                            :dirxy       direction-xy
                            :show        show
                            :block block
                            :active      active
                            :hide-stools hide-stools
                            :x           (common/tables-snap snap x)
                            :y           (common/tables-snap snap y)
                            :x-new       (common/tables-snap snap x-new)
                            :x-move      (common/tables-snap snap x-move)
                            :y-new       (common/tables-snap snap y-new)
                            :y-move      (common/tables-snap snap y-move)
                            :slected-ids selected
                            :sel?        sel?
                            :width       (common/tables-snap snap (:width table-my))
                            :height      (common/tables-snap snap (:height table-my))
                            :table-xy    table-xy}))
                       selected-tables)
          test-block (seq (flatten (mapv :dirxy result)))
          update-data (atom {})]

      (do
        (mapv (fn [x]
                (let [{:keys [id x-new y-new x-move y-move dir show block hide-stools dirxy sel? width height]} x]
                  (when (and (seq dirxy) block) (swap! update-data assoc-in [id [:tables id :block]] [x-new y-new]))
                  (when-not (and sel? test-block)
                    (if (or show hide-stools)
                      (do
                        (swap! update-data #(-> %
                                                (assoc-in [id [:selection :show]] false)
                                                (assoc-in [id [:selection :active]] false)
                                                (assoc-in [id [:tables id :hide-stools]] true)))))
                    (if (seq dirxy)
                      (do
                        (swap! update-data assoc-in [id [:tables id :block]] [x-new y-new])
                        (if (not= :xy dir)
                          (do
                            (swap! update-data #(-> %
                                                    (assoc-in [id [:tables id :x]] x-move)
                                                    (assoc-in [id [:tables id :y]] y-move)
                                                    (assoc-in [id [:tables id :rect-right]] (+ x-move width))
                                                    (assoc-in [id [:tables id :rect-bottom]] (+ y-move height))))
                            (aset js/document "body" "style" "cursor" "move"))
                          (aset js/document "body" "style" "cursor" "not-allowed")))
                      (do (aset js/document "body" "style" "cursor" "move")
                          (swap! update-data #(-> %
                                                  (assoc-in [id [:tables id :block]] nil)
                                                  (assoc-in [id [:tables id :x]] x-new)
                                                  (assoc-in [id [:tables id :y]] y-new)
                                                  (assoc-in [id [:tables id :rect-right]] (+ x-new width))
                                                  (assoc-in [id [:tables id :rect-bottom]] (+ y-new height)))))))))
              result)
        ;(swap! update-data assoc-in [id [:tables id :block]] [x-new y-new])))) !!!!! _?????
        (when (not (and test-block selected))
          (swap! update-data #(let [x-sel (- (+ x-current (.-pageXOffset js/window) x-bcr) (:x offset))
                                    y-sel (- (+ y-current (.-pageYOffset js/window) y-bcr) (:y offset))
                                    x1-sel (- (+ x-current (.-pageXOffset js/window) x-bcr) (:x1 offset))
                                    y1-sel (- (+ y-current (.-pageYOffset js/window) y-bcr) (:y1 offset))
                                    x-sel (if (pos? x-sel) x-sel 0)
                                    y-sel (if (pos? y-sel) y-sel 0)
                                    x1-sel (if (pos? x1-sel) x1-sel 0)
                                    y1-sel (if (pos? y1-sel) y1-sel 0)]
                                (-> %
                                    (assoc-in [1 [:selection :start]] {:x x-sel
                                                                       :y y-sel})
                                    (assoc-in [1 [:selection :end]] {:x x1-sel
                                                                     :y y1-sel})))))
        (swap! table-data/tables-state (fn [x] (reduce #(assoc-in %1 (first %2) (second %2)) x
                                                       (compiled-select (:all table-data/specter-paths)
                                                                        (mapv vec (compiled-select (:all-last table-data/specter-paths) @update-data))))))))))


