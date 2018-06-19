(ns pickaseat.ver01.tables.table-actions
  (:use [com.rpl.specter :only [select transform setval FIRST LAST ALL keypath filterer srange comp-paths compiled-select collect-one compiled-setval]])
  (:require
    [pickaseat.ver01.tables.table-utils :as table-utils]
    [pickaseat.ver01.data.common :as common]
    [pickaseat.ver01.data.table_data :as table-data]))


(defn reset-seats [id tables]
  (let [rs (:rs (tables id))]
    (swap! table-data/tables-state assoc-in [:tables id :rs] nil)
    (doseq [idrs (map key rs)]
      (let [ids (filterv #(= % id)
                         (select [(keypath idrs) :rs FIRST FIRST] tables))]
        (doseq [idm ids]
          (swap! table-data/tables-state update-in [:tables idrs :rs] #(dissoc % idm)))))))

(defn remove-seats [close id]
  (let [cs (->> (partition 4 close) (map #(zipmap [:w :h :id :dir] %)) (group-by :id) (map #(first (val %))))]
    (doseq [c cs]
      (let [id1 (:id c)
            d (rest (name (:dir c)))
            d1 (keyword (first d))
            d2 (keyword (second d))]
        (doall
          (swap! table-data/tables-state update-in [:tables id :rs] #(update-in % [id1] (fn [x] (set (conj x d2)))))
          (swap! table-data/tables-state update-in [:tables id1 :rs] #(update-in % [id] (fn [x] (set (conj x d1))))))))))



(defn move-table [selected-tables]
  (fn [x-current y-current start-xy tables-data ctrl]
    (let [tables-collection (into (vals tables-data) (:borders @table-data/base-settings))
          common-data @common/data
          {:keys [selected show active offset]} (:selection @table-data/tables-state)
          [x-bcr y-bcr] (:bcr-tables common-data)
          [x-start y-start] start-xy
          sel? (> (count selected-tables) 1)
          result (mapv (fn [selected-tables]
                         (let [[id [xp yp]]  selected-tables
                               x-new-corr (- x-current (- x-start xp))
                               y-new-corr (- y-current (- y-start yp))
                               table-my (first (filterv #(= (:id %) id) tables-collection))
                               tables-other (filterv #(not= (:id %) id) tables-collection)
                               {:keys [x y width height hide-stools block]} table-my
                               x-new (if (pos? x-new-corr) x-new-corr 0)
                               y-new (if (pos? y-new-corr) y-new-corr 0)
                               table-xy (assoc table-my :x x-new :y y-new :rect-right (+ x-new width) :rect-bottom (+ y-new height))
                               tables-collision (if sel? (into [] (remove #((set selected) (:id %))) tables-other) tables-other)
                               block-new (doall
                                           (for [table tables-collision
                                                 :let [dir (table-utils/collides-with table table-xy)]
                                                 :when dir]
                                             dir))
                               colision-new (set (mapv (comp val first) block-new))
                               close (when (and (not-empty block-new)  (not sel?))
                                       (let [close-all (first (remove empty? (mapv #(table-utils/close-table % table-my)
                                                                                   (filterv #(colision-new (:id %)) tables-collision))))
                                             closest (first (sort-by #(min (last %)) close-all))]
                                         closest))]
                           (println (filterv #(colision-new (:id %)) tables-collision))
                           {:id          id
                            :block-new   block-new
                            :show        show
                            :block       block
                            :active      active
                            :hide-stools hide-stools
                            :x           (common/tables-snap x)
                            :y           (common/tables-snap y)
                            :x-new       (common/tables-snap x-new)
                            :y-new       (common/tables-snap y-new)
                            :slected-ids selected
                            :sel?        sel?
                            :width       (:width table-my)
                            :height      (:height table-my)
                            :table-xy    table-xy
                            :close       close}))
                       selected-tables)
          test-collision (seq (flatten (mapv :block-new result)))
          update-data (atom {})]
      (do
        (mapv (fn [x]
                (let [{:keys [id x-new y-new show block hide-stools block-new sel? width height close]} x
                      [x-close y-close] close]
                  ;(println close)
                  (when block (swap! update-data assoc-in [id [:tables id :block]] [x-new y-new]))
                  (when-not (and sel? test-collision block)
                    (when  close
                      (swap! update-data #(-> %
                                              (assoc-in [id [:tables id :x]] x-close)
                                              (assoc-in [id [:tables id :y]] y-close)
                                              (assoc-in [id [:tables id :rect-bottom]] (+ y-close height))
                                              (assoc-in [id [:tables id :rect-right]] (+ x-close width)))))
                    (do
                      (when (or show hide-stools)
                        (swap! update-data #(-> %
                                                (assoc-in [id [:selection :show]] false)
                                                (assoc-in [id [:selection :active]] false)
                                                (assoc-in [id [:tables id :hide-stools]] true))))
                      (if test-collision
                        (when (not-empty block-new)
                          (swap! update-data assoc-in [id [:tables id :block]] [x-new y-new])
                          (aset js/document "body" "style" "cursor" "not-allowed"))
                        (do (aset js/document "body" "style" "cursor" "move")
                            (swap! update-data #(-> %
                                                    (assoc-in [id [:tables id :block]] nil)
                                                    (assoc-in [id [:tables id :x]] x-new)
                                                    (assoc-in [id [:tables id :y]] y-new)
                                                    (assoc-in [id [:tables id :rect-right]] (+ x-new width))
                                                    (assoc-in [id [:tables id :rect-bottom]] (+ y-new height))))))))))
              result)
        (when (not (and test-collision selected))
          (swap! update-data #(let [x-sel (common/tables-snap  (- x-current x-bcr (:x offset)))
                                    y-sel (common/tables-snap  (- y-current y-bcr (:y offset)))
                                    x1-sel (common/tables-snap  (- x-current x-bcr (:x1 offset)))
                                    y1-sel (common/tables-snap  (- y-current y-bcr (:y1 offset)))
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


