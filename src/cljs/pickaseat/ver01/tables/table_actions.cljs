(ns pickaseat.ver01.tables.table-actions
  (:use [com.rpl.specter :only [select transform setval FIRST LAST ALL keypath filterer srange comp-paths compiled-select collect-one compiled-setval]])
  (:require
    [pickaseat.ver01.tables.table-utils :as table-utils]
    [pickaseat.ver01.data.common :as common]
    [pickaseat.ver01.data.table_data :as table-data]))

(defn move-table [selected-tables]
  (fn [x-current y-current start-xy tables-data ctrl]
    (let [tables-collection (into (vals tables-data) (:borders @table-data/base-settings))
          common-data @common/data
          {:keys [selected show active offset]} (:selection @table-data/tables-state)
          [x-bcr y-bcr] (:bcr-tables common-data)
          snap (:snap-tables common-data)
          [x-start y-start] start-xy
          sel? (> (count selected-tables) 1)
          result (mapv (fn [selected-tables]
                         (let [id (first selected-tables)
                               [xp yp] (second selected-tables)
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
                                             dir))]
                           {:id          id
                            :block-new   block-new
                            :show        show
                            :block       block
                            :active      active
                            :hide-stools hide-stools
                            :x           (common/tables-snap snap x)
                            :y           (common/tables-snap snap y)
                            :x-new       (common/tables-snap snap x-new)
                            :y-new       (common/tables-snap snap y-new)
                            :slected-ids selected
                            :sel?        sel?
                            :width       (:width table-my)
                            :height      (:height table-my)
                            :table-xy    table-xy}))
                       selected-tables)
          test-collision (seq (flatten (mapv :block-new result)))
          update-data (atom {})]
      (do
        (mapv (fn [x]
                (let [{:keys [id x-new y-new show block hide-stools block-new sel? width height]} x]
                  (when block (swap! update-data assoc-in [id [:tables id :block]] [x-new y-new]))
                  (when-not (and sel? test-collision block)
                    (if (or show hide-stools)
                      (do
                        (swap! update-data #(-> %
                                                (assoc-in [id [:selection :show]] false)
                                                (assoc-in [id [:selection :active]] false)
                                                (assoc-in [id [:tables id :hide-stools]] true)))))
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
                                                  (assoc-in [id [:tables id :rect-bottom]] (+ y-new height)))))))))
              result)
        (when (not (and test-collision selected))
          (swap! update-data #(let [x-sel (common/tables-snap snap (- x-current x-bcr (:x offset)))
                                    y-sel (common/tables-snap snap (- y-current y-bcr (:y offset)))
                                    x1-sel (common/tables-snap snap (- x-current x-bcr (:x1 offset)))
                                    y1-sel (common/tables-snap snap (- y-current y-bcr (:y1 offset)))
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


