(ns pickaseat.ver01.tables.table-actions
  (:use [com.rpl.specter :only [select transform setval FIRST LAST ALL keypath filterer srange comp-paths compiled-select collect-one compiled-setval]])
  (:require
    [reagent.core :as r]
    [goog.events :as events]
    [pickaseat.ver01.tables.tables-components :as c]
    [pickaseat.ver01.data.table_data :as td]
    [pickaseat.ver01.tables.util :as u]
    [pickaseat.ver01.data.common-data :as cd]))


(defn reset-seats [id tables]
  (let [rs (:rs (tables id))]
    (swap! td/tables-state assoc-in [:tables id :rs] nil)
    (doseq [idrs (map key rs)]
      (let [ids (filterv #(= % id)
                         (select [(keypath idrs) :rs FIRST FIRST] tables))]
        (doseq [idm ids]
          (swap! td/tables-state update-in [:tables idrs :rs] #(dissoc % idm)))))))

(defn remove-seats [close id]
  (let [cs (->> (partition 4 close) (map #(zipmap [:w :h :id :dir] %)) (group-by :id) (map #(first (val %))))]
    (doseq [c cs]
      (let [id1 (:id c)
            d (rest (name (:dir c)))
            d1 (keyword (first d))
            d2 (keyword (second d))]
        (doall
          (swap! td/tables-state update-in [:tables id :rs] #(update-in % [id1] (fn [x] (into #{} (conj x d2)))))
          (swap! td/tables-state update-in [:tables id1 :rs] #(update-in % [id] (fn [x] (into #{} (conj x d1))))))))))


(defn move-table [sel-top-lefts]
  (fn [x-org y-org start tables ctrl]
    (let [tables-collection (into (vals tables) (:borders @td/settings-base))
          {:keys [selected show active offset]} (:selection @td/tables-state)
          {:keys [tabale-selected selectected-path selection-active selection-offset selection-end selection-start selection-show]} td/specter-paths-data
          [x y] (mapv - (:svg @cd/common-data))
          x-current (+ x-org (.-pageXOffset js/window) x)
          y-current (+ y-org (.-pageYOffset js/window) y)
          [x-start y-start] start
          sel? (> (count sel-top-lefts) 1)
          result (doall (for [ids sel-top-lefts]
                          (let [id (first ids)
                                [xp yp] (second ids)
                                x-new-corr (- x-org (- x-start xp))
                                y-new-corr (- y-org (- y-start yp))
                                table-my (first (filterv #(= (:id %) id) tables-collection))
                                tables-other (filterv #(not= (:id %) id) tables-collection)
                                {:keys [x y width height rs hide-stools block]} table-my
                                x-new (if (pos? x-new-corr) x-new-corr 0)
                                y-new (if (pos? y-new-corr) y-new-corr 0)
                                table-x (assoc table-my :x x-new :rect-right (+ x-new width))
                                table-y (assoc table-my :y y-new :rect-bottom (+ y-new height))
                                table-xy (assoc table-my :x x-new :y y-new :rect-right (+ x-new width) :rect-bottom (+ y-new height))
                                tables-collision (if sel? (into [] (remove #((set selected) (:id %))) tables-other) tables-other)
                                direction-xy (doall
                                               (for [table tables-collision
                                                     :let [dir (u/collides-with table table-xy)]
                                                     :when dir]
                                                 dir))
                                direction1 (when (and (not sel?) direction-xy)
                                             (doall
                                               (for [table tables-collision
                                                     :let [dir (u/collides-with table table-x table-y)]
                                                     :when dir]
                                                 dir)))
                                direction (when direction1 (if (or (and (some #(= :x %) direction1) (some #(= :y %) direction1)) (some #(= :xy %) direction1)) :xy (first direction1)))
                                x-move (if (= :x direction) x x-new)
                                y-move (if (= :y direction) y y-new)
                                close (if ctrl
                                        (let [close1 (mapv #(u/close-table % table-my) tables-collision)
                                              close1 (filterv boolean (flatten close1))]
                                          (if (and (not-empty close1) (empty (for [table tables-collision
                                                                                   :let [dir (u/collides-with table table-xy)]
                                                                                   :when (not= false dir)]
                                                                               dir)))
                                            close1)))]
                            {:id       id :dir direction :dirxy direction-xy :rs rs :show show :active active :hide-stools hide-stools
                             :x        (Math/round x) :y (Math/round y)
                             :x-new    (Math/round x-new) :x-move (Math/round x-move)
                             :y-new    (Math/round y-new) :y-move (Math/round y-move)
                             :block    block :ctrl ctrl :close close :slected-ids selected :sel? sel?
                             :width    (Math/round (:width table-my)) :height (Math/round (:height table-my))
                             :table-xy table-xy})))
          test-block (seq (flatten (mapv :dirxy result)))
          update-data (atom {})]
      (do
        (doall (for [x result]
                 (let [{:keys [id x-new y-new x-move y-move dir show active hide-stools rs block close slected-ids dirxy sel? width height]} x
                       [x-close y-close] close]
                   (when-not (and sel? test-block)
                     (if (and ctrl (seq close))
                       (do (swap! update-data #(-> %
                                                   (assoc-in [id [:tables id :x]] x-close)
                                                   (assoc-in [id [:tables id :y]] y-close)
                                                   (assoc-in [id [:tables id :rect-bottom]] (+ y-close height))
                                                   (assoc-in [id [:tables id :rect-right]] (+ x-close width))))
                           (remove-seats close id))
                       (do
                         (if (and rs dirxy) (reset-seats id tables))
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
                   (swap! update-data assoc-in [id [:tables id :block]] [x-new y-new]))))
        (when (not (and test-block selected))
          (swap! update-data #(let [x-sel (- (+ x-org (.-pageXOffset js/window) x) (:x offset))
                                    y-sel (- (+ y-org (.-pageYOffset js/window) y) (:y offset))
                                    x1-sel (- (+ x-org (.-pageXOffset js/window) x) (:x1 offset))
                                    y1-sel (- (+ y-org (.-pageYOffset js/window) y) (:y1 offset))
                                    x-sel (if (pos? x-sel) x-sel 0)
                                    y-sel (if (pos? y-sel) y-sel 0)
                                    x1-sel (if (pos? x1-sel) x1-sel 0)
                                    y1-sel (if (pos? y1-sel) y1-sel 0)]
                                (-> %
                                    (assoc-in [1 [:selection :start]] {:x x-sel
                                                                       :y y-sel})
                                    (assoc-in [1 [:selection :end]] {:x1 x1-sel
                                                                     :y1 y1-sel})))))
        (swap! td/tables-state (fn [x] (doall (reduce #(assoc-in %1 (first %2) (second %2)) x
                                                      (compiled-select (:all td/specter-paths-data)
                                                                       (mapv vec (compiled-select (:all-last td/specter-paths-data) @update-data)))))))))))
