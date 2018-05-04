(ns pickaseat.ver01.tables.table-actions
  (:use [com.rpl.specter :only [select transform setval FIRST LAST ALL keypath filterer srange comp-paths compiled-select collect-one compiled-setval]])
  (:require
    [pickaseat.ver01.data.table-data :as table-data]
    [pickaseat.ver01.tables.table-utils :as table-utils]
    [pickaseat.ver01.data.common-data :as common-data]))

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


(defn move-table [sel-top-lefts]
  (fn [x-current y-current start-xy tables-data ctrl]
    (let [tables-collection (into (vals tables-data) (:borders (deref table-data/base-settings))) {:keys [selected show active offset]} (:selection (deref table-data/tables-state)) [x y] (mapv - (:svg (deref common-data/data))) [x-start y-start] start-xy sel? (> (count sel-top-lefts) 1) result (doall (for [ids sel-top-lefts] (let [id (first ids) [xp yp] (second ids) x-new-corr (- x-current (- x-start xp)) y-new-corr (- y-current (- y-start yp)) table-my (first (filterv (fn* [p1__2291410#] (= (:id p1__2291410#) id)) tables-collection)) tables-other (filterv (fn* [p1__2291412#] (not= (:id p1__2291412#) id)) tables-collection) {:keys [x y width height rs hide-stools block]} table-my x-new (if (pos? x-new-corr) x-new-corr 0) y-new (if (pos? y-new-corr) y-new-corr 0) table-x (assoc table-my :x x-new :rect-right (+ x-new width)) table-y (assoc table-my :y y-new :rect-bottom (+ y-new height)) table-xy (assoc table-my :x x-new :y y-new :rect-right (+ x-new width) :rect-bottom (+ y-new height)) tables-collision (if sel? (into [] (remove (fn* [p1__2291414#] ((set selected) (:id p1__2291414#)))) tables-other) tables-other) direction-xy (doall (for [table tables-collision :let [dir (table-utils/collides-with table table-xy)] :when dir] dir)) direction1 (when (and (not sel?) direction-xy) (doall (for [table tables-collision :let [dir (table-utils/collides-with table table-x table-y)] :when dir] dir))) direction (when direction1 (if (or (and (some (fn* [p1__2291416#] (= :x p1__2291416#)) direction1) (some (fn* [p1__2291418#] (= :y p1__2291418#)) direction1)) (some (fn* [p1__2291420#] (= :xy p1__2291420#)) direction1)) :xy (first direction1))) x-move (if (= :x direction) x x-new) y-move (if (= :y direction) y y-new) close (if ctrl (let [close1 (mapv (fn* [p1__2291422#] (table-utils/close-table p1__2291422# table-my)) tables-collision) close1 (filterv boolean (flatten close1))] (if (and (not-empty close1) (empty (for [table tables-collision :let [dir (table-utils/collides-with table table-xy)] :when (not= false dir)] dir))) close1)))] {:y (Math/round y), :block block, :dir direction, :x-move (Math/round x-move), :hide-stools hide-stools, :slected-ids selected, :x-new (Math/round x-new), :table-xy table-xy, :width (Math/round (:width table-my)), :close close, :ctrl ctrl, :rs rs, :y-new (Math/round y-new), :active active, :id id, :dirxy direction-xy, :sel? sel?, :x (Math/round x), :y-move (Math/round y-move), :height (Math/round (:height table-my)), :show show}))) test-block (seq (flatten (mapv :dirxy result))) update-data (atom {})] (doall (for [x result] (let [{:keys [id x-new y-new x-move y-move dir show hide-stools rs close dirxy sel? width height]} x [x-close y-close] close] (when-not (and sel? test-block) (if (and ctrl (seq close)) (do (swap! update-data (fn* [p1__2291424#] (-> p1__2291424# (assoc-in [id [:tables id :x]] x-close) (assoc-in [id [:tables id :y]] y-close) (assoc-in [id [:tables id :rect-bottom]] (+ y-close height)) (assoc-in [id [:tables id :rect-right]] (+ x-close width))))) (remove-seats close id)) (do (if (and rs dirxy) (reset-seats id tables-data)) (if (or show hide-stools) (swap! update-data (fn* [p1__2163283#] (-> p1__2163283# (assoc-in [id [:selection :show]] false) (assoc-in [id [:selection :active]] false) (assoc-in [id [:tables id :hide-stools]] true))))) (if (seq dirxy) (do (swap! update-data assoc-in [id [:tables id :block]] [x-new y-new]) (if (not= :xy dir) (do (swap! update-data (fn* [p1__2291426#] (-> p1__2291426# (assoc-in [id [:tables id :x]] x-move) (assoc-in [id [:tables id :y]] y-move) (assoc-in [id [:tables id :rect-right]] (+ x-move width)) (assoc-in [id [:tables id :rect-bottom]] (+ y-move height))))) (aset js/document "body" "style" "cursor" "move")) (aset js/document "body" "style" "cursor" "not-allowed"))) (do (aset js/document "body" "style" "cursor" "move") (swap! update-data (fn* [p1__2291428#] (-> p1__2291428# (assoc-in [id [:tables id :block]] nil) (assoc-in [id [:tables id :x]] x-new) (assoc-in [id [:tables id :y]] y-new) (assoc-in [id [:tables id :rect-right]] (+ x-new width)) (assoc-in [id [:tables id :rect-bottom]] (+ y-new height))))))))))))) (when-not (and test-block selected) (swap! update-data (fn* [p1__2117876#] (let [x-sel (- (+ x-current (.-pageXOffset js/window) x) (:x offset)) y-sel (- (+ y-current (.-pageYOffset js/window) y) (:y offset)) x1-sel (- (+ x-current (.-pageXOffset js/window) x) (:x1 offset)) y1-sel (- (+ y-current (.-pageYOffset js/window) y) (:y1 offset)) x-sel (if (pos? x-sel) x-sel 0) y-sel (if (pos? y-sel) y-sel 0) x1-sel (if (pos? x1-sel) x1-sel 0) y1-sel (if (pos? y1-sel) y1-sel 0)] (-> p1__2117876# (assoc-in [1 [:selection :start]] {:y y-sel, :x x-sel}) (assoc-in [1 [:selection :end]] {:y1 y1-sel, :x1 x1-sel})))))) (swap! table-data/tables-state (fn [x] (doall (reduce (fn* [p1__2291430# p2__2291431#] (assoc-in p1__2291430# (first p2__2291431#) (second p2__2291431#))) x (compiled-select (:all table-data/specter-paths) (mapv vec (compiled-select (:all-last table-data/specter-paths) (deref update-data)))))))))))


