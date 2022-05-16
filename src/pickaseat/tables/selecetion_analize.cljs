(ns pickaseat.tables.selecetion-analize
  (:use [com.rpl.specter :only [select transform setval FIRST LAST ALL keypath filterer srange comp-paths compiled-select collect-one compiled-setval]])
  (:require [pickaseat.data.table-data :as table-data]
            [pickaseat.tables.table-utils :as table-utils]
            [reagent.core :as r]))

(def selected-init {:current-state 0 :ids [] :tables {} :start {} :end {} :del false})

(def selected-current (r/atom selected-init))

(defn sel-modifications [data]
  (-> data
      (assoc-in [:hide-stools] true)
      (assoc-in [:fill-opacity] 0.3)
      (assoc-in [:stroke] "orange")))

(defn clear-modifications [data]
  (-> data
      (assoc-in [:hide-stools] false)
      (assoc-in [:fill-opacity] 0.3)
      (assoc-in [:stroke] "black")))


(defn test-collision [test-table tables]
  (doall (for [table tables
               :let [dir (table-utils/collides-sel table test-table 16)]
               :when (not= false dir)]
             dir)))


(defn move-table [table [x y]]
  (-> table
      (update-in [:x] #(+ % x))
      (update-in [:rect-right] #(+ % x))
      (update-in [:y] #(+ % y))
      (update-in [:rect-bottom] #(+ % y))))

(defn extract-ids [table-data]
  (into {} (mapv #(vector (:id %) %) table-data)))

(defn data-preparation [tables-state selected]
  (let [selection (:selection @table-data/tables-state)
        all-tables (map second tables-state)
        {:keys [start end]} selection
        next-id (inc (->> all-tables
                         (map :id)
                         (apply max)))]
    (case (count selected)
      ; 0 znaci dodavanje novih stolova
      0 (let [sel-type :empty
              x-sel (+ (:x start) 5)
              y-sel (+ (:y start) 5)
              x1-sel (- (:x end) 5)
              y1-sel (- (:y end) 5)
              sel-width (- x1-sel x-sel)
              sel-height (- y1-sel y-sel)
              selection-table {:id 1 :x (:x start) :y (:y start) :rect-right (:x end) :rect-bottom (:y end)}
              extern-collision (set (test-collision selection-table all-tables))
              extern-tables (filter #(extern-collision (:id %)) all-tables)
              table-types table-data/table-types
              table-types (map #(let [t-dims (table-data/table-dims-by-stools (:stools %))]
                                  (merge {:id next-id :x x-sel :y y-sel
                                          :width (first t-dims)
                                          :height (second t-dims)
                                          :rect-right (+ x-sel (first t-dims))
                                          :rect-bottom (+ y-sel (second t-dims))
                                          :stroke "orange"}
                                         %))
                               table-types)
              table-types (filter #(and (> sel-width (:width %)) (> sel-height (:height %)))
                                  table-types)
              table-types (map (partial hash-map next-id)
                               table-types)]
          {:sel-type sel-type :start start :end end :next-id next-id
           :table-types table-types :x-sel x-sel :y-sel y-sel :x1-sel x1-sel :y1-sel y1-sel
           :extern-tables extern-tables :selection-table selection-table})

      ;1 ne znam sta radi, ne radi nista ?????
      1 (let [sel-type :one]
          {:sel-type sel-type :start start :end end :next-id next-id})

      (let [sel-type :many
            sel-tables (map second (select-keys tables-state selected))
            del-sel-tables (extract-ids  (map #(assoc-in % [:del] true) sel-tables))
            other-tables (remove (set sel-tables) all-tables)
            other-ids (mapv :id other-tables)
            sel-tables-left (sort-by :x sel-tables)
            sel-tables-top (sort-by :y sel-tables)
            sel-tables-right (sort-by :rect-right > sel-tables)
            sel-tables-down (sort-by :rect-bottom > sel-tables)
            x-min (:x (first sel-tables-left))
            x1-max (:rect-right (last sel-tables-left))
            y-min (:y (first sel-tables-top))
            y1-max (:rect-bottom (last sel-tables-top))
            lefts (drop-while #(< (- (:x %) x-min) 1) sel-tables-left)
            tops (drop-while #(< (- (:y %) y-min) 1) sel-tables-top)
            rights (drop-while #(< (- x1-max (:rect-right %)) 1) sel-tables-right)
            downs (drop-while #(< (- y1-max (:rect-bottom %)) 1) sel-tables-down)
            selection-table {:id 1 :x x-min :y y-min :rect-right x1-max :rect-bottom y1-max}
            extern-collision (test-collision selection-table other-tables)
            extern-tables (filter #((set extern-collision) (:id %)) other-tables)]
        {
         :all-tables       all-tables
         :sel-tables sel-tables
         :other-tables other-tables
         :other-ids        other-ids
         :sel-tables-left sel-tables-left
         :sel-tables-top sel-tables-top
         :sel-tables-right sel-tables-right
         :sel-tables-down sel-tables-down
         :extern-tables extern-tables
         :del-sel-tables   del-sel-tables
         :lefts            lefts :tops tops :rights rights :downs downs
         :x-min            x-min
         :x1-max x1-max
         :y-min            y-min
         :y1-max y1-max
         :sel-type         sel-type}))))


(defn test-one [sel-tables-state  extern-tables x-min  x1-max y-min  y1-max]
  (fn [sel-tables-all sel-tables type]
    (if (empty? sel-tables)
      (list)
      (do
        (reset! sel-tables-state (extract-ids sel-tables-all))
        (remove nil?
                (mapv
                  (fn [current-table]
                    (let [new-table-left (move-table current-table [(- x-min (:x current-table)) 0])
                          new-table-top (move-table current-table [0 (- y-min (:y current-table))])
                          new-table-right (move-table current-table [(- x1-max (:rect-right current-table)) 0])
                          new-table-down (move-table current-table [0 (- y1-max (:rect-bottom current-table))])
                          new-table (case type :l new-table-left :t new-table-top :r new-table-right :d new-table-down)]
                      (if (and (empty? (test-collision new-table (remove (set [current-table]) (vals @sel-tables-state))))
                               (empty? (test-collision new-table extern-tables)))
                        (swap! sel-tables-state update-in [(:id current-table)]
                               #(-> %
                                    sel-modifications
                                    (assoc-in [:x] (:x new-table))
                                    (assoc-in [:y] (:y new-table))
                                    (assoc-in [:rect-right] (:rect-right new-table))
                                    (assoc-in [:rect-bottom] (:rect-bottom new-table)))))))
                 sel-tables))))))

(defn adjust-space [tables sel-min sel-max orientation join]
  (let [sel-extent (- sel-max sel-min)
        count-tables (count tables)
        [a b dim dir-back dir-forward] (if (= orientation :y)
                                         [:y :rect-bottom :height :t :d]
                                         [:x :rect-right :width :l :r])
        occupied (reduce + (map dim tables))
        space (if join 0 (/ (- sel-extent occupied) (dec count-tables)))]
    (loop [old-tables (rest tables)
           distance (+ sel-min (dim (first tables)) space)
           new-tables [(if join (assoc-in (first tables) [:rs] {(:id (second tables)) #{dir-forward}})
                                (first tables))]]
      (if (empty? old-tables)
        (if (= (map #(select-keys % [:x :y]) new-tables) (map #(select-keys % [:x :y]) tables))
         nil
         new-tables)
        (recur (rest old-tables)
               (+ distance (dim (first old-tables)) space)
               (conj new-tables (-> (first old-tables)
                                    sel-modifications
                                    (assoc-in [:rs] (when join (dissoc {(:id (second old-tables)) #{dir-forward}, (:id (last new-tables)) #{dir-back}} nil)))
                                    (assoc-in [a] distance)
                                    (assoc-in [b] (+ distance (dim (first old-tables)))))))))))

(defn test-all []
  (let [tables-state (:tables @table-data/tables-state)
        selected (:selected (:selection @table-data/tables-state))
        sel-tables-state (atom {})
        {:keys [
                sel-tables-left
                sel-tables-top
                sel-tables-right
                sel-tables-down
                extern-tables
                del-sel-tables
                lefts tops rights downs
                x-min  x1-max y-min  y1-max
                sel-type
                table-types]}
        (data-preparation tables-state selected)
        test-one-template (test-one sel-tables-state  extern-tables x-min  x1-max y-min  y1-max)]
    (if (= sel-type :many)
      (let [
            tables-left (test-one-template sel-tables-left lefts :l)
            tables-top (test-one-template sel-tables-top tops :t)
            tables-right (test-one-template sel-tables-right rights :r)
            tables-down (test-one-template sel-tables-down downs :d)
            tables-new (map last (sort-by count (remove empty? [tables-right tables-down tables-left tables-top])))
            space-fn (fn [test-1 test-2 tables min max orientation close]
                       (let [adjusted (adjust-space tables min max orientation close)]
                         (when adjusted (extract-ids (if (or (empty? test-1) (empty? test-2)) adjusted '())))))
            tables-v-space (space-fn lefts rights sel-tables-top y-min y1-max :y false)
            tables-h-space (space-fn tops downs sel-tables-left x-min x1-max :x false)
            tables-v-join (space-fn lefts rights sel-tables-top y-min y1-max :y true)
            tables-h-join (space-fn tops downs sel-tables-left x-min x1-max :x true)]
        (vec
          (cond
            (> (count tables-new) 1) (merge  (vec (conj tables-new {})) del-sel-tables)
            (or (seq tables-v-space) (seq tables-v-join))
            (vec (remove nil? [{} (first tables-new) tables-v-space tables-v-join del-sel-tables]))
            (or (seq tables-h-space) (seq tables-h-join))
            (vec (remove nil? [{} (first tables-new) tables-h-space tables-h-join del-sel-tables]))
            :else [{}])))
      (if (= sel-type :empty)
          (vec (conj (filter #(empty? (test-collision (first (map val %)) extern-tables)) table-types) {}))
        (let [table (tables-state (first selected))]
          [{}])))))


