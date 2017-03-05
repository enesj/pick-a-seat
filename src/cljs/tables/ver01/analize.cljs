(ns tables.ver01.analize
  (:use [com.rpl.specter :only [select transform setval FIRST LAST ALL keypath filterer srange comp-paths compiled-select collect-one compiled-setval]])
  (:require [tables.ver01.table_data :as td]
            [tables.ver01.svg :as svg]
            [tables.ver01.util :as u]
            [reagent.core :as r]
            [debux.cs.core :refer-macros [clog dbg break]]))

(def selected-current (r/atom {:current-state 0 :ids [] :tables {}}))

(defn sel-modifications [data]
  (-> data
      (assoc-in [:hide-stools] true)
      (assoc-in [:fill-opacity] 0.9)
      (assoc-in [:stroke] "orange")))

(defn clear-modifications [data]
  (-> data
      (assoc-in [:hide-stools] false)
      (assoc-in [:fill-opacity] 0.8)
      (assoc-in [:stroke] "black")))


(defn test-collision [test-table tables]
  (let [dir (doall (for [table tables
                         :let [dir (u/collides-sel table test-table 8)]
                         :when (not= false dir)]
                     ;(do (println "coll" table test-table)
                     dir))]
    dir))

(defn move-table [table [x y]]
  (-> table
      (update-in [:x] #(+ % x))
      (update-in [:rect-right] #(+ % x))
      (update-in [:y] #(+ % y))
      (update-in [:rect-bottom] #(+ % y))))


(defn data-preparation [tables-state selected]
  (let [all-tables (map second tables-state)
        sel-tables (map second (select-keys tables-state selected))
        other-tables (remove (set sel-tables) all-tables)
        other-ids (mapv :id other-tables)
        sel-tables-left (sort-by :x sel-tables)
        sel-tables-top (sort-by :y sel-tables)
        sel-tables-right (sort-by :rect-right > sel-tables)
        sel-tables-down (sort-by :rect-bottom > sel-tables)
        x-min (:x (first sel-tables-left))
        x-max (:x (last sel-tables-left))
        x1-max (:rect-right (last sel-tables-left))
        y-min (:y (first sel-tables-top))
        y-max (:y (last sel-tables-top))
        y1-max (:rect-bottom (last sel-tables-top))
        lefts (drop-while #(< (- (:x %) x-min) 1) sel-tables-left)
        tops (drop-while #(< (- (:y %) y-min) 1) sel-tables-top)
        rights (drop-while #(< (- x1-max (:rect-right %)) 1) sel-tables-right)
        downs (drop-while #(< (- y1-max (:rect-bottom %)) 1) sel-tables-down)
        selection-table {:id 1 :x x-min :y y-min :rect-right x1-max :rect-bottom y1-max}
        extern-collision (test-collision selection-table other-tables)
        extern-tables (filter #((set extern-collision) (:id %)) other-tables)]
    (swap! td/tables-state assoc-in [:selection :start] {:x x-min :y y-min})
    (swap! td/tables-state assoc-in [:selection :end] {:x1 x1-max :y1 y1-max})
    {:all-tables       all-tables :sel-tables sel-tables :other-tables other-tables
     :other-ids        other-ids :sel-tables-left sel-tables-left :sel-tables-top sel-tables-top
     :sel-tables-right sel-tables-right :sel-tables-down sel-tables-down :extern-tables extern-tables
     :lefts            lefts :tops tops :rights rights :downs downs
     :x-min            x-min :x-max x-max :x1-max x1-max
     :y-min            y-min :y-max y-max :y1-max y1-max}))


(defn test-one [sel-tables-state sel-tables-all sel-tables extern-tables x-min x-max x1-max y-min y-max y1-max type]
  (reset! sel-tables-state (into {} (map #(vector (:id %) %) sel-tables-all)))
  (doall
    (for [current-table sel-tables
          :let [new-table-left (move-table current-table [(- x-min (:x current-table)) 0])
                new-table-top (move-table current-table [0 (- y-min (:y current-table))])
                new-table-right (move-table current-table [(- x1-max (:rect-right current-table)) 0])
                new-table-down (move-table current-table [0 (- y1-max (:rect-bottom current-table))])
                new-table (case type :l new-table-left :t new-table-top :r new-table-right :d new-table-down)]]
      (if (and (empty? (test-collision new-table (remove (set [current-table]) (vals @sel-tables-state))))
               (empty? (test-collision new-table extern-tables)))
        (do
          (swap! sel-tables-state update-in [(:id current-table)]
                 #(-> %
                      sel-modifications
                      (assoc-in [:x] (:x new-table))
                      (assoc-in [:y] (:y new-table))
                      (assoc-in [:rect-right] (:rect-right new-table))
                      (assoc-in [:rect-bottom] (:rect-bottom new-table)))))))))

(defn adjust-v-space [tables y-min y1-max]
  (let [sel-heght (- y1-max y-min)
        count-tables (count tables)
        total-height (reduce  + (map :height tables))
        v-space   (/ (- sel-heght total-height) (dec count-tables))]
    (loop [old-tables (rest tables)
           distance  (+ y-min (:height (first tables)) v-space)
           new-tables [(first tables)]]
      (js/console.log new-tables)
      (if (empty? old-tables)
        new-tables
        (recur (rest old-tables) (+ distance (:height (first old-tables)) v-space)
               (conj new-tables  (-> (first old-tables)
                                     (assoc-in [:y] distance)
                                     (assoc-in [:rect-bottom] (+ distance (:height (first old-tables)))))))))))



(defn test-all []
  (let [tables-state (:tables @td/tables-state)
        selected (:selected (:selection @td/tables-state))
        {:keys [all-tables sel-tables other-tables
                other-ids sel-tables-left sel-tables-top
                sel-tables-right sel-tables-down extern-tables
                lefts tops rights downs
                x-min x-max x1-max y-min y-max y1-max]}
        (data-preparation tables-state selected)
        sel-tables-state (atom {})
        tables-left (if (empty? lefts)
                      '()
                      (remove nil? (test-one sel-tables-state sel-tables-left lefts extern-tables x-min x-max x1-max y-min y-max y1-max :l)))
        tables-top (if (empty? tops)
                     '()
                     (remove nil? (test-one sel-tables-state sel-tables-top tops extern-tables x-min x-max x1-max y-min y-max y1-max :t)))
        tables-right (if (empty? rights)
                       '()
                       (remove nil? (test-one sel-tables-state sel-tables-right rights extern-tables x-min x-max x1-max y-min y-max y1-max :r)))
        tables-down (if (empty? downs)
                      '()
                      (remove nil? (test-one sel-tables-state sel-tables-down downs extern-tables x-min x-max x1-max y-min y-max y1-max :d)))
        tables-new (sort-by count (remove empty? [tables-right tables-down tables-left tables-top]))
        tables-v-space (if (or (empty? lefts))
                           (adjust-v-space sel-tables-top y-min y1-max)
                           '())
        tables-h-space (if (or (empty? tops) (empty? downs))
                           (adjust-v-space sel-tables-left y-min y1-max)
                           '())]
    (js/console.log (into {} (map #(vector (:id %) %) tables-v-space)) (last (last tables-new)))
    (if-not (empty? tables-new)
      (reset! selected-current {:state 1 :ids selected :tables (last (last tables-new))})
      (if-not (empty? tables-v-space)
        (let [tvs (into {} (map #(vector (:id %) %)  tables-v-space))]
          (reset! selected-current {:state 1 :ids selected :tables tvs}))))))



(defn a-top [tables-state selected]
  (transform [ALL LAST] #(-> %
                             (update-in [:y] (fn [y] (- y 20)))
                             sel-modifications)
             (select-keys (:tables tables-state) selected)))



(defn a-down [tables-state selected]
  (transform [ALL LAST] #(-> %
                             (update-in [:y] (fn [y] (+ y 20)))
                             sel-modifications)
             (select-keys (:tables tables-state) selected)))

(defn a-left [tables-state selected]
  (transform [ALL LAST] #(-> %
                             (update-in [:x] (fn [x] (- x 20)))
                             sel-modifications)
             (select-keys (:tables tables-state) selected)))

(defn a-right [tables-state selected]
  (transform [ALL LAST] #(-> %
                             (update-in [:x] (fn [x] (+ x 20)))
                             sel-modifications)
             (select-keys (:tables tables-state) selected)))