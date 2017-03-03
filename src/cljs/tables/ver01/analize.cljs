(ns tables.ver01.analize
  (:use [com.rpl.specter :only [select transform setval FIRST LAST ALL keypath filterer srange comp-paths compiled-select collect-one compiled-setval]])
  (:require [tables.ver01.table_data :as td]
    [tables.ver01.svg :as svg]
    [tables.ver01.util :as u]
    [reagent.core :as r]))

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

(defn data-preparation [tables-state selected]
  (let [all-tables (map second tables-state)
        sel-tables (map second (select-keys tables-state selected))
        other-tables (remove (set sel-tables) all-tables)
        other-ids (mapv :id other-tables)
        sel-tables-x (sort-by :x sel-tables)
        sel-tables-y (sort-by :y sel-tables)
        x-min (:x (first sel-tables-x))
        x-max (:x (last sel-tables-x))
        x1-max (:rect-right (last sel-tables-x))
        y-min (:y (first sel-tables-y))
        y-max (:y (last sel-tables-y))
        y1-max (:rect-bottom (last sel-tables-y))
        selection-table {:id 1 :x x-min :y y-min :rect-right x1-max :rect-bottom y1-max}
        extern-collision (test-collision selection-table other-tables)
        extern-tables (filter #((set extern-collision) (:id %)) other-tables)]
   ;(println extern-tables)
   (swap! td/tables-state assoc-in [:selection :start] {:x x-min :y y-min})
   (swap! td/tables-state assoc-in [:selection :end] {:x1 x1-max :y1 y1-max})
   {:all-tables all-tables :sel-tables sel-tables :other-tables other-tables
    :other-ids other-ids :sel-tables-x sel-tables-x :sel-tables-y sel-tables-y :extern-tables extern-tables
    :x-min x-min :x-max x-max :x1-max x1-max
    :y-min y-min :y-max y-max :y1-max y1-max}))



(defn move-table [table [x y]]
  (-> table
      (update-in [:x] #(+ % x))
      (update-in [:rect-right] #(+ % x))
      (update-in [:y] #(+ % y))
      (update-in [:rect-bottom] #(+ % y))))


(defn test-one [sel-tables-state sel-tables extern-tables y-min]
    (doall
      (for [current-table (rest sel-tables)
            :let [new-table (move-table current-table [ 0 (- y-min (:y current-table))])]]
        (if (and (empty? (test-collision new-table (remove (set [current-table]) (vals @sel-tables-state))))
                 (empty? (test-collision new-table extern-tables)))
          (do
            (swap! sel-tables-state update-in [(:id current-table)]
                   #(-> %
                        sel-modifications
                        (assoc-in [:y] (:y new-table))
                        (assoc-in [:rect-bottom] (:rect-bottom new-table)))))))))




(defn test-all []
  (let [tables-state (:tables @td/tables-state)
        selected (:selected (:selection @td/tables-state))
        {:keys [all-tables sel-tables other-tables
                 other-ids sel-tables-x sel-tables-y extern-tables
                 x-min x-max x1-max y-min y-max y1-max]}
        (data-preparation tables-state selected)
        sel-tables-state (atom (into {}  (map #(vector (:id %) %) sel-tables-y)))
        tables-top (test-one sel-tables-state sel-tables-y extern-tables y-min)]
     (reset! selected-current {:state 1 :ids selected :tables (last (remove nil? tables-top))})))



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