(ns tables.ver01.analize
  (:use [com.rpl.specter :only [select transform setval FIRST LAST ALL keypath filterer srange comp-paths compiled-select collect-one compiled-setval]])
  (:require [tables.ver01.table_data :as td]
    [tables.ver01.svg :as svg]
    [tables.ver01.util :as u]))


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
        dir1 (doall (for [table (rest sel-tables-x)
                          :let [dir (u/collides-sel table (update-in (first sel-tables-x) [:rect-bottom]  #( + % 40 )  ) 0)]
                              :when (not= false dir)]
                         dir))]

    (swap! td/tables-state assoc-in [:selection :start] {:x x-min :y y-min})
    (swap! td/tables-state assoc-in [:selection :end] {:x1 x1-max :y1 y1-max})


   [other-ids sel-tables-x dir1]))





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