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

(defn extract-data [tables]
  (map (fn [x]
         (let [[id data] x]
           {:id id
             :x  (:x data)
             :y  (:y data)
             :x1 (:rect-right data)
             :y1 (:rect-bottom data)}))
       tables))

(defn a-top [spoints selected]
  (let [all-tables (extract-data spoints)
        sel-tables  (extract-data (select-keys spoints selected))]
    (transform [ALL LAST] #(-> %
                               (update-in [:y] (fn [y] (- y 20)))
                               sel-modifications)
               (select-keys (:tables spoints) selected))))




(defn a-top-new [spoints selected]
  (let [all-tables (extract-data spoints)
        sel-tables  (extract-data (select-keys spoints selected))
        sel-tables-x (sort-by :x sel-tables)
        sel-tables-y (sort-by :y sel-tables)]


   [sel-tables-x sel-tables-y]))



(defn a-down [spoints selected]
  (transform [ALL LAST] #(-> %
                             (update-in [:y] (fn [y] (+ y 20)))
                             sel-modifications)
             (select-keys (:tables spoints) selected)))

(defn a-left [spoints selected]
  (transform [ALL LAST] #(-> %
                             (update-in [:x] (fn [x] (- x 20)))
                             sel-modifications)
             (select-keys (:tables spoints) selected)))

(defn a-right [spoints selected]
  (transform [ALL LAST] #(-> %
                             (update-in [:x] (fn [x] (+ x 20)))
                             sel-modifications)
             (select-keys (:tables spoints) selected)))