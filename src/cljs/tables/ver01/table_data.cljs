(ns tables.ver01.table_data
  (:use [com.rpl.specter :only [select transform setval FIRST LAST ALL keypath filterer srange comp-paths compiled-select compiled-transform collect-one compiled-setval]])
  (:require [tables.ver01.themes :as t]
            [reagent.core :as r]))
            ;[tables.selection-utils :as su]


(defonce tables-state
         (r/atom
           {:selection {:active false
                        :show false
                        :start  {:x 0, :y 0}
                        :end    {:x1 0, :y1 0}
                        :selected []
                        :offset {}}
            :tables    {
                        3  {:id 3  :pos [120, 120] :x 120 :y 120 :hide-stools false :selected false :stools [1 0 2 2]}
                        23 {:id 23 :pos [350, 180] :x 350 :y 180 :hide-stools false :selected false :stools [3 3 1 1]}
                        1  {:id 1  :pos [180, 60]  :x 180 :y 60 :hide-stools false :selected false :stools [0 0 10 10]}
                        21 {:id 21 :pos [180, 180] :x 180 :y 180 :hide-stools false :selected false :stools [2 2 1 1]}
                        22 {:id 22 :pos [240, 180] :x 240 :y 180 :hide-stools false :selected false :stools [1 1 2 2]}
                        24 {:id 24 :pos [300, 300] :x 300 :y 300 :hide-stools false :selected false :stools [1 1 3 3]}
                        25 {:id 25 :pos [300, 120] :x 300 :y 120 :hide-stools false :selected false :stools [0 0 0 3]}}
            :window    {}
            :timer     0
            :scale     {:zoom 1 :size 1}
            :pos       {:x 400 :y 300}
            :svg []}))



(defonce settings-base
         (r/atom
           {:table-stool [30 8]
            :stool-dims {:normal {:w 8 :h 4} :small {:w 4 :h 2}}
            :menu-dims  [25 20 5]
            :borders    [{:id :l :x 0 :y 0 :rect-right 5 :rect-bottom 2000 :pos [0 0]}
                         {:id :t :x 0 :y 0 :rect-right 625 :rect-bottom 5 :pos [0 0]}
                         {:id :r :x 620 :y 0 :rect-right 1000 :rect-bottom 2000 :pos [620 0]}]}))

(def menu-defaults
  {:width        30
   :height       15
   :fill         "rgba(255,255,255, 0.8)"
   :stroke       "black"
   :stroke-width 0.5
   :dims         [25 20 5]})


(def table-defaults
  {:fill         (:table-fill t/palete)
   :fill-opacity 0.8
   :stroke       (:table-stroke t/palete)
   :stroke-width 0.7})


(def stool-defaults-normal
  {:rx           3
   :ry           3
   :fill         (:stool-fill t/palete)
   :fill-opacity 1
   :stroke       (:stool-stroke t/palete)
   :stroke-width 0.3})

(def stool-defaults-small
  {:rx           1
   :ry           1
   :fill         (:stool-small-fill t/palete)
   :fill-opacity 1
   :stroke       (:stool-small-stroke t/palete)
   :stroke-width 0})


(def sel-defaults
  {:fill-opacity 0
   :stroke       "black"
   :stroke-width 0.5})

(def specter-paths-data
  {:table-stool (comp-paths :table-stool)
   :menu-dims (comp-paths :menu-dims)
   :stool-dims (comp-paths :stool-dims ALL LAST ALL LAST)
   :zoom-pos (comp-paths :tables ALL LAST :pos ALL)
   :sel-start (comp-paths :selection :start ALL LAST)
   :sel-end (comp-paths :selection :end ALL LAST)
   :zoom (comp-paths :scale :zoom)
   :borders-right-x (comp-paths :borders (filterer #(= (:id %) :r)) FIRST :x)
   :borders-right (comp-paths :borders (filterer #(= (:id %) :r)) FIRST :rect-right)
   :borders-top (comp-paths :borders (filterer #(= (:id %) :t)) FIRST :rect-right)})


(transform  [:borders (filterer #(= (:id %) :r)) FIRST :x] #(* 2 %) @settings-base)

(defn settings
  [size]
  (swap! settings-base (fn [x] (->> x
                                    (compiled-transform (:table-stool specter-paths-data) #(mapv (partial * size) %))
                                    (compiled-transform (:menu-dims specter-paths-data) #(mapv (partial * size) %))
                                    (compiled-transform (:stool-dims specter-paths-data) #(* size %))
                                    (compiled-transform (:borders-right specter-paths-data) #(* size %))
                                    (compiled-transform (:borders-right-x specter-paths-data) #(* size %))
                                    (compiled-transform (:borders-top specter-paths-data) #(* size %))))))


(defn settings-pos [zoom-new]
  (let [zoom-old (:zoom (:scale @tables-state))
        zoom (/ zoom-new zoom-old)]
    (do (swap! tables-state
               (fn [x] (->> x
                            (compiled-transform (:zoom-pos specter-paths-data) #(* zoom %))
                            (compiled-setval (:zoom specter-paths-data) zoom-new))))
        (when (not (empty? (:selected (:selection @tables-state))))
          (swap! tables-state
                 (fn [x] (->> x (compiled-transform (:sel-start specter-paths-data) #(* zoom %))
                              (compiled-transform (:sel-end specter-paths-data) #(* zoom %))))))
        (settings zoom))))


(defn stool-data [stool dir id t1 t2]
  (conj [:rect] (assoc stool :dir dir :id id :transform (str " translate(" t1 ", " t2 ")"))))

(defn table-dims [stools]
  (let [[base per-stool] (:table-stool @settings-base)
        w (+ base (* per-stool (dec (apply max (take 2 stools)))))
        h (+ base (* per-stool (dec (apply max (take-last 2 stools)))))]
    [w h]))

(defn stool-maps [x y w h rs stools]
  (let [dims (:stool-dims @settings-base)
        {{n-h :h  n-w :w } :normal {s-h :h s-w :w } :small } dims
        wr2 (/ n-h 2)
        h-wr (+ h n-h)
        h-wr2 (+ h wr2)
        w-wr (+ w n-h)
        w-wr2 (+ w wr2)
        h2 (/ h 2)
        w2 (/ w 2)
        stool-h (merge stool-defaults-normal  {:width n-h :height n-w} {:x ( - x n-h)        :y (+ y h2 ( - n-h))})
        stool-v (merge stool-defaults-normal  {:width n-w :height n-h} {:x (+ x w2 ( - n-h)) :y ( - y n-h)})
        stool-h-s (merge stool-defaults-small {:width s-h :height s-w} {:x ( - x wr2)        :y (+ y h2 ( - wr2))})
        stool-v-s (merge stool-defaults-small {:width s-w :height s-h} {:x (+ x w2 ( - wr2)) :y ( - y wr2)})
        all-seats-new (->>  (doall (for [side (range 0 4)
                                         :let [stools-side (inc (stools side))]]
                                     (for [i (range 1 stools-side)
                                           :let [id1 (keyword (str side "-" (+ (* i 2) 1)))
                                                 id1s (keyword (str id1 "-s"))
                                                 id2 (keyword (str side "-" (+ (* i 2) 2)))
                                                 id2s (keyword (str id1 "-s"))
                                                 t-h (- (* i (/ w stools-side)) w2)
                                                 t-v (- (* i (/ h stools-side)) h2)]]
                                       (case side
                                         0
                                         [(stool-data stool-v :t id1 t-h 0)
                                          (stool-data stool-v-s :t id1s t-h 0)]
                                         1
                                         [(stool-data stool-v :d id2 t-h h-wr)
                                          (stool-data stool-v-s :d id2s t-h h-wr2)]
                                         2
                                         [(stool-data stool-h :l id1 0 t-v)
                                          (stool-data stool-h-s :l id1s 0 t-v)]
                                         3
                                         [(stool-data stool-h :r id2 w-wr t-v)
                                          (stool-data stool-h-s :r id2s w-wr2 t-v)]))))
                            flatten
                            (partition 2)
                            (mapv vec)
                            (into []))]
    (loop [rs rs
           all-seats all-seats-new]
      (if (> (count rs) 0)
        (recur (next rs) (into [] (remove (fn [x] (some #(= (:dir (second x)) %) (first rs))) all-seats)))
        all-seats))))





