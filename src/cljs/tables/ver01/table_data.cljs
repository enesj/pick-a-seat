(ns tables.ver01.table_data
  (:use [com.rpl.specter :only [select transform setval FIRST LAST ALL keypath filterer srange comp-paths compiled-select compiled-transform collect-one compiled-setval]])
  (:require [tables.ver01.themes :as t]
            [reagent.core :as r]
            [debux.cs.core :refer-macros [clog dbg break]]))
            ;[tables.selection-utils :as su]


(defonce tables-state
         (r/atom
           {:selection {:active false
                        :show false
                        :stop false
                        :start  {:x 0, :y 0}
                        :end    {:x1 0, :y1 0}
                        :selected []
                        :offset {}}
            :tables    {}
            :scale     {:zoom 1 :size 1}
            :pos       {:x 400 :y 300}
            :svg []}))


(def settings-base
         (let [[w h] [2000 2000]]
           (r/atom
             {:window    {:w w :h w}
              :table-stool [30 8]
              :stool-dims {:normal {:w 8 :h 4} :small {:w 4 :h 2}}
              :menu-dims  [25 20 3]
              :borders    [{:id :l :x 0 :y 0 :rect-right 5 :rect-bottom h}
                           {:id :t :x 0 :y 0 :rect-right w :rect-bottom 5}
                           {:id :r :x w :y 0 :rect-right (+ w 100) :rect-bottom h}]})))

(def menu-defaults
  {
   ;:width        30
   ;:height       15
   :fill         "rgba(255,255,255, 0.8)"
   :stroke       "orange"
   :stroke-width 0.5})

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
   :zoom-x (comp-paths :tables ALL LAST :x)
   :zoom-y (comp-paths :tables ALL LAST :y)
   :sel-start (comp-paths :selection :start ALL LAST)
   :sel-end (comp-paths :selection :end ALL LAST)
   :zoom (comp-paths :scale :zoom)
   :borders-right-x (comp-paths :borders (filterer #(= (:id %) :r)) FIRST :x)
   :borders-right (comp-paths :borders (filterer #(= (:id %) :r)) FIRST :rect-right)
   :borders-top (comp-paths :borders (filterer #(= (:id %) :t)) FIRST :rect-right)})


(defn table-dims [stools]
  (let [[base per-stool] (:table-stool @settings-base)
        w (+ base (* per-stool (dec (apply max (take 2 stools)))))
        h (+ base (* per-stool (dec (apply max (take-last 2 stools)))))]
    [w h]))

(defn table-props []
  "Preracunava karakteristike svakog stola u zavisnosti od trenute velicine ekrana.
  Poziva je (settings-pos [zoom-new]) na startu i kod resajzinga.
  Uzima u obzir trenutni polozaj stola i zum, kao i raspored stolica.
  Poziva funkciju:
  (table-dims [stools]) "

  (doall (for [table (:tables @tables-state)
               :let [id (key table)
                     table-v (val table)
                     {:keys [x y stools]} table-v
                     [width height]  (table-dims stools);))
                     rect-right (+ x width)
                     rect-bottom (+ y height)
                     rect {:width width :height height :rect-right rect-right :rect-bottom rect-bottom}]]
            (swap! tables-state update-in [:tables id ]  #(merge % rect)))))

(defn settings
  [size]
  "Preracunava dimenzije stolova, stolica i menija.
   Poziva je (settings-pos [zoom-new]) na startu i kod resajzinga."

 (swap! settings-base (fn [x] (->> x
                                   (compiled-transform (:table-stool specter-paths-data) #(mapv (partial * size) %))
                                   (compiled-transform (:menu-dims specter-paths-data) #(mapv (partial * size) %))
                                   (compiled-transform (:stool-dims specter-paths-data) #(* size %))))))

(defn settings-pos [zoom-new]
 "Preracunava x i y pozicije svih stolova i pamti tekucu vrijednost zum-a.
  Poziva je (resize) na startu i kod resajzinga.
  Poziva funkcije (settings zoom) i (table-props)"

  (let [zoom-old (:zoom (:scale @tables-state))
        zoom (/ zoom-new zoom-old)]
    (if (not= zoom 1)
      (do
        (swap! tables-state
                 (fn [x] (->> x
                              (compiled-transform (:zoom-x specter-paths-data) #(* zoom %))
                              (compiled-transform (:zoom-y specter-paths-data) #(* zoom %))
                              (compiled-setval (:zoom specter-paths-data) zoom-new))))
        (when (not (empty? (:selected (:selection @tables-state))))
            (swap! tables-state
                   (fn [x] (->> x (compiled-transform (:sel-start specter-paths-data) #(* zoom %))
                                (compiled-transform (:sel-end specter-paths-data) #(* zoom %))))))
        (settings zoom)
        (table-props)))))


(defn stool-data [stool dir id t1 t2]
  (conj [:rect] (assoc stool :dir dir :id id :transform (str " translate(" t1 ", " t2 ")"))))


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





