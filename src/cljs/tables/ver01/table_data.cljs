(ns tables.ver01.table_data
  (:use [com.rpl.specter :only [select transform setval FIRST LAST ALL keypath filterer srange comp-paths compiled-select compiled-transform collect-one compiled-setval]])
  (:require [tables.ver01.themes :as t]
            [reagent.core :as r]))
            ;[tables.selection-utils :as su]


(defonce tables-state
  (r/atom
    {:selection {:active false
                 :show false
                 :start  {}
                 :end    {}
                 :selected []
                 :offset {}}

     :tables    {
                  3  {:id 3 :pos [120, 120] :seats :4 :sides 4 :rotation false :hide-stools false :selected false}
                  23 {:id 23 :pos [350, 180] :seats :4 :sides 4 :rotation false :hide-stools false :selected false}
                  1  {:id 1 :pos [180, 120] :seats :8 :sides 4 :rotation false :hide-stools false :selected false}
                  21 {:id 21 :pos [180, 180] :seats :6 :sides 4 :rotation true :hide-stools false :selected false}
                  22 {:id 22 :pos [240, 180] :seats :4 :sides 2 :rotation false :hide-stools false :selected false}
                  24 {:id 24 :pos [300, 300] :seats :4 :sides 4 :rotation false :hide-stools false :selected false}
                  25 {:id 25 :pos [300, 120] :seats :8 :sides 2 :rotation true :hide-stools false :selected false}}

     :window    {}
     :timer     0
     :scale     {:zoom 1 :size 1}
     :pos       {:x 400 :y 300}
     :svg []}))



(defonce settings-base
  (r/atom
    {:table-dims {:2 [20 20] :4 [30 30] :6 [30 45] :8 [30 60]}
     :stool-dims {:normal {:w 8 :h 4} :small {:w 4 :h 2}}
     :menu-dims  [25 20 5]
     :borders    [{:id :l :x 0 :y 0 :rect-right 5 :rect-bottom 2000}
                  {:id :t :x 0 :y 0 :rect-right 625 :rect-bottom 5}
                  {:id :r :x 620 :y 0 :rect-right 1000 :rect-bottom 2000}]}))

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
  {:table-dims (comp-paths :table-dims ALL LAST)
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
                                    (compiled-transform (:table-dims specter-paths-data) #(mapv (partial * size) %))
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


(defn stool-defaults [size dir]
  (let [dims (:stool-dims @settings-base)
        dims-n (:normal dims)
        dims-s (:small dims)
        defaults (if (= size "s")
                   (merge stool-defaults-small
                          (if (= dir "h") {:width (:h dims-s) :height (:w dims-s)}
                            {:width (:w dims-s) :height (:h dims-s)}))
                   (merge stool-defaults-normal
                          (if (= dir "h") {:width (:h dims-n) :height (:w dims-n)}
                            {:width (:w dims-n) :height (:h dims-n)})))]
    defaults))


(defn stool-data [stool dir id t1 t2]
  (conj [:rect] (assoc stool :dir dir :id id :transform (str " translate(" t1 ", " t2 ")"))))

(defn stool-maps [x y w h se si r rs]
  (let [wr (:width (stool-defaults "n" "h"))
        wr2 (/ wr 2)
        h-wr (+ h wr)
        h-wr2 (+ h wr2)
        w-wr (+ w wr)
        w-wr2 (+ w wr2)
        h2 (/ h 2)
        w2 (/ w 2)
        se (if se (js/parseInt (name se)))
        n (if (= si 4) (+ (/ (- se 2) 2) 1) (+ (/ se 2) 1))
        stool-h (merge (stool-defaults "n" "h") {:x (- x wr) :y (+ y h2 (- wr))})
        stool-v (merge (stool-defaults "n" "v") {:x (+ x w2 (- wr)) :y (- y wr)})
        stool-h-s (merge (stool-defaults "s" "h") {:x (- x wr2) :y (+ y h2 (- wr2))})
        stool-v-s (merge (stool-defaults "s" "v") {:x (+ x w2 (- wr2)) :y (- y wr2)})
        result
        (if (= si 4)
          (if (not= r :r)
            [(stool-data stool-h :l 1 0 0)
             (stool-data stool-h-s :l :1 0 0)
             (stool-data stool-h :r 2 w-wr 0)
             (stool-data stool-h-s :r :2 w-wr2 0)]
            [(stool-data stool-v :t 1 0 0)
             (stool-data stool-v-s :t :1 0 0)
             (stool-data stool-v :d 2 0 h-wr)
             (stool-data stool-v-s :d :2 0 h-wr2)])
          [])
        all-seats (->>  (doall (for [i (range 1 n)
                                     :let [id1 (+ (* i 2) 1)
                                           id2 (+ (* i 2) 2)
                                           id1s (keyword (str id1))
                                           id2s (keyword (str id2))
                                           t-h (- (* i (/ w n)) w2)
                                           t-v (- (* i (/ h n)) h2)]]
                                 (if (not= r :r)
                                   [(stool-data stool-v :t id1 t-h 0)
                                    (stool-data stool-v-s :t id1s t-h 0)
                                    (stool-data stool-v :d id2 t-h h-wr)
                                    (stool-data stool-v-s :d id2s t-h h-wr2)]
                                   [(stool-data stool-h :l id1 0 t-v)
                                    (stool-data stool-h-s :l id1s 0 t-v)
                                    (stool-data stool-h :r id2 w-wr t-v)
                                    (stool-data stool-h-s :r id2s w-wr2 t-v)])))

                        flatten
                        (partition 2)
                        (mapv vec)
                        (into result))]
    (loop [rs rs
           all-seats all-seats]
      (if (> (count rs) 0)
        (recur (next rs) (into [] (remove (fn [x] (some #(= (:dir (second x)) %) (first rs))) all-seats)))
        all-seats))))








