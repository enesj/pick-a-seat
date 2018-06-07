(ns pickaseat.ver01.data.table_data
  (:use [com.rpl.specter :only [select transform setval FIRST LAST ALL keypath
                                filterer srange comp-paths compiled-select compiled-transform
                                collect-one compiled-setval]])
  (:require [pickaseat.ver01.data.themes :as themes]
            [reagent.core :as r]
            [pickaseat.ver01.data.floor-data :as floor-data]
            [pickaseat.ver01.data.common-data :as common-data]))

(defonce tables-state
         (r/atom
           {:selection {:active   false
                        :show     false
                        :stop     false
                        :start    {:x 0, :y 0}
                        :end      {:x1 0, :y1 0}
                        :selected []
                        :offset   {}}
            :tables    {
                        3  {:id 3 :x 120 :y 120 :hide-stools false :selected false :block nil :stools [1 1 1 1]}
                        23 {:id 23 :x 350 :y 180 :hide-stools false :selected false :block nil :stools [1 1 2 2]}
                        12 {:id 12 :x 250 :y 220 :hide-stools false :selected false :block nil :stools [2 2 1 1]}
                        11 {:id 11 :x 180 :y 60 :hide-stools false :selected false :block nil :stools [1 1 1 1]}
                        21 {:id 21 :x 180 :y 180 :hide-stools false :selected false :block nil :stools [1 1 1 1]}
                        22 {:id 22 :x 240 :y 180 :hide-stools false :selected false :block nil :stools [1 1 1 1]}
                        24 {:id 24 :x 300 :y 300 :hide-stools false :selected false :block nil :stools [1 1 1 1]}
                        25 {:id 25 :x 300 :y 120 :hide-stools false :selected false :block nil :stools [1 1 1 1]}}
            :layout    false
            :scale     {:zoom 1 :size 1}
            :pos       {:x 400 :y 300}}))


(def history (r/atom {:performed [] :recalled [] :layout false}))


(def table-types
  [{:stools [1 1 1 1]}
   {:stools [2 2 0 0]}
   {:stools [1 1 2 2]}
   {:stools [1 1 3 3]}])


(def base-settings
         (let [{:keys [w h]} @common-data/data]
           (r/atom
             {:window    {:w w :h w}
              :table-stool [30 8]
              :stool-dims {:normal {:w 8 :h 4} :small {:w 4 :h 2}}
              :menu-dims  [25 20 3]
              :borders    [{:id :l :x 0 :y 0 :rect-right 5 :rect-bottom h}
                           {:id :t :x 0 :y 0 :rect-right w :rect-bottom 5}
                           {:id :r :x w :y 0 :rect-right (+ w 100) :rect-bottom h}]
              :history-length         15})))

(def menu-defaults
  {
   ;:width        30
   ;:height       15
   :fill         "rgba(255,255,255, 0.4)"
   :stroke       "orange"
   :stroke-width 0.5})

(def table-defaults
  {:fill         (:table-fill themes/palete)
   :fill-opacity 0.8
   :stroke       (:table-stroke themes/palete)
   :stroke-width 0.7})

(def stool-defaults-normal
  {:rx           3
   :ry           3
   :fill         (:stool-fill themes/palete)
   :fill-opacity 1
   :stroke       (:stool-stroke themes/palete)
   :stroke-width 0.3})

(def stool-defaults-small
  {:rx           1
   :ry           1
   :fill         (:stool-small-fill themes/palete)
   :fill-opacity 1
   :stroke       (:stool-small-stroke themes/palete)
   :stroke-width 0})


(def sel-defaults
  {:fill-opacity 0
   :stroke       "black"
   :stroke-width 0.5})

(def specter-paths
  {:table-stool      (comp-paths :table-stool)
   :menu-dims        (comp-paths :menu-dims)
   :stool-dims       (comp-paths :stool-dims ALL LAST ALL LAST)
   :scale-x          (comp-paths :tables ALL LAST :x)
   :scale-y          (comp-paths :tables ALL LAST :y)
   :tabale-selected  (comp-paths :tables ALL LAST :selected)
   :hide-stools      (comp-paths :tables ALL LAST :hide-stools)
   :borders-right-x  (comp-paths :borders (filterer #(= (:id %) :r)) FIRST :x)
   :borders-right    (comp-paths :borders (filterer #(= (:id %) :r)) FIRST :rect-right)
   :borders-top      (comp-paths :borders (filterer #(= (:id %) :t)) FIRST :rect-right)
   :sel-end          (comp-paths :selection :end ALL LAST)
   :sel-start        (comp-paths :selection :start ALL LAST)
   :selection-show   (comp-paths :selection :show)
   :selected         (comp-paths :selection :selected)
   :selection-active (comp-paths :selection :active)
   :selection-offset (comp-paths :selection :offset)
   :selection-end    (comp-paths :selection :end)
   :selection-start  (comp-paths :selection :start)
   :zoom             (comp-paths :scale :zoom)
   :all              (comp-paths ALL ALL)
   :all-last         (comp-paths ALL LAST)})


(defn table-dims [stools]
  (let [[base per-stool] (:table-stool @base-settings)
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
                     [width height] (table-dims stools)
                     rect-right (+ x width)
                     rect-bottom (+ y height)
                     rect {:width (Math/round width) :height (Math/round height) :rect-right (Math/round rect-right) :rect-bottom (Math/round rect-bottom)}]]
             (swap! tables-state update-in [:tables id ]  #(merge % rect)))))

(defn settings
  [size]
  "Preracunava dimenzije stolova, stolica i menija.
   Poziva je (settings-pos [zoom-new]) na startu i kod resajzinga."

 (swap! base-settings (fn [x] (->> x
                                   (compiled-transform (:table-stool specter-paths) #(mapv (fn [x] (Math/round (* size x))) %))
                                   (compiled-transform (:menu-dims specter-paths) #(mapv (fn [x] (Math/round (* size x))) %))
                                   (compiled-transform (:stool-dims specter-paths) #(Math/round (* size %)))))))

(defn settings-pos [zoom-new settings?]
  "Preracunava x i y pozicije svih stolova i pamti tekucu vrijednost zum-a.
	 Poziva je (resize) na startu i kod resajzinga.
	 Poziva funkcije (settings zoom) i (table-props)"

  (let [zoom-old (:zoom (:scale @tables-state))
        zoom (/ zoom-new zoom-old)]
    (if (not= zoom 1)
      (do
        (swap! tables-state
               (fn [x] (->> x
                            (compiled-transform (:scale-x specter-paths) #(Math/round (* zoom %)))
                            (compiled-transform (:scale-y specter-paths) #(Math/round (* zoom %)))
                            (compiled-setval (:zoom specter-paths) zoom-new))))
        (when (not (empty? (:selected (:selection @tables-state))))
          (swap! tables-state
                 (fn [x] (->> x (compiled-transform (:sel-start specter-paths) #(Math/round (* zoom %)))
                              (compiled-transform (:sel-end specter-paths) #(Math/round (* zoom %)))))))
        (when settings?
          (swap! floor-data/data (fn [x] (compiled-transform (:polygon floor-data/specter-paths) #(Math/round (* zoom %)) x)))
          (settings zoom))
        (table-props)))))

