(ns pickaseat.ver01.floor-map.floor-common
  (:require [pickaseat.ver01.floor-map.floor-components :as comps]))



(defn draw-figures [figures opacity move-figures]
  (for [figure (sort-by key figures)]
    (let [fig (first (val figure))]
      (case (key fig)
        :polygon (comps/polygon
                   {:id      (key figure)
                    :key     (key figure)
                    :stroke  "black"
                    :fill    "white"
                    :opacity opacity
                    :filter  "url(#s1)"}
                   (val fig)
                   move-figures)))))
