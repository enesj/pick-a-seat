(ns pickaseat.ver01.floor-map.floor-common
  (:require [pickaseat.ver01.floor-map.floor-components :as floor-components]
            [pickaseat.ver01.data.common-data :refer [data]]))

(defn snap-round [x]
  (let [snap (:snap @data)]
    (* (Math/round (/ x snap)) snap)))

(defn draw-figures [figures opacity move-figures]
  (for [figure (sort-by key figures)]
    (let [fig (first (val figure))]
      (case (key fig)
        :polygon (floor-components/polygon
                   {:id      (key figure)
                    :key     (key figure)
                    :stroke  "black"
                    :stroke-width 2
                    :fill    "rgba(255,255,255,0.1)"
                    :opacity opacity}
                    ;:filter  "url(#s1)"}
                   (val fig)
                   (:polygon move-figures))
        :circle
          (floor-components/circle (first (val fig))
                                   (second (val fig))
                                   {:id      (key figure)
                                    :key     (key figure)
                                    :stroke  "black"
                                    :stroke-width 2
                                    :fill    "rgba(255,255,255,0.1)"
                                    :opacity opacity}
                                   ;:filter  "url(#s1)"}
                                   true
                                   (:circle move-figures))))))

