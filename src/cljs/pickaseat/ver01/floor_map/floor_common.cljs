(ns pickaseat.ver01.floor-map.floor-common
  (:require [pickaseat.ver01.floor-map.floor-components :as floor-components]
            [pickaseat.ver01.data.common-data :refer [data]]
            [pickaseat.ver01.data.floor-data :as floor-data]))

(defn snap-round [x]
  (let [snap (:snap @data)]
    (* (Math/round (/ x snap)) snap)))

