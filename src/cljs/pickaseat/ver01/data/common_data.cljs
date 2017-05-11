(ns pickaseat.ver01.data.common-data
  (:require [reagent.core :as r]))

(def common-data
  (r/atom {:svg []
           :w   2000
           :h   2000}))
