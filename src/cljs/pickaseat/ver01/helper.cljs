(ns pickaseat.ver01.helper
  (:require [cljs.core.async :refer [chan close! put!]]
            [goog.events :as events]
            [goog.fx :as fx]
            [goog.fx.dom :as fx-dom]
            [goog.dom :as gdom]
            [goog.dom.forms :as gforms]
            [goog.net.XhrIo :as xhr]
            [pickaseat.ver01.data.table_data :as table-data])
            ;[ajax.core :as ajax]

  (:require-macros [cljs.core.async.macros :refer [go]]))


(defn resize []
  (fn [evt]
    (table-data/settings-pos (* (/ (.-innerWidth js/window) 1000) (.-devicePixelRatio js/window)) true)))


(defn timeout [ms]
      (let [c (chan)]
           (js/setTimeout (fn [] (close! c)) ms)
           c))

(defn get-value [elem]
      (gforms/getValue (gdom/getElement elem)))

(defn get-elem [id] (gdom/getElement id))

(defn get-attr [elem attr] (.getAttribute elem attr))


(defn cut-str-at
      "Cuts string at (- length 3) and adds \"...\" to the end of the returned string"
      [s length]
      (if (> (count s) length)
        (str (.substring s 0 (- length 3)) "...")
        s))

(defn fade-out
      ([] (fade-out 1000 nil))
      ([tm] (fade-out tm nil))
      ([tm callback]
       (fn [node]
           (let [anim (fx-dom/FadeOut. node tm)]
                (when callback
                      (events/listen anim js/goog.fx.Animation.EventType.END callback))
                (.play anim)))))

(defn fade-in
      ([] (fade-in 1000 nil))
      ([tm] (fade-in tm nil))
      ([tm callback]
       (fn [node]
           (let [anim (fx-dom/FadeIn. node tm)]
                (when callback
                      (events/listen anim js/goog.fx.Animation.EventType.END callback))
                (.play anim)))))
