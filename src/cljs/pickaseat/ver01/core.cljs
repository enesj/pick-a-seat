(ns pickaseat.ver01.core
  (:require
    [reagent.core :as r]
    [goog.events :as events]
    [pickaseat.ver01.helper :as h]
    [pickaseat.ver01.data.table_data :as td]
    [pickaseat.ver01.tables.tables-core :as tc]
    [pickaseat.ver01.floor-map.core :as floor-core]
    [pickaseat.ver01.floor-map.draw :as floor]
    [pickaseat.ver01.tables.themes :as t]
    [pickaseat.ver01.tables.templates :as tt]
    [pickaseat.ver01.tables.table-events :as tev]
    [pickaseat.ver01.data.common-data :as cd]
    [devtools.core :as devtools]
    [devtools.toolbox :as toolbox])
    ;[devcards.core])
  (:import [goog.events EventType]))


(enable-console-print!)
(devtools/install! [:formatters :hints])

(def current-mode (r/atom 0))

(defn take-mode [n] (nth (cycle {:floor floor/draw-floor :tables tc/draw-tables}) n))

(defn menu-stage []
  [:div
   [:svg {:width "400px" :height "30px" :font-family "Courier New" :fill "blue" :font-size "15"}
    [:text {:opacity       0.8
            :on-mouse-down (fn [e] (.preventDefault e)
                             (js/console.log (name (first (take-mode  @current-mode))))
                             (swap! current-mode inc))
            :x             10 :y 20}
     (name (first (take-mode  @current-mode)))]]
   [(second (take-mode  @current-mode))]])



(def resize-fn
  (td/settings-pos (* (/ (.-innerWidth js/window) 1000) (.-devicePixelRatio js/window))))

(defn resize []
  (fn [evt] resize-fn))

(def mount-stage
    (with-meta menu-stage
               {:component-did-mount
                (fn [this]
                  (let [bcr (.getBoundingClientRect (r/dom-node this))
                        x (.-left bcr) y (+ (.-top bcr) 28)]  ;; 28 pxela visina naslova !!!
                    (swap! cd/common-data assoc-in [:svg] [x y])
                    resize-fn
                    (events/listen js/window EventType.RESIZE (resize))
                    (events/listen js/window EventType.MOUSEUP (tev/mouse-up))))}))

(defn ^:export main []
  (r/render [mount-stage] (h/get-elem "app")))




(comment
  (defn init [template]
    (swap! td/tables-state assoc-in [:tables] (tt/table-templates template))
    (init :2)))

