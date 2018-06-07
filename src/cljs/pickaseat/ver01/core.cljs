(ns pickaseat.ver01.core
  (:require
    [reagent.core :as r]
    [goog.events :as events]
    [pickaseat.ver01.helper :as helper]
    [pickaseat.ver01.data.table_data :as table-data]
    [pickaseat.ver01.tables.tables-core :as tables-core]
    [pickaseat.ver01.floor-map.floor-core :as floor-core]
    [pickaseat.ver01.data.tables-templates :as tables-templates]
    [pickaseat.ver01.tables.table-events :as table-events]
    [pickaseat.ver01.data.common-data :as common-data]
    [devtools.core :as devtools])
    ;[devtools.toolbox :as toolbox])
  (:import [goog.events EventType]))


(enable-console-print!)
(devtools/install! [:formatters :hints])

(def current-mode (r/atom 0))

(defn take-mode [n] (nth (cycle {:Layout floor-core/draw-floor :Tables tables-core/draw-tables}) n))

(defn app []
  [:div
   [:h4 {:style {:padding-left "10px"}} [:span {:style {:color "lightgray"}} (str (:restaurant @common-data/data) ": ")]
                                        [:span (str (:hall @common-data/data))]]
   [:svg {:width "400px" :height "40px" :font-family "Courier New" :fill "blue" :font-size "15"}
    [:text {:opacity       0.8
            :font-weight "bold"
            :on-mouse-down (fn [e] (.preventDefault e)
                             (swap! current-mode inc))
            :x             10 :y 20}
     (name (first (take-mode  @current-mode)))]]
   [(second (take-mode  @current-mode))]])


;(defn resize []
;  (fn [evt]
;    (td/settings-pos (* (/ (.-innerWidth js/window) 1000) (.-devicePixelRatio js/window)))))

(def mount-app
    (with-meta app
               {:component-did-mount
                (fn [this]
                  (let [bcr (.getBoundingClientRect (r/dom-node this))
                        x (.-left bcr) y (+ (.-top bcr) 90)]  ;; 28 pxela visina naslova !!!
                    (swap! common-data/data assoc-in [:svg] [x y])
                    (table-data/settings-pos (* (/ (.-innerWidth js/window) 1000) (.-devicePixelRatio js/window)) true)
                    (reset! table-data/history {:performed [@table-data/tables-state] :recalled []})
                    (events/listen js/window EventType.RESIZE (helper/resize))
                    (events/listen js/window EventType.MOUSEUP (table-events/mouse-up))))}))

(defn ^:export main []
  (r/render [mount-app] (helper/get-elem "app")))


(comment
  (defn init [template]
    (swap! table-data/tables-state assoc-in [:tables] (tables-templates/table-templates template))
    (init :2)))

