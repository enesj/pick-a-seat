(ns pickaseat.core
  (:require
    [reagent.core :as r]
    [goog.events :as events]
    [pickaseat.helper :as helper]
    [pickaseat.data.table-data :as table-data]
    [pickaseat.tables.tables-core :as tables-core]
    [pickaseat.floor-map.floor-core :as floor-core]
    [pickaseat.data.tables-templates :as tables-templates]
    [pickaseat.tables.table-events :as table-events]
    [pickaseat.data.common :as common]
    [devtools.core :as devtools])
    ;[devtools.toolbox :as toolbox])
  (:import [goog.events EventType]))


(enable-console-print!)
(devtools/install! [:formatters :hints])

(def current-mode (r/atom :tables))

(defn take-mode [mode] (mode {:tables ["Tables" tables-core/tables] :layout ["Layout" floor-core/floor]}))


(defn app []
  [:div
   [:h4 {:style {:padding-left "10px"}} [:span {:style {:color "lightgray"}} (str (:restaurant @common/data) ": ")]
                                        [:span (str (:hall @common/data))]]
   [:svg {:width "400px" :height "40px" :font-family "Courier New" :fill "blue" :font-size "15"}
    [:text {:opacity       0.8
            :font-weight "bold"
            :on-mouse-down (fn [e] (.preventDefault e)
                             (swap! current-mode #(if (= % :tables) :layout :tables)))
            :x             10 :y 20}
     (first (take-mode  @current-mode))]]
   [(second (take-mode  @current-mode)) (r/current-component)]])


;(defn resize []
;  (fn [evt]
;    (td/settings-pos (* (/ (.-innerWidth js/window) 1000) (.-devicePixelRatio js/window)))))

(def mount-app
    (with-meta app
               {:component-did-mount
                (fn [this]
                  (let []
                    (table-data/settings-pos (* (/ (.-innerWidth js/window) 1000) (.-devicePixelRatio js/window)) true)
                    (reset! table-data/history {:performed [@table-data/tables-state] :recalled []})
                    (events/listen js/window EventType.RESIZE (helper/resize))
                    (events/listen js/window EventType.MOUSEUP (table-events/mouse-up))))}))


(defn ^:export main []
  (r/render [mount-app] (helper/get-elem "app")))



;(main)

(comment
  (defn init [template]
    (swap! table-data/tables-state assoc-in [:tables] (tables-templates/table-templates template))
    (init :2)))

