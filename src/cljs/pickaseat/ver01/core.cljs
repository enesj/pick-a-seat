(ns pickaseat.ver01.core
  (:require
    [reagent.core :as r]
    [goog.events :as events]
    [pickaseat.ver01.helper :as h]
    [pickaseat.ver01.data.table_data :as td]
    [pickaseat.ver01.tables.tables-core :as tc]
    [pickaseat.ver01.floor-map.floor-draw :as floor]
    [pickaseat.ver01.data.tables-templates :as tt]
    [pickaseat.ver01.tables.table-events :as tev]
    [pickaseat.ver01.data.common-data :as cd]
    [devtools.core :as devtools])
    ;[devtools.toolbox :as toolbox])
  (:import [goog.events EventType]))


(enable-console-print!)
(devtools/install! [:formatters :hints])

(def current-mode (r/atom 0))

(defn take-mode [n] (nth (cycle {:Layout floor/draw-floor :Tables tc/draw-tables}) n))


(defn menu-stage []
  [:div
   [:h4 {:style {:padding-left "10px"}} [:span {:style {:color "lightgray"}} (str (:restaurant @cd/common-data) ": ")]
                                        [:span (str  (:hall @cd/common-data))]]
   [:svg {:width "400px" :height "40px" :font-family "Courier New" :fill "blue" :font-size "15"}
    [:text {:opacity       0.8
            :font-weight "bold"
            :on-mouse-down (fn [e] (.preventDefault e)
                             (swap! current-mode inc))
            :x             10 :y 20}
     (name (first (take-mode  @current-mode)))]]
   [(second (take-mode  @current-mode))]])


(defn resize []
  (fn [evt]
    (td/settings-pos (* (/ (.-innerWidth js/window) 1000) (.-devicePixelRatio js/window)))))

(def mount-stage
    (with-meta menu-stage
               {:component-did-mount
                (fn [this]
                  (let [bcr (.getBoundingClientRect (r/dom-node this))
                        x (.-left bcr) y (+ (.-top bcr) 28)]  ;; 28 pxela visina naslova !!!
                    (swap! cd/common-data assoc-in [:svg] [x y])
                    (td/settings-pos (* (/ (.-innerWidth js/window) 1000) (.-devicePixelRatio js/window)))
                    (reset! td/history {:performed [@td/tables-state] :recalled []})
                    (events/listen js/window EventType.RESIZE (resize))
                    (events/listen js/window EventType.MOUSEUP (tev/mouse-up))))}))

(defn ^:export main []
  (r/render [mount-stage] (h/get-elem "app")))




(comment
  (defn init [template]
    (swap! td/tables-state assoc-in [:tables] (tt/table-templates template))
    (init :2)))

