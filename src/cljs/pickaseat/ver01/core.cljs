(ns pickaseat.ver01.core
  (:require
    [reagent.core :as r]
    [goog.events :as events]
    [pickaseat.ver01.helper :as h]
    [pickaseat.ver01.data.table_data :as td]
    [pickaseat.ver01.tables.tables-core :as tc]
    [pickaseat.ver01.floor-map.core :as floor-core]
    [pickaseat.ver01.floor-map.testboard :as floor]
    [pickaseat.ver01.tables.themes :as t]
    [pickaseat.ver01.tables.templates :as tt]
    [pickaseat.ver01.tables.table-events :as tev]
    [pickaseat.ver01.data.common-data :as cd]
    [devtools.core :as devtools]
    [devtools.toolbox :as toolbox]
    [devcards.core])
  (:import [goog.events EventType]))

(enable-console-print!)
(devtools/install! [:formatters :hints])

;(def current-mode (atom tc/table-mode))
(def current-mode (atom floor-core/floor-mode))

(defn modes []
  (let [mode (@current-mode)
        {:keys [events root w h]} mode]
    {:events events
     :root   root}))

(defn stage []
  (let [{:keys [events root]} (modes)
        {:keys [key-down key-up mouse-down mouse-move]} events
         common-data @cd/common-data]
    [:div {:style {:font-size "20px" :margin-top "-20px"}}
     [:div {:style {:padding-left "5%"}} "Velika Sala"]
     [:svg
      {:fill          (:text t/palete)
       :width         (:w common-data)
       :height        (:h common-data)
       :on-key-down   key-down
       :on-key-up     key-up
       :on-mouse-down mouse-down
       :on-mouse-move mouse-move}
      [floor/draw-floor]]]))

(def resize-fn
  (td/settings-pos (* (/ (.-innerWidth js/window) 1000) (.-devicePixelRatio js/window))))

(defn resize []
  (fn [evt] resize-fn))

(def mount-stage
  (with-meta stage
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

