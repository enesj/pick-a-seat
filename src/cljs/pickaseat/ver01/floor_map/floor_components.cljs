(ns pickaseat.ver01.floor-map.floor-components
  (:require
    [goog.events :as events]
    [pickaseat.ver01.data.floor-data :as floor-data])
  (:import [goog.events EventType]))


(defn path [path-string]
  [:path {:d path-string}])


(defn group-svg [id & elements]
  (into [:g {:id id}] elements))



(defn point-str [[x y]]
  (str x "," y))

(defn points-str [& points]
  (clojure.string/join " " (map point-str points)))

(defn line [p1 p2]
  (let [[x1 y1] p1
        [x2 y2] p2]
    [:line {:x1 x1 :y1 y1 :x2 x2 :y2 y2}]))

(defn color-line
  [stroke  points attributes]
  (let [[[x1 y1] [x2 y2]] points]
    [:line (merge {:x1 x1 :y1 y1 :x2 x2 :y2 y2 :stroke stroke}
                  attributes)]))



(defn polyline
  [class-name attributes & points]
  [:polyline (merge {:points (apply points-str points)
                     :class class-name} attributes)])

(defn drag-move-fn [on-drag start]
  (fn [evt]
    ;pageY (aget evt "event_" "pageY")  XY poyicija misa na strani
    ;pageX (aget evt "event_" "pageX")
    (.preventDefault evt)
    ;(js/console.log "d-m-f" on-drag (.-clientX evt) (.-clientY evt) start)
    (on-drag (.-clientX evt) (.-clientY evt) start)))

(defn drag-end-fn [drag-move]
  (fn [evt]
    (.preventDefault evt)
    (events/unlisten js/window EventType.MOUSEMOVE drag-move)
    (aset js/document "body" "style" "cursor" "default")))

(defn dragging
  ([on-drag start sel-ids]
   ;(js/console.log "sdfds" on-drag)
   (let [drag-move (drag-move-fn (on-drag sel-ids) start)
         drag-end (drag-end-fn drag-move)]
     (events/listen js/window EventType.MOUSEMOVE drag-move)
     (events/listen js/window EventType.MOUSEUP drag-end))))

(defn circle [center radius attributes show-center? on-drag]
  (let [[cx cy] center]
    [:g {:key (rand 1000)}
     [:circle (merge {:cx cx :cy cy :r radius
                      :on-mouse-up   (fn [e]
                                       (when (:id attributes)
                                         (swap! floor-data/floor-state assoc-in [:selection :selected] [(:id attributes)])))
                      :on-mouse-down (if on-drag (fn [e] (.stopPropagation e)
                                                   (dragging on-drag [(.-clientX e) (.-clientY e)] [(:id attributes) [center radius] nil])))}
                     attributes)]
     (if show-center? (circle center 0 {:r 2 :fill "rgba(0,0,0,0.8)"} false nil))]))


(defn polygon [attributes points on-drag]
   [:polygon (merge {:transform "scale(1)"
                     :points points
                     :stroke "black"
                     :on-mouse-up   (fn [e]  (swap! floor-data/floor-state assoc-in [:selection :selected] [(:id attributes)]))
                     :on-mouse-down (when on-drag (fn [e] (.stopPropagation e) (dragging on-drag [(.-clientX e) (.-clientY e)] [(:id attributes) points])))}
                    attributes)])









