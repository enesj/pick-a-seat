(ns pickaseat.ver01.floor-map.floor-components
  (:require
    [goog.events :as events])
  (:import [goog.events EventType]))

(defn drag-move-fn [on-drag]
  (fn [evt]
    (on-drag (.-clientX evt) (.-clientY evt))))

(defn drag-end-fn [drag-move drag-end on-end]
  (fn [evt]
    (events/unlisten js/window EventType.MOUSEMOVE drag-move)
    (events/unlisten js/window EventType.MOUSEUP @drag-end)
    (on-end)))

(defn dragging
  ([on-drag] (dragging on-drag (fn []) (fn [])))
  ([on-drag on-start on-end]
   (let [drag-move (drag-move-fn on-drag)
         drag-end-atom (atom nil)
         drag-end (drag-end-fn drag-move drag-end-atom on-end)]
     (on-start)
     (reset! drag-end-atom drag-end)
     (events/listen js/window EventType.MOUSEMOVE drag-move)
     (events/listen js/window EventType.MOUSEUP drag-end))))

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

(defn circle [center radius attributes]
  (let [[cx cy] center]
    [:circle (merge {:cx cx :cy cy :r radius}
                    attributes)]))


;(defn polygon
;  [ class-name color attributes points]
;  (if (nil? color)
;    [:polygon (merge {:points (apply points-str points)
;                      :class class-name}
;                     attributes)]
;    [:polygon (merge {
;                      ;:on-mouse-down #(dragging on-drag)
;                      :filter "url(#s1)"
;                      :points (apply points-str points)
;                      :class class-name
;                      :fill color
;                      :stroke "black"}
;                     attributes)]))

(defn polygon
  ([attributes points]
   [:polygon (merge {
                     :points points
                     :stroke "black"}
                    attributes)])
  ([attributes attributes-2 points]
   [:g {:key (:key attributes-2)}
    [:polygon (merge {
                      :points points
                      :stroke "black"}
                     attributes)]
    [:polygon (merge {
                      :points points
                      :stroke "black"}
                     attributes-2)]]))

(defn polyline
  [class-name attributes & points]
  [:polyline (merge {:points (apply points-str points)
                     :class class-name} attributes)])






