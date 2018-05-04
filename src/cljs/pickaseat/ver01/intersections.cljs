(ns pickaseat.ver01.intersections)

(defn points-str [points]
  (loop [point-str ""
         points points
         separator " "]
    (if (not-empty points)
      (recur (str point-str (if (not= point-str "") separator) (first points))
             (rest points)
             (if (= separator ",") " " ","))
      point-str)))

(defn line-intersections [line1 line2]
  (let [[[x11 y11] [x12 y12]] line1
        [[x21 y21] [x22 y22]] line2
        points (.-points
                 (js/SvgIntersections.intersect
                   (js/SvgIntersections.shape "line" #js {:x1 x11 :y1 y11 :x2 x12 :y2 y12})
                   (js/SvgIntersections.shape "line" #js {:x1 x21 :y1 y21 :x2 x22 :y2 y22})))
        point  (aget points 0)]
    point))


(defn line-rect-intersections [poly rect]
  (let [[x y width height ] rect
        points (.-points
                 (js/SvgIntersections.intersect
                   (js/SvgIntersections.shape "rect" #js {:x x :y y :width width :height height})
                   (js/SvgIntersections.shape "polyline" #js {:points (points-str poly)})))]
    (count points)))

(defn circle-rect-intersections [circle rect]
  (let [[[cx cy] r] circle
        [x y width height ] rect
        points (.-points
                 (js/SvgIntersections.intersect
                   (js/SvgIntersections.shape "rect" #js {:x x :y y :width width :height height})
                   (js/SvgIntersections.shape "circle" #js {:cx cx :cy cy :r r})))]
    (count points)))

(defn poly-poly-intersection [poly-1 poly-2]
  (count (.-points
           (js/SvgIntersections.intersect
             (js/SvgIntersections.shape "polyline" #js {:points (points-str (flatten poly-1))})
             (js/SvgIntersections.shape "polyline" #js {:points (points-str (flatten poly-2))})))))


(defn self-poly-intersections [poly]
  (let [segment 4
        poly (take (count poly) (partition 2 1 (cycle poly)))
        flat-poly (flatten poly)
        flat-poly-segments  (vec (take (count poly) (partition segment (cycle flat-poly))))
        indexed-poly (map-indexed (fn [idx itm] [idx itm]) flat-poly-segments)]
       (for [index indexed-poly
             :let [current (first index)
                   ;poly (mapv second flat-poly-segments)
                   start (flat-poly-segments current)
                   left (apply subvec flat-poly-segments (if (zero? current) [(dec (count poly)) (count poly)] [(dec current) current]))
                   right (apply subvec flat-poly-segments (if (= current (dec (count poly))) [0 1] [(inc current) (+ current 2)]))
                   end-1 (subvec flat-poly-segments (if (= current (dec (count poly))) 1 0) (if (zero? current) current (dec current)))
                   end-2 (subvec flat-poly-segments (if (= current  (dec (count poly))) (inc current) (+ current 2)) (if (zero? current) (dec (count poly)) (count poly)))]]
         (if (and (> 1 (poly-poly-intersection start end-1)) (> 1 (poly-poly-intersection start end-2))) false index))))

