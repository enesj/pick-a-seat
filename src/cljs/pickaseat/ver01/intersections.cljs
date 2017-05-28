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

(defn poly-poly-intersection [shadow poly]
  (count (.-points
           (js/SvgIntersections.intersect
             (js/SvgIntersections.shape "polyline" #js {:points (points-str (flatten shadow))})
             (js/SvgIntersections.shape "polyline" #js {:points (points-str (flatten poly))})))))

(defn self-poly-intersections [shadow]
  (let [segment 8]
    (loop [ok true
           flat-shadow (flatten (partition 2 1 shadow))]
      (let [start (take segment flat-shadow)
            end (drop segment flat-shadow)]
        (if (and (not-empty flat-shadow) ok)
          (recur
            (if (> 2 (poly-poly-intersection start end))
              true
              false)
            (drop segment flat-shadow))
          ok)))))