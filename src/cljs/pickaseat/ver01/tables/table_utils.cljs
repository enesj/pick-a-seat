(ns pickaseat.ver01.tables.table-utils)

(defn start-end [start end]
  ((juxt (partial mapv min) (partial mapv max)) (vals start) (vals end)))

(def d 16)
(def d1 25)

(defn >+ [d x y]
  (> x (+ y d)))



(defn <+ [d x y]
  (< x (- y d)))


(defn collides-sel
  [one t-xy d]
  (let [
        {:keys [x y rect-right rect-bottom]} t-xy
        {x1 :x y1 :y rect-right1 :rect-right rect-bottom1 :rect-bottom} one]
    (cond
      (<+ d rect-right1 x) false
      (>+ d x1 rect-right) false
      (<+ d rect-bottom1 y) false
      (>+ d y1 rect-bottom) false
      :else (:id one))))

(defn collides-sel-active
  [one t-xy d]
  (let [{:keys [x y rect-right rect-bottom]} t-xy
        {x1 :x y1 :y rect-right1 :rect-right rect-bottom1 :rect-bottom} one]
    (cond
      (<+ d x1 x) false
      (>+ d rect-right1 rect-right) false
      (<+ d y1 y) false
      (>+ d rect-bottom1 rect-bottom) false
      :else (:id one))))

(defn collides-with
  ([one t-x t-y]
   (let [{x1 :x y1 :y rect-right1 :rect-right rect-bottom1 :rect-bottom} one
         {:keys [x y rect-right rect-bottom]} t-x
         dir-y (cond
                 (<+ d rect-right1 x) :y
                 (>+ d x1 rect-right) :y
                 (<+ d rect-bottom1 y) :y
                 (>+ d y1 rect-bottom) :y)
         {:keys [x y rect-right rect-bottom]} t-y
         dir-x (cond
                 (<+ d rect-right1 x) :x
                 (>+ d x1 rect-right) :x
                 (<+ d rect-bottom1 y) :x
                 (>+ d y1 rect-bottom) :x)]
     (if (and dir-x dir-y) false (or dir-x dir-y :xy))))
  ([one t-xy]
   (let [{id1 :id x1 :x y1 :y rect-right1 :rect-right rect-bottom1 :rect-bottom} one
         {:keys [x y rect-right rect-bottom]} t-xy]
     (cond
       (<+ d rect-right1 x) false
       (>+ d x1 rect-right) false
       (<+ d rect-bottom1 y) false
       (>+ d y1 rect-bottom) false
       :else {:xy id1}))))

(defn distance [p1 p2]
  (let [x1 (- (:x p1) (:x p2))
        x2 (* x1 x1)
        y1 (- (:y p1) (:y p2))
        y2 (* y1 y1)]
    (if (< (Math/sqrt (+ x2 y2)) (+ d 6) ) (Math/sqrt (+ x2 y2)) false)))

(defn corners [table]
  (let [{:keys [x y rect-right rect-bottom]} table]
    {:lt {:x x :y y}
     :ld {:x x :y rect-bottom}
     :rt {:x rect-right :y y}
     :rd {:x rect-right :y rect-bottom}}))

(defn close-table [other-table my-table]
  (let [corners-other (corners other-table)
        corners-my (corners my-table)
        {:keys [id x y width height]} other-table
        {width-my :width height-my :height} my-table]
    (letfn [(ac [other-corner my-corner] [[other-corner (other-corner corners-other)] [my-corner (my-corner corners-my)]])]
        (remove nil?
          (for [[other-corner my-corner]
                (mapv #(apply ac %) [[:lt :ld] [:lt :rt] [:ld :lt] [:ld :rd]  [:rt :lt] [:rt :rd] [:rd :rt] [:rd :ld]])
                :let [first-corner (first other-corner)
                      second-corner (first my-corner)
                      distance (distance (second other-corner) (second my-corner))]]
              (when distance
                (cond
                  (and (= first-corner :lt) (= second-corner :ld)) [x (- y height-my) id :ltd distance]
                  (and (= first-corner :lt) (= second-corner :rt)) [(- x width-my) y id :tlr distance]
                  (and (= first-corner :ld) (= second-corner :lt)) [x (+ y height) id :ldt distance]
                  (and (= first-corner :ld) (= second-corner :rd)) [(- x width-my) (- (+ y height) height-my) id :dlr distance]
                  (and (= first-corner :rd) (= second-corner :rt)) [(- (+ x width) width-my) (+ y height) id :rdt distance]
                  (and (= first-corner :rd) (= second-corner :ld)) [(+ x width) (- (+ y height) height-my) id :drl distance]
                  (and (= first-corner :rt) (= second-corner :lt)) [(+ x width) y id :trl distance]
                  (and (= first-corner :rt) (= second-corner :rd)) [(- (+ x width) width-my) (- y height-my) id :rtd distance]
                  :else false)))))))








