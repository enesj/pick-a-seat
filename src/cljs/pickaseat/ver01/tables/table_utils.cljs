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

;(defn distance [p1 p2]
;  (let [x1 (- (:x p1) (:x p2))
;        x2 (* x1 x1)
;        y1 (- (:y p1) (:y p2))
;        y2 (* y1 y1)]
;    (if (< (Math/sqrt (+ x2 y2)) (+ d 3) ) true false)))
;
;(defn angles [table]
;  (let [{:keys [x y rect-right rect-bottom]} table]
;    {:lt {:x x :y y}
;     :ld {:x x :y rect-bottom}
;     :rt {:x rect-right :y y}
;     :rd {:x rect-right :y rect-bottom}}))

;(defn close-table [one t-xy]
;  (let [one-a (angles one)
;        two-a (angles t-xy)
;        {:keys [id x y width height]} one
;        {width-t :width height-t :height} t-xy]
;    (letfn [(ac [one-p two-p] [[one-p (one-p one-a)] [two-p (two-p two-a)]])]
;      (for [[one-p two-p]
;            (mapv #(apply ac %) [[:lt :ld] [:lt :rt] [:rd :rt] [:rd :ld] [:ld :lt] [:rt :lt] [:rt :rd] [:ld :rd]])
;            :let [one-k (key one-p) two-k (key two-p)]]
;        (if (distance (val one-p) (val two-p))
;          (cond
;            (and (= one-k :lt) (= two-k :ld)) [x (- y height-t) id :ltd]
;            (and (= one-k :lt) (= two-k :rt)) [(- x width-t) y id :tlr]
;            (and (= one-k :rd) (= two-k :rt)) [(- (+ x width) width-t) (+ y height) id :rdt]
;            (and (= one-k :rd) (= two-k :ld)) [(+ x width) (- (+ y height) height-t) id :drl]
;            (and (= one-k :ld) (= two-k :lt)) [x (+ y height) id :ldt]
;            (and (= one-k :rt) (= two-k :lt)) [(+ x width) y id :trl]
;            (and (= one-k :rt) (= two-k :rd)) [(- (+ x width) width-t) (- y height-t) id :rtd]
;            (and (= one-k :ld) (= two-k :rd)) [(- x width-t) (- (+ y height) height-t) id :dlr]
;            :else false) false)))))







