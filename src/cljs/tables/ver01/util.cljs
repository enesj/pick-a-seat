(ns tables.ver01.util
  (:require [tables.ver01.table_data :as td]))


(defn table-props [tables]
  (let [table-dims (:table-dims @td/settings-base)]
    (doall (for [table tables
                 :let [id (key table)
                       table-v (val table)
                       {:keys [seats block rs pos rotation]} table-v
                       [x y] pos
                       [width height]  (seats table-dims)
                       [width height] (if rotation [width height] [height width])
                       rect-right (+ x width)
                       rect-bottom (+ y height)
                       rect {:id id :x x :y y :width width :height height :s seats :block block :rs rs :rect-right rect-right :rect-bottom rect-bottom}]]
            rect))))

(def d 2)
(def d1 25)

(defn >+ [d x y]
  (> x (+ y d)))

(defn <+ [d x y]
  (< x (- y d)))


(defn collides-sel-active
  [one t-xy d]
  (let [{:keys [x y width height rect-right rect-bottom]} t-xy
        [x1 width1](if (neg? width) [rect-right (- width)] [x width])
        [y1 height1](if (neg? height) [rect-bottom (- height)] [y height])
        [rect-right rect-bottom ] [(+ x1 width1) (+ y1 height1)]
         { x :x  y :y rect-right1 :rect-right rect-bottom1 :rect-bottom} one]
    (cond
      (<+ d x x1) false
      (>+ d rect-right1 rect-right) false
      (<+ d y y1) false
      (>+ d rect-bottom1 rect-bottom) false
      :else (:id one))))

(defn collides-sel
  [one t-xy d]
  (let [
         {:keys [x y width height rect-right rect-bottom]} t-xy
         [x1 width1](if (neg? width) [rect-right (- width)] [x width])
         [y1 height1](if (neg? height) [rect-bottom (- height)] [y height])
         [rect-right rect-bottom] [(+ x1 width1) (+ y1 height1)]
         { x :x  y :y rect-right1 :rect-right rect-bottom1 :rect-bottom} one]
    (cond
      (<+ d rect-right1 x1) false
      (>+ d x rect-right) false
      (<+ d rect-bottom1 y1) false
      (>+ d y rect-bottom) false
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
         {:keys [id x y rect-right rect-bottom]} t-xy]
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
    (if (< (Math/sqrt (+ x2 y2)) 10) true false)))

(defn angles [table]
  (let [{:keys [x y rect-right rect-bottom]} table]
    {:lt {:x x :y y}
     :ld {:x x :y rect-bottom}
     :rt {:x rect-right :y y}
     :rd {:x rect-right :y rect-bottom}}))

(defn close-table [one t-xy]
  (let [one-a (angles one)
        two-a (angles t-xy)]
    (letfn [(ac [one-p two-p] [[one-p (one-p one-a)] [two-p (two-p two-a)]])]
      (for [[one-p two-p]
            (mapv #(apply ac %) [[:lt :ld] [:lt :rt] [:rd :rt] [:rd :ld] [:ld :lt] [:rt :lt] [:rt :rd] [:ld :rd]])
            :let [one-k (key one-p) two-k (key two-p)]]
        (if (distance (val one-p) (val two-p))
          (cond
            (and (= one-k :lt) (= two-k :ld)) [(:x one) (- (:y one) (:h t-xy)) (:id one) :ltd]
            (and (= one-k :lt) (= two-k :rt)) [(- (:x one) (:width t-xy)) (:y one) (:id one) :tlr]
            (and (= one-k :rd) (= two-k :rt)) [(- (+ (:x one) (:width one)) (:width t-xy)) (+ (:y one) (:height one)) (:id one) :rdt]
            (and (= one-k :rd) (= two-k :ld)) [(+ (:x one) (:width one)) (- (+ (:y one) (:height one)) (:height t-xy)) (:id one) :drl]
            (and (= one-k :ld) (= two-k :lt)) [(:x one) (+ (:y one) (:height one)) (:id one) :ldt]
            (and (= one-k :rt) (= two-k :lt)) [(+ (:x one) (:width one)) (:y one) (:id one) :trl]
            (and (= one-k :rt) (= two-k :rd)) [(- (+ (:x one) (:width one)) (:width t-xy)) (- (:y one) (:height t-xy)) (:id one) :rtd]
            (and (= one-k :ld) (= two-k :rd)) [(- (:x one) (:width t-xy)) (- (+ (:y one) (:height one)) (:height t-xy)) (:id one) :dlr]
            :else false) false)))))



(let [{ a1 :a b :b  } {:a  1 :b 2}]
  [a1 b])






