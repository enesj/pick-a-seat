(ns pickaseat.tables.table-svg
  (:require
    [pickaseat.data.table-data :as table-data]))


(defn rec-path [x y & z]
  (apply str "M" x "," y z))

(defn merege-path [x]
  [:path (merge table-data/menu-defaults x)])

(declare ok cancel a-top a-down a-left a-right)

(defn RoundedRect [x, y, w, h, r dir]
  (let [up (str " v" (- h))
        up1 (str " v" (- r h))
        up2 (str " v" (- (* 2 r) h))
        dw (str " v" h)
        dw1 (str " v" (- h r))
        dw2 (str " v" (- h (* 2 r)))
        lft (str " h" (- w))
        lft1 (str " h" (- r w))
        lft2 (str " h" (- (* 2 r) w))
        rgh (str " h" w)
        rgh1 (str " h" (- w r))
        rgh2 (str " h" (- w (* 2 r)))
        a1 (str " a" r "," r " 0 0 1 " r "," (- r))
        a2 (str " a" r "," r " 0 0 1 " r "," r)
        a3 (str " a" r "," r " 0 0 1 " (- r) "," r)
        a4 (str " a" r "," r " 0 0 1 " (- r) "," (- r))
        a5 (str " a" r "," r " 0 0 0 " r "," r)
        a6 (str " a" r "," r " 0 0 0 " (- r) "," (- r))
        paths {:l           [(+ x w) (+ y h), lft1, a4, up2, a1, rgh1]
               :t           [x (+ y h), up1, a1, rgh2, a2, dw1]
               :r           [x y, rgh1, a2, dw2, a3, lft1]
               :d           [(+ x w) y, dw1, a3, lft2, a4, up1]
               :lt1         [x (+ y h), up1, a1, rgh1, dw]
               :lt0         [x y, rgh1, a2, dw2, a3, lft2, a4]
               :rt1         [x y, rgh1, a2, dw1, lft]
               :rt0         [(+ x w) y, dw1, a3, lft2, a4, up2, a1]
               :rd1         [(+ x w) y, dw1, a3, lft2, up]
               :rd0         [(+ x w) (+ y h), lft1, a4, up2, a1, rgh2, a2]
               :ld1         [(+ x w) (+ y h ), lft1, a4, up1, rgh]
               :ld0         [x (+ y h), up1, a1, rgh2, a2, dw2, a3]
               :td          [x (+ y h), up1, a1, rgh1, dw1, a3]
               :dt          [(+ x w) (+ y h), lft1, a4, up1, rgh1, a2]
               :v-menu      [(+ x w (- r)) (+ y h), lft2, a4, up, a5, rgh2, a2, dw, a6]
               :v-menu-1    [(+ x w (- r)) (+ y h), lft2, a4, up1, rgh1, a2, dw, a6]
               :v-menu-last [(+ x w (- r)) (+ y h), lft2, a4, up, a5, rgh2, a2, dw2, a3]
               :h-menu      [(+ x w (- r)) (+ y h), lft2, a4, up2, a6, rgh, a2, dw2, a5 " z"]
               :h-menu-1    [(+ x w (- r)) (+ y h), lft2, a4, up1, rgh1, a2, dw2, a5 " z"]
               :h-menu-last [(+ x w (- r)) (+ y h), lft2, a4, up2, a6, rgh, a2, dw2, a3]}]
    {:d (apply rec-path (dir paths))}))



(defn all-tabs
  ([] (all-tabs nil nil nil nil nil nil))
  ([x1 y1 w2 h2 arr1 arr2]

   {:ok     {:icon [:g
                    (merege-path {:d (str "M" (+ x1 (/ w2 8)) "," (+ y1 (/ h2 2.5)) " l" (/ w2 4) " " (/ h2 2)) :stroke-width 3 :stroke "blue" :stroke-opacity 0.6})
                    (merege-path {:d (str "M" (+ x1 (/ w2 3)) "," (+ y1 (/ h2 1.1)) " l" (/ w2 2) " " (/ (- h2) 1.2)) :stroke-width 1.5 :stroke "blue" :stroke-opacity 0.6})]}
    :cancel {:icon [:g {:view-box "0 0 1024 1024"}
                    (merege-path {:d (str "M" (+ x1 (/ w2 8)) "," (+ y1 (/ h2 8)) " l" (/ w2 1.5) " " (/ h2 1.3)) :stroke-width 2 :stroke "red" :stroke-opacity 0.6})
                    (merege-path {:d (str "M" (+ x1 (/ w2 8)) "," (+ y1 (/ h2 8) (/ h2 1.3)) " l" (/ w2 1.5) " " (/ (- h2) 1.3)) :stroke-width 2 :stroke "red" :stroke-opacity 0.6})]}
    :at     {:icon [:g
                    (merege-path {:d (str "M" x1 "," y1 " h" w2) :stroke-width 1.5})
                    (merege-path {:d (str " M" (+ x1 (/ w2 2)) "," y1 " v" h2)})
                    (merege-path {:d (str "M" (+ x1 (/ w2 2)) "," y1 " l" (- arr2) "," arr1 " h" (* arr2 2) " l" (- arr2) ", " (- arr1) "z") :fill "orange"})]}
    :ad     {:icon [:g
                    (merege-path {:d (str "M" x1 "," (+ y1 h2) " h" w2) :stroke-width 1.5})
                    (merege-path {:d (str " M" (+ x1 (/ w2 2)) "," (+ y1 h2) " v" (- h2))})
                    (merege-path {:d (str "M" (+ x1 (/ w2 2)) "," (+ y1 h2) " l" (- arr2) "," (- arr1) " h" (* arr2 2) " l" (- arr2) "," arr1 " z") :fill "orange"})]}
    :al     {:icon [:g
                    (merege-path {:d (str "M" x1 "," y1 " v" h2) :stroke-width 1.5})
                    (merege-path {:d (str " M" x1 "," (+ y1 (/ h2 2)) " h" w2)})
                    (merege-path {:d (str "M" x1 "," (+ y1 (/ h2 2)) " l" arr1 "," arr2 " v" (- (* arr2 2)) " l" (- arr1) "," arr2 " z") :fill "orange"})]}
    :ar     {:icon [:g
                    (merege-path {:d (str "M" (+ x1 w2) "," y1 " v" h2) :stroke-width 1.5})
                    (merege-path {:d (str " M" (+ x1 w2) "," (+ y1 (/ h2 2)) " h" (- w2))})
                    (merege-path {:d (str "M" (+ x1 w2) "," (+ y1 (/ h2 2)) " l" (- arr1) ", " arr2 " v" (- (* arr2 2)) " l" arr1 ", " arr2 " z") :fill "orange"})]}}))

(defn  delete-tables [x y w2 h2]
  [:g
   (merege-path {:d (str "M" (+ x (/ w2 8)) "," (+ y (/ h2 8)) " l" (/ w2 1.5) " " (/ h2 1.3)) :stroke-width 2 :stroke "red" :stroke-opacity 0.6})
   (merege-path {:d (str "M" (+ x (/ w2 8)) "," (+ y (/ h2 8) (/ h2 1.3)) " l" (/ w2 1.5) " " (/ (- h2) 1.3)) :stroke-width 2 :stroke "red" :stroke-opacity 0.6})])
