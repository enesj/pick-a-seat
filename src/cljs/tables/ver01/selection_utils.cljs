(ns tables.ver01.selection-utils
  (:use [com.rpl.specter :only [select transform setval FIRST LAST ALL keypath filterer srange comp-paths compiled-select collect-one compiled-setval]])
  (:require [tables.ver01.table_data :as td]))

(defn rec-path [x y & z]
  (apply str "M" x "," y z))

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
               :ld1         [(+ x w) (+ y h), lft1, a4, up1, rgh]
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

(defn merege-path [x]
  [:path (merge td/menu-defaults x)])

(defn a-top []
  (println "at"))

(defn a-down []
  (println "ad"))

(defn a-left []
  (println "al"))

(defn a-right []
  (println "ar"))


(defn check-a-top [selected]
  (rand-nth  [:at nil]))


(defn check-a-down [selected]
  (rand-nth  [:ad nil]))


(defn check-a-left [selected]
  (rand-nth  [:al nil]))


(defn check-a-right [selected]
  (rand-nth  [:ar nil]))


(defn all-tabs
  ([] (all-tabs nil nil nil nil nil nil))
  ([x1 y1 w2 h2 arr1 arr2]
   {:at {:icon [:g
                (merege-path {:d (str "M" x1 "," y1 " h" w2) :stroke-width 1.5})
                (merege-path {:d (str " M" (+ x1 (/ w2 2)) "," y1 " v" h2)})
                (merege-path {:d (str " M" (+ x1 (/ w2 2)) "," y1 " v" h2)})
                (merege-path {:d (str "M" (+ x1 (/ w2 2)) "," y1 " l" (- arr2) "," arr1 " h" (* arr2 2) " l" (- arr2) ", " (- arr1) "z") :fill "black"})]
         :func a-top
         :check-func check-a-top}
    :ad {:icon [:g
                (merege-path {:d (str "M" x1 "," (+ y1 h2) " h" w2) :stroke-width 1.5})
                (merege-path {:d (str " M" (+ x1 (/ w2 2)) "," (+ y1 h2) " v" (- h2))})
                (merege-path {:d (str "M" (+ x1 (/ w2 2)) "," (+ y1 h2) " l" (- arr2) "," (- arr1) " h" (* arr2 2) " l" (- arr2) "," arr1 " z") :fill "black"})]
         :func a-down
         :check-func check-a-down}
    :al {:icon [:g
                (merege-path {:d (str "M" x1 "," y1 " v" h2) :stroke-width 1.5})
                (merege-path {:d (str " M" x1 "," (+ y1 (/ h2 2)) " h" w2)})
                (merege-path {:d (str "M" x1 "," (+ y1 (/ h2 2)) " l" arr1 "," arr2 " v" (- (* arr2 2)) " l" (- arr1) "," arr2 " z") :fill "black"})]
         :func a-left
         :check-func check-a-left}
    :ar {:icon [:g
                (merege-path {:d (str "M" (+ x1 w2) "," y1 " v" h2) :stroke-width 1.5})
                (merege-path {:d (str " M" (+ x1 w2) "," (+ y1 (/ h2 2)) " h" (- w2))})
                (merege-path {:d (str "M" (+ x1 w2) "," (+ y1 (/ h2 2)) " l" (- arr1) ", " arr2 " v" (- (* arr2 2)) " l" arr1 ", " arr2 " z") :fill "black"})]
         :func a-right
         :check-func check-a-right}}))



(def check-func
  (comp-paths ALL LAST :check-func))

(defn sel-menu-tabs []
  (let [active-tabs (doall (map-indexed vector (filterv boolean (mapv #(% 1)  (compiled-select check-func (all-tabs))))))]
    (mapv  #(hash-map :type (second %) :pos (first %) :h-menu :h-menu-1 :v-menu :v-menu-1) active-tabs)))

(defn sel-menu-tab [tab [x y w h r dir]]
  (let [{:keys [type]} tab
        x1 (+ x r)
        y1 (+ y r)
        w2 (- w (* 2 r))
        h2 (- h (* 2 r))
        arr1 (/ w 4)
        arr2 (/ w 12)
        menu-defaults (merge td/menu-defaults {:on-mouse-down (fn [e] ((:func (type (all-tabs)))))})]
    ^{:key type} [:g [:path (merge menu-defaults (RoundedRect x, y, w, h, r, dir) {:id type})]
                  (:icon (type
                           (all-tabs x1 y1 w2 h2 arr1 arr2)))]))


(defn sel-menu [x y w h tabs]
  (let [tabs-count (count tabs)
        [w1 h1 r] (:menu-dims @td/settings-base)
        count-tabs (if (> w h) (/ w tabs-count) (/ h tabs-count))
        hs (* count-tabs (/ h1 w1))
        rs (/ count-tabs r)
        small-tabs (or (> h (* tabs-count w1)) (> w (* tabs-count w1)))
        [dir w1 h1 r ws hs rs] (if (> w h) [:h w1 h1 r count-tabs hs rs]
                                           [:v h1 w1 r hs count-tabs rs])
        [w1 h1 r] (if-not small-tabs
                    [ws hs rs]
                    [w1 h1 r])
        [x1 y1] (if (= dir :h) [x (+ y h) (+ x w) (+ y h)]
                               [(+ x w) y (+ x w) (+ y h)])]
   (doall (for [tab tabs]
           (let [j (:pos tab)]
             (if (= dir :h)
               (sel-menu-tab tab [(+ x1 (* j w1)), y1, w1, h1 r (:h-menu tab)])
               (sel-menu-tab tab [x1, (+ y1 (* j h1)), w1, h1 r (:v-menu tab)])))))))
