(ns pickaseat.ver01.tables.selection-utils
  (:use [com.rpl.specter :only [select transform setval FIRST LAST ALL keypath filterer srange comp-paths compiled-select collect-one compiled-setval]])
  (:require [pickaseat.ver01.data.table_data :as td]
            [pickaseat.ver01.tables.table-svg :as svg]
            [pickaseat.ver01.tables.tables-analize :as  an]
            [debux.cs.core :refer-macros [clog dbg break]]))


(def tab-events
  {:ok     (fn [] (let [selected-current @an/selected-current
                        del (= (:del selected-current) (:current-state selected-current))]
                    (swap! td/tables-state  (fn [x]
                                                (if del
                                                  (-> x
                                                    (assoc-in  [:selection :selected] [])
                                                    (update-in  [:tables] #(apply dissoc % (:ids selected-current))))
                                                  (update-in x [:tables] (fn [y] (merge y (transform [ALL LAST] #(an/clear-modifications %) (:tables selected-current))))))))
                    (swap! an/selected-current (fn [x]
                                                 (-> x
                                                  (assoc-in [:ids] (if del [] (:ids selected-current)))
                                                  (assoc-in [:tables] {})
                                                  (assoc-in [:current-state] 0)
                                                  (assoc-in [:del] false))))))
   :cancel (fn [] (reset! an/selected-current {:current-state 0 :ids [] :tables {} :start {} :end {} :del false}))})

(defn preview-state [current-state full-state all-states]
  (let [selected (:selected (:selection full-state))
        tables-state (:tables full-state)
        {:keys [next-id x-min  y-min x1-max  y1-max sel-type]} (an/data-preparation tables-state selected)]
    ;(js/console.log "active" (:active (:selection full-state)))
    (when (= sel-type :many)
       (swap! td/tables-state #(-> %
                                (assoc-in [:selection :start] {:x x-min :y y-min})
                                (assoc-in [:selection :end] {:x1 x1-max :y1 y1-max}))))
    (swap! an/selected-current #(-> %
                                    (assoc-in [:ids] (if (not-empty selected) selected [next-id]))
                                    (assoc-in [:del] (if (= sel-type :many) (- (count all-states ) 1) nil))
                                    (assoc-in [:tables] (all-states current-state))))))




(defn sel-menu-tabs [full-state]
  (let [tabs-data [:ok :cancel]
        ft 0
        lt (- (count tabs-data) 1)
        active-tabs (map-indexed #(vector %1 %2) tabs-data)]
    (mapv #(hash-map :pos (first %)
                     :type (second %)
                     :func ((second %) tab-events)
                     :full-state full-state
                     :h-menu (condp = (first %)
                               ft :h-menu-1
                               lt :h-menu-last
                               :h-menu)
                     :v-menu (condp = (first %)
                               ft :v-menu-1
                               lt :v-menu-last
                               :v-menu))
          active-tabs)))

(defn sel-menu-tab [tab [x y w h r dir]]
  (let [{:keys [type func full-state]} tab
        x1 (+ x (* 1 r))
        y1 (+ y (* 1 r))
        w2 (- w (* 2 r))
        h2 (- h (* 2 r))
        arr1 (/ w 4)
        arr2 (/ w 12)
        menu-defaults (merge td/menu-defaults {:on-mouse-down (fn [e] (.stopPropagation e) (.preventDefault e) (func full-state))})]
    ^{:key type} [:g [:path (merge menu-defaults (svg/RoundedRect x, y, w, h, r, dir) {:id type :stroke "black"})]
                  (:icon (type (svg/all-tabs x1 y1 w2 h2 arr1 arr2)))]))


(defn sel-menu [x y w h full-state]
  (let [tabs (sel-menu-tabs full-state)
        tabs-count (count tabs)
        [w1 h1 r] (:menu-dims @td/base-settings)
        per-tab (if (> w h) (/ w tabs-count) (/ h tabs-count))
        hs (* per-tab (/ h1 w1))
        rs (* r (/ h1 w1))
        full-tabs (or (> h (* tabs-count w1)) (> w (* tabs-count w1)))
        [dir w1 h1 r ws hs rs] (if (> w h) [:h w1 h1 r per-tab hs rs]
                                           [:v h1 w1 r hs per-tab rs])
        [w1 h1 r] (if full-tabs
                    [w1 h1 r]
                    [ws hs rs])
        [x1 y1] (if (= dir :h) [x (+ y h) (+ x w) (+ y h)]
                               [(+ x w) y (+ x w) (+ y h)])]
    (doall (for [tab tabs]
             (let [j (:pos tab)]
               (if (= dir :h)
                 (sel-menu-tab tab [(+ x1 (* j w1)), y1, w1, h1 r (:h-menu tab)])
                 (sel-menu-tab tab [x1, (+ y1 (* j h1)), w1, h1 r (:v-menu tab)])))))))
