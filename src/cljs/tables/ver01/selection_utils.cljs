(ns tables.ver01.selection-utils
  (:use [com.rpl.specter :only [select transform setval FIRST LAST ALL keypath filterer srange comp-paths compiled-select collect-one compiled-setval]])
  (:require [tables.ver01.table_data :as td]
            [tables.ver01.svg :as svg]
            [tables.ver01.analize :as  an]
            [debux.cs.core :refer-macros [clog dbg break]]
            [reagent.core :as r]))


(def selected-current (r/atom {:current-state 0 :ids [] :tables {}}))


(def tab-events
  {:ok     (fn [] (swap! td/tables-state update-in [:tables] (fn [x] (merge x (transform [ALL LAST] #(an/clear-modifications %) (:tables @selected-current))))
                         (swap! selected-current assoc-in [:current-state] 0)))
   :cancel (fn [] (reset! selected-current {:current-state 0 :ids [] :tables {}}))})


(defn anlalize-tables [full-state]
  (let [selected (:selected (:selection full-state))
        tables-state (:tables full-state)
        zero-state (select-keys tables-state selected)
        a-top (an/a-top full-state selected)
        a-down (an/a-down full-state selected)
        a-left (an/a-left full-state selected)
        a-right (an/a-right full-state selected)]
    [zero-state a-top a-down a-left a-right]))


(defn preview-state [current-state full-state]
  (let [selected (:selected (:selection full-state))]
    (swap! selected-current assoc-in [:ids] selected)
    (swap! selected-current assoc-in [:tables] ((anlalize-tables full-state) current-state))))


(defn sel-menu-tabs [full-state]
  (let [
        all-tabs (svg/all-tabs)
        tabs-data [:ok :cancel]
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
        [w1 h1 r] (:menu-dims @td/settings-base)
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
