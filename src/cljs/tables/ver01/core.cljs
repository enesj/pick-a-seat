(ns tables.ver01.core
  (:use [com.rpl.specter :only [select transform setval FIRST LAST ALL keypath filterer srange comp-paths compiled-select collect-one compiled-setval]])
  (:require
    [reagent.core :as r]
    [goog.events :as events]
    [tables.ver01.components :as c]
    [tables.ver01.table_data :as td]
    [tables.ver01.themes :as t]
    [tables.ver01.util :as u]
    [tables.ver01.selection-utils :as su]
    [tables.ver01.analize :as an]
    [tables.ver01.templates :as tt]
    [devtools.core :as devtools]
    [devtools.toolbox :as toolbox])
  (:import [goog.events EventType]))

(enable-console-print!)
(devtools/install! [:formatters :hints])


(def specter-paths
  {:selection-show   (comp-paths :selection :show)
   :tabale-selected  (comp-paths :tables ALL LAST :selected)
   :selectected-path (comp-paths :selection :selected)
   :selection-active (comp-paths :selection :active)
   :selection-offset (comp-paths :selection :offset)
   :selection-end    (comp-paths :selection :end)
   :selection-start  (comp-paths :selection :start)
   :all              (comp-paths ALL ALL)
   :all-last         (comp-paths ALL LAST)})



(defn reset-seats [id tables]
  (let [rs (:rs (tables id))]
    (swap! td/tables-state assoc-in [:tables id :rs] nil)
    (doseq [idrs (map key rs)]
      (let [ids (filterv #(= % id)
                         (select [(keypath idrs) :rs FIRST FIRST] tables))]
        (doseq [idm ids]
          (swap! td/tables-state update-in [:tables idrs :rs] #(dissoc % idm)))))))


(defn remove-seats [close id]
  (let [cs (map #(first (val %)) (group-by :id (map #(zipmap [:w :h :id :dir] %) (partition 4 close))))]
    (doseq [c cs]
      (let [id1 (:id c)
            d (rest (name (:dir c)))
            d1 (keyword (first d))
            d2 (keyword (second d))]
        (doall
          (swap! td/tables-state update-in [:tables id :rs] #(update-in % [id1] (fn [x] (into #{} (conj x d2)))))
          (swap! td/tables-state update-in [:tables id1 :rs] #(update-in % [id] (fn [x] (into #{} (conj x d1))))))))))

(defn move-table [sel-top-lefts]
  (fn [x-org y-org start tables ctrl]
    (let [tables-collection (into (vals tables) (:borders @td/settings-base))
          {:keys [selected show active offset]} (:selection @td/tables-state)
          {:keys [tabale-selected selectected-path selection-active selection-offset selection-end selection-start selection-show]} specter-paths
          [x y] (mapv - (:svg @td/tables-state))
          x-current (+  x-org (.-pageXOffset js/window) x)
          y-current (+  y-org (.-pageYOffset js/window) y)
          [x-start y-start] start
          sel? (> (count sel-top-lefts) 1)
          result (doall (for [ids sel-top-lefts]
                          (let [id (first ids)
                                [xp yp] (second ids)
                                x-new-corr (- x-org (- x-start xp))
                                y-new-corr (- y-org (- y-start yp))
                                table-my (first (filterv #(= (:id %) id) tables-collection))
                                tables-other (filterv #(not= (:id %) id) tables-collection)
                                {:keys [x y width height rs hide-stools block]} table-my
                                x-new (if (pos? x-new-corr) x-new-corr 0)
                                y-new (if (pos? y-new-corr) y-new-corr 0)
                                table-x (assoc table-my :x x-new :rect-right (+ x-new width))
                                table-y (assoc table-my :y y-new :rect-bottom (+ y-new height))
                                table-xy (assoc table-my :x x-new :y y-new :rect-right (+ x-new width) :rect-bottom (+ y-new height))
                                tables-collision (if sel? (into [] (remove #((set selected) (:id %))) tables-other) tables-other)
                                direction-xy (doall
                                               (for [table tables-collision
                                                     :let [dir (u/collides-with table table-xy)]
                                                     :when dir]
                                                   dir))
                                direction1 (when (and (not sel?) direction-xy)
                                             (doall
                                               (for [table tables-collision
                                                     :let [dir (u/collides-with table table-x table-y)]
                                                     :when dir]
                                                 dir)))
                                direction (when direction1 (if (or (and (some #(= :x %) direction1) (some #(= :y %) direction1)) (some #(= :xy %) direction1)) :xy (first direction1)))
                                x-move (if (= :x direction) x x-new)
                                y-move (if (= :y direction) y y-new)
                                close (if ctrl
                                        (let [close1 (mapv #(u/close-table % table-my) tables-collision)
                                              close1 (filterv boolean (flatten close1))]
                                          (if (and (not-empty close1) (empty (for [table tables-collision
                                                                                   :let [dir (u/collides-with table table-xy)]
                                                                                   :when (not= false dir)]
                                                                               dir)))
                                            close1)))]
                            {:id     id :dir direction :dirxy direction-xy :rs rs :show show :active active :hide-stools hide-stools :x (Math/round x) :y (Math/round y) :x-new (Math/round x-new)
                             :x-move (Math/round x-move) :y-new (Math/round y-new) :y-move (Math/round y-move) :block block :ctrl ctrl :close close :slected-ids selected :sel? sel?
                             :width  (Math/round (:width table-my)) :height (Math/round (:height table-my)) :table-xy table-xy})))
          test-block (seq (flatten (mapv :dirxy result)))
          update-data (atom {})]
      (do

        (doall (for [x result]
                 (let [{:keys [id x-new y-new x-move y-move dir show active hide-stools rs block close slected-ids dirxy sel? width height]} x
                       [x-close y-close] close]
                   (when-not (and sel? test-block)
                     (if (and ctrl (seq close))
                       (do (swap! update-data #(-> %
                                                   (assoc-in [id [:tables id :x]] x-close)
                                                   (assoc-in [id [:tables id :y]] y-close)
                                                   (assoc-in [id [:tables id :rect-bottom]] (+ y-close height))
                                                   (assoc-in [id [:tables id :rect-right]] (+ x-close width))))
                           (remove-seats close id))
                       (do
                         (if (and rs dirxy) (reset-seats id tables))
                         (if (or show active hide-stools)
                           (do
                             (swap! update-data #(-> %
                                                     (assoc-in [id [:selection :show]] false)
                                                     (assoc-in [id [:selection :active]] false)
                                                     (assoc-in [id [:tables id :hide-stools]] true)))))
                         (if (seq dirxy)
                           (do
                             (swap! update-data assoc-in [id [:tables id :block]] [x-new y-new])
                             (if (not= :xy dir)
                               (do
                                 (swap! update-data #(-> %
                                                         (assoc-in [id [:tables id :x]] x-move)
                                                         (assoc-in [id [:tables id :y]] y-move)
                                                         (assoc-in [id [:tables id :rect-right]] (+ x-move width))
                                                         (assoc-in [id [:tables id :rect-bottom]] (+ y-move height))))
                                 (aset js/document "body" "style" "cursor" "move"))
                               (aset js/document "body" "style" "cursor" "not-allowed")))
                           (do (aset js/document "body" "style" "cursor" "move")
                               (swap! update-data #(-> %
                                                       (assoc-in [id [:tables id :block]] nil)
                                                       (assoc-in [id [:tables id :x]] x-new)
                                                       (assoc-in [id [:tables id :y]] y-new)
                                                       (assoc-in [id [:tables id :rect-right]] (+ x-new width))
                                                       (assoc-in [id [:tables id :rect-bottom]] (+ y-new height)))))))))
                   (swap! update-data assoc-in [id [:tables id :block]] [x-new y-new]))))
        (when   (not (and test-block selected))
                (swap! update-data #(-> %
                                        (assoc-in [1 [:selection :start ]] {:x (- (+  x-org (.-pageXOffset js/window) x) (:x offset))
                                                                            :y (- (+  y-org (.-pageXOffset js/window) y) (:y offset))})
                                        (assoc-in [1 [:selection :end ]] {:x1 (- (+  x-org (.-pageXOffset js/window) x) (:x1 offset))
                                                                          :y1 (- (+  y-org (.-pageXOffset js/window) y) (:y1 offset))}))))
        (swap! td/tables-state (fn [x] (doall (reduce #(assoc-in %1 (first %2) (second %2)) x
                                                      (compiled-select (:all specter-paths)
                                                                       (mapv vec (compiled-select (:all-last specter-paths) @update-data)))))))))))


(defn move-tables []
  (fn [sel-top-lefts] (move-table sel-top-lefts)))

(defn root [tables ids]
  [:g
   (doall (for [id ids]
            ^{:key id} [c/table {:on-drag (move-tables)} (r/cursor tables [id])]))])

(defn tables-small []
  "Ne brisi! Moze trebati nekad"
  (let [spoints @td/tables-state
        selection (:selection spoints)
        tables (:tables spoints)]
    [:svg {:viewBox ["-500 -500 15000 15000"]}
     {:fill   (:text t/palete)
      :width  50
      :height 50}
     [root (r/cursor td/tables-state [:tables]) (for [table tables] (first table))]
     (if (:show selection)
       [(c/selection-rect (move-tables) spoints)])]))

(defn tables []
  (let [full-state @td/tables-state
        selection (:selection full-state)
        tables (:tables full-state)
        [x y] (mapv - (:svg full-state))
        [[x-sel-s y-sel-s] [x-sel-e y-sel-e]] (u/start-end (:start selection) (:end selection))
        {:keys [w h]} (:window @td/settings-base)
        {:keys [tabale-selected selectected-path selection-active selection-offset selection-end selection-start selection-show]} specter-paths]
    [:div {:style {:font-size "20px" :margin-top "-20px"}}
     [:div {:style {:padding-left "5%"}} "Velika Sala"]

     [:svg
      {:fill          (:text t/palete)
       :width         w
       :height        h
       :on-key-down   (fn [e]
                        (.preventDefault e)
                        (case (.-which e)
                          7 (swap! td/tables-state assoc-in [:snap] true)
                          nil))
       :on-key-up     (fn [e]
                        (.preventDefault e)
                        (case (.-which e)
                          7 (swap! td/tables-state assoc-in [:snap] false)
                          nil))
       :on-mouse-down (fn [e]
                        (.preventDefault e)
                        (let [x-current (+ (.-clientX e) (.-pageXOffset js/window) x)
                              y-current (+ (.-clientY e) (.-pageYOffset js/window) y)
                              start {:x x-current :y y-current}
                              end {:x1 x-current :y1 y-current}
                              direction (filterv boolean (doall (for [table
                                                                      (conj (into (vals tables) (:borders @td/settings-base))
                                                                            {:id         :1 :x x-sel-s :y y-sel-s :width (- x-sel-e x-sel-s (- 0)) :height (- y-sel-e y-sel-s (- 0))
                                                                             :rect-right (+ x-sel-e 0) :rect-bottom (+ y-sel-e 0)})
                                                                      :let [dir (u/collides-sel table {:id         1 :x x-current :y y-current :width 1 :height 1
                                                                                                       :rect-right (+ x-current 1) :rect-bottom (+ y-current 1)} 0)]
                                                                      :when (not= false dir)]
                                                                  dir)))]
                          (if (empty? direction)
                            (do
                              (reset! an/selected-current {:current-state 0 :ids [] :tables {}})
                              (swap! td/tables-state #(->> %
                                                           (compiled-setval selection-start start)
                                                           (compiled-setval selection-end end)
                                                           (compiled-setval selection-active true)
                                                           (compiled-setval selectected-path nil)
                                                           (compiled-setval tabale-selected false))))

                            (if (some #(= :1 %) direction)
                              (swap! td/tables-state #(->> %
                                                           (compiled-setval selection-offset {:x  (- x-current (:x (:start selection)))
                                                                                              :y  (- y-current (:y (:start selection)))
                                                                                              :x1 (- x-current (:x1 (:end selection)))
                                                                                              :y1 (- y-current (:y1 (:end selection)))})
                                                           (compiled-setval selection-active false)))
                              (swap! td/tables-state
                                     #(compiled-setval tabale-selected false
                                                       (->> %
                                                            (compiled-setval selection-show false)
                                                            (compiled-setval selectected-path nil)
                                                            (compiled-setval selection-end nil)
                                                            (compiled-setval selection-start nil))))))))

       :on-mouse-up   (fn [e]
                        (.preventDefault e)
                        (if (not-empty (:selected selection))
                          (do
                            (swap! td/tables-state assoc-in [:selection :show] true)
                            (swap! td/tables-state assoc-in [:selection :active] false)) ; prestaje formiranje
                          (do
                            (swap! td/tables-state assoc-in [:selection :show] false)
                            (swap! td/tables-state assoc-in [:selection :active] false))))


       :on-mouse-move (fn [e]
                        (.preventDefault e)
                        (let [x-current (+ (.-clientX e) (.-pageXOffset js/window) x)
                              y-current (+ (.-clientY e) (.-pageYOffset js/window) y)
                              start (:start selection)
                              end {:x1 x-current :y1 y-current}
                              offset (:offset selection)
                              [[x y] [x1 y1]] (u/start-end start end)]
                          (when (:active selection)
                            (let [sel (filterv boolean (doall (for [table (vals tables)]
                                                                (u/collides-sel-active table {:id         1 :x x :y y
                                                                                              :width      (- x1 x) :height (- y1 y)
                                                                                              :rect-right x1 :rect-bottom y1} 16))))
                                  select-true (comp-paths :tables ALL LAST #(some (set sel) [(:id %)]) :selected)]
                              (swap! td/tables-state #(->> % (compiled-setval selection-show true)
                                                           (compiled-setval tabale-selected false)
                                                           (compiled-setval select-true true)
                                                           (compiled-setval selection-end end)
                                                           (compiled-setval selectected-path sel)))))

                          (when (and (not (:show selection)) (seq (:selected selection)))
                            (if  (not= @an/selected-current {:current-state 0 :ids [] :tables {}})
                              (reset! an/selected-current {:current-state 0 :ids [] :tables {}})))))}
                            ;(swap! td/tables-state #(->> %
                            ;                             (compiled-setval selection-start {:x (- x-current (:x offset)) :y (- y-current (:y offset))})
                            ;                             (compiled-setval selection-end {:x1 (- x-current (:x1 offset)) :y1 (- y-current (:y1 offset))}))))))}

      [root (r/cursor td/tables-state [:tables]) (for [table tables] (first table))]
      ;(js/console.log (:show selection))
      (if (:show selection)
        [(c/selection-rect (move-tables) full-state)])]]))







(defn resize []
  (fn [evt]
    (td/settings-pos (* (/ (.-innerWidth js/window) 1000) (.-devicePixelRatio js/window)))))


(def tables-mount
  (with-meta tables
             {:component-did-mount
              (fn [this]
                (let [bcr (.getBoundingClientRect (r/dom-node this))
                      x (.-left bcr) y (+ (.-top bcr) 28)]  ;; 28 pxela visina naslova !!!
                  (swap! td/tables-state assoc-in [:svg] [x y])
                  (td/settings-pos (* (/ (.-innerWidth js/window) 1000) (.-devicePixelRatio js/window)))
                  (events/listen js/window EventType.RESIZE (resize))))}))



(defn by-id [id]
  (.getElementById js/document id))

(defn init [template]
  (swap! td/tables-state assoc-in [:tables] (tt/table-templates template)))


;(init :2)


(defn ^:export main []
  (r/render [tables-mount] (by-id "app")))
