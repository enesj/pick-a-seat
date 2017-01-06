  (ns tables.ver01.core
    (:use [com.rpl.specter :only [select transform setval FIRST LAST ALL keypath filterer srange comp-paths compiled-select collect-one compiled-setval]])
    (:require
      [reagent.core :as r]
      [goog.events :as events]
      [tables.ver01.components :as c]
      [tables.ver01.table_data :as td]
      [tables.ver01.themes :as t]
      [tables.ver01.util :as u])
    (:import [goog.events EventType]))

  (enable-console-print!)

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
          (let [tables-collection (into (u/table-props tables) (:borders @td/settings-base))
                {:keys [selected show active]} (:selection @td/tables-state)
                [xs ys] start
                sel? (> (count sel-top-lefts) 1)
                result (doall (for [ids sel-top-lefts]
                                (let [id (first ids)
                                      [xp yp] (second ids)
                                      x-new (- x-org (- xs xp))
                                      y-new (- y-org (- ys yp))
                                      table-my (first (filterv #(= (:id %) id) tables-collection))
                                      {:keys [x y w h rs hide-stools block]} table-my
                                      x-new (if (> x-new 0) x-new x)
                                      y-new (if (> y-new 0) y-new y)
                                      table-x (assoc table-my :x x-new :rect-right (+ x-new (:width table-my)))
                                      table-y (assoc table-my :y y-new :rect-bottom (+ y-new (:height table-my)))
                                      table-xy (merge table-my {:y y-new :x x-new :rect-right (+ x-new (:width table-my)) :rect-bottom (+ y-new (:height table-my))})
                                      tables-other (filterv #(not= (:id %) id) tables-collection)
                                      tables-collision (if sel? (into [] (remove #((set selected) (:id %)))  tables-other) tables-other)
                                      direction-xy  (doall
                                                      (for [table tables-collision
                                                            :let [dir (u/collides-with table table-xy)]
                                                            :when (not= false dir)]
                                                        dir))
                                      direction1 (if (and (not sel?) direction-xy)
                                                   (doall
                                                     (for [table tables-collision
                                                           :let [dir (u/collides-with table table-x table-y)]
                                                           :when (not= false dir)]
                                                       dir)))
                                      direction (if direction1 (if (or (and (some #(= :x %) direction1) (some #(= :y %) direction1)) (some #(= :xy %) direction1)) :xy (first direction1)))
                                      x-move (if (= :x direction) x x-new)
                                      y-move (if (= :y direction) y y-new)
                                      close (if ctrl
                                              (let [close1 (mapv #(u/close-table % table-my) tables-collision)
                                                    close1 (filterv boolean (flatten close1))]
                                                (if (and (not-empty close1) (empty  (for [table tables-collision
                                                                                          :let [dir (u/collides-with table table-xy)]
                                                                                          :when (not= false dir)]
                                                                                      dir)))
                                                  close1)))]
                                  {:id id :dir direction :dirxy direction-xy :rs rs :show show :active active :hide-stools hide-stools :x x :y y :x-new x-new
                                   :x-move x-move :y-new y-new :y-move y-move :block block :ctrl ctrl :close close :slected-ids selected :sel? sel?})))]
            (let [test-block (seq (flatten (mapv :dirxy result)))
                  update-data (atom {})]
              (do
                (doall (for [x result]
                         (let [{:keys [id x-new y-new x-move y-move dir show active hide-stools rs block close slected-ids dirxy sel?]} x]

                           (if (and test-block selected)
                             (do
                               (swap! update-data assoc-in [id [:selection :show]] true)))
                           (when-not (and sel? test-block)
                             (if (and ctrl (seq close))
                               (do (swap! update-data assoc-in [id [:tables id :pos] ]  (take 2 close))
                                   (remove-seats close id))
                               (do
                                 (if (and rs dirxy) (reset-seats id tables))
                                 (if (or show active hide-stools)
                                   (do
                                     (swap! update-data #(-> %
                                                             (assoc-in [id [:selection :show]] false)
                                                             (assoc-in [id [:selection :active]] false)
                                                             (assoc-in [id [:tables id :hide-stools] ] true)))))
                                 (if (seq dirxy)
                                   (do
                                     (swap! update-data assoc-in [id [:tables id :block ]] [x-new y-new])
                                     (if (not= :xy dir)
                                       (do
                                         (swap! update-data assoc-in [id [ :tables id :pos ]] [x-move y-move])
                                         (aset js/document "body" "style" "cursor" "move"))
                                       (aset js/document "body" "style" "cursor" "not-allowed")))
                                   (do (aset js/document "body" "style" "cursor" "move")
                                       (swap! update-data  #(-> %
                                                                (assoc-in [id [ :tables id :block ]] nil)
                                                                (assoc-in [id [ :tables id :pos ]] [x-new y-new]))))))))
                           (swap! update-data assoc-in [id [ :tables id :block ]] [x-new y-new]))))
                (swap! td/tables-state  (fn [x] (doall (reduce #(assoc-in %1 (first %2) (second %2)) x
                                                               (compiled-select (:all specter-paths)
                                                                                (mapv vec (compiled-select (:all-last specter-paths) @update-data))))))))))))

  (defn move-tables []
        (fn [sel-top-lefts] (move-table sel-top-lefts)))

  (defn root [tables ids]
        [:g
         (doall(for [id ids]
                 ^{:key id} [c/table {:on-drag (move-tables)} (r/cursor tables [id])]))])



  (defn tables []
        (let [spoints @td/tables-state
              selection (:selection spoints)
              tables (:tables spoints)
              [x y] (:svg spoints)
              {xsel :x ysel :y} (:start selection)
              {x1sel :x1 y1sel :y1} (:end selection)
              w (/ (.-innerWidth js/window) 1.6)
              h (/ w 1.3)
              {:keys [tabale-selected selectected-path selection-active selection-offset selection-end selection-start selection-show]} specter-paths]
          [:svg
           {:fill          (:text t/palete)
            :width         w
            :height        h
            :on-key-down   #(case (.-which %)
                              7 (swap! td/tables-state assoc-in [:snap] true)
                              nil)
            :on-key-up     #(case (.-which %)
                              7 (swap! td/tables-state assoc-in [:snap] false)
                              nil)
            :on-mouse-down (fn [e]
                             (let [x-current (- (.-clientX e) x)
                                   y-current (- (.-clientY e) y)
                                   start {:x x-current :y y-current}
                                   end {:x1 x-current :y1 y-current}
                                   direction (filterv boolean (doall (for [table
                                                                           (conj (into (u/table-props tables) (:borders @td/settings-base))
                                                                                 {:id :1 :x xsel :y ysel :width (- x1sel xsel (- 20)) :height (- y1sel ysel (- 20))
                                                                                  :rect-right (+ x1sel 20) :rect-bottom (+ y1sel 20)})
                                                                           :let [dir (u/collides-sel table {:id 1 :x x-current :y y-current :width 1 :height 1
                                                                                                            :rect-right (+ x-current 1) :rect-bottom (+ y-current 1)} 0)]
                                                                           :when (not= false dir)]
                                                                       dir)))]
                               (if (empty?  direction)
                                 (swap! td/tables-state #(->> %

                                                              (compiled-setval selection-start start)
                                                              (compiled-setval selection-end end)
                                                              (compiled-setval selection-active true)
                                                              (compiled-setval selectected-path nil)
                                                              (compiled-setval tabale-selected false)))
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
                             (do
                               (swap! td/tables-state assoc-in [:selection :active] false)
                               (if (and (= :1 (u/collides-sel {:id :1 :x (- (.-clientX e) x) :y (- (.-clientY e) y) :width 1 :height 1 :rect-right (- (.-clientX e) x (- 1)) :rect-bottom (- (.-clientY e) y (- 1))}
                                                              {:id :2 :x xsel :y ysel :width (- x1sel xsel) :height (- y1sel ysel) :rect-right x1sel :rect-bottom y1sel} 0))
                                        (:selected selection))
                                 (swap! td/tables-state #(compiled-setval selection-show true %)))))
            :on-mouse-move (fn [e]
                             (let [x-current (- (.-clientX e) x)
                                   y-current (- (.-clientY e) y)
                                   start (:start selection)
                                   offset (:offset selection)
                                   end {:x1 x-current :y1 y-current}
                                   x (:x start)
                                   y (:y start)
                                   x1 (:x1 end)
                                   y1 (:y1 end)]

                               (when (:active selection)
                                 ( let [sel (filterv boolean (doall (for [table (u/table-props tables)]
                                                                      (u/collides-sel-active table {:id 1 :x x :y y
                                                                                                    :width  (- x1 x) :height (- y1 y)
                                                                                                    :rect-right x1 :rect-bottom y1  } 0))))
                                        select-true (comp-paths :tables ALL LAST #(some (set sel) [(:id %)]) :selected)]
                                   (if-not (:show selection)
                                     (swap! td/tables-state assoc-in [:selection :show] true))
                                   (swap! td/tables-state #(->> % (compiled-setval selection-show true)
                                                                (compiled-setval tabale-selected false)
                                                                (compiled-setval select-true true)
                                                                (compiled-setval selection-end end)
                                                                (compiled-setval selectected-path sel)))))

                               (if (and (not (:show selection)) (seq (:selected selection)))
                                 (swap! td/tables-state #(->> %
                                                              (compiled-setval selection-start {:x (- x-current (:x offset)) :y (- y-current (:y offset))})
                                                              (compiled-setval selection-end {:x1 (- x-current (:x1 offset)) :y1 (- y-current (:y1 offset))}))))))}

           [:rect {:x 0 :y 0 :width "100%" :height "100%" :fill (:background t/palete) :stroke (:table-stroke t/palete)}]
           [:text {:style {:-webkit-user-select "none" :-moz-user-select "none"} :x 20 :y 25 :font-size 20} "Velika sala "]
           [root (r/cursor td/tables-state [:tables]) (for [table tables] (first table))]
           (if (:show selection)
             [(c/selection-rect (move-tables) spoints)])]))

  (defn resize []
        (fn [evt]
          (td/settings-pos (/ (.-innerWidth js/window) 1000))))


  (def tables1
       (with-meta tables
                  {:component-did-mount
                   (fn [this]
                     (let [bcr (.getBoundingClientRect (r/dom-node this))
                           x (.-left bcr) y (.-top bcr)]
                       (swap! td/tables-state assoc-in [:svg] [x y])
                       (events/listen js/window EventType.RESIZE (resize))))}))


  (defn by-id [id]
        (.getElementById js/document id))

  (defn ^:export main []
        (r/render [tables1] (by-id "app")))
