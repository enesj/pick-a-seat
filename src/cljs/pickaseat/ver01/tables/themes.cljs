(ns pickaseat.ver01.tables.themes)
(def paletes

  {:0 {:table-fill "rgb(250, 250, 250)"
       :stool-fill "rgb(150, 150, 150)"
       :stool-small-fill "rgb(90, 90, 90)"
       :table-stroke "rgb(11, 0, 20)"
       :stool-stroke "rgb(11, 0, 20)"
       :stool-small-stroke "rgb(212, 77, 92)"
       :background "rgb(208, 230, 255)"
       :text "rgb(50, 51, 68)"}
   :1 {:table-fill "rgb(119, 51, 68)"
       :stool-fill "rgb(227, 181, 164)"
       :stool-small-fill "rgb(245, 233, 226)"
       :table-stroke "rgb(11, 0, 20)"
       :stool-stroke "rgb(11, 0, 20)"
       :stool-small-stroke "rgb(212, 77, 92)"
       :background "rgb(208, 230, 255)"
       :text "rgb(119, 51, 68)"}
   :2 {:table-fill "rgb(254, 147, 140)"
       :stool-fill "rgb(230, 184, 156)"
       :stool-small-fill "rgb(234, 210, 172)"
       :table-stroke "rgb(156, 175, 183)"
       :stool-stroke "rgb(156, 175, 183)"
       :stool-small-stroke "rgb(66, 129, 164)"
       :background "rgb(43, 74, 154)"
       :text "rgb(254, 147, 140)"}
   :3 {:table-fill "rgb(36, 123, 160)"
       :stool-fill "rgb(112, 193, 179)"
       :stool-small-fill "rgb(178, 219, 191)"
       :table-stroke "rgb(243, 255, 189)"
       :stool-stroke "rgb(243, 255, 189)"
       :stool-small-stroke "rgb(255, 22, 84)"
       :background "rgb(238, 245, 219)"
       :text "rgb(36, 123, 160)"}
   :4 {:table-fill "rgb(255, 191, 0)"
       :stool-fill "rgb(232, 63, 111)"
       :stool-small-fill "rgb(34, 116, 165)"
       :table-stroke "rgb(50, 147, 111)"
       :stool-stroke "rgb(50, 147, 111)"
       :stool-small-stroke "rgb(255, 255, 255)"
       :background "rgb(200, 200, 200)"
       :text "rgb(255, 191, 0)"}
   :5 {:table-fill "rgb(93, 115, 126)"
       :stool-fill "rgb(100, 182, 172)"
       :stool-small-fill "rgb(192, 253, 251)"
       :table-stroke "rgb(218, 255, 239)"
       :stool-stroke "rgb(218, 255, 239)"
       :stool-small-stroke "rgb(252, 255, 253)"
       :background "rgb(252, 231, 150)"
       :text "rgb(93, 115, 126)"}
   :6 {:table-fill "rgb(246, 81, 29)"
       :stool-fill "rgb(255, 180, 0)"
       :stool-small-fill "rgb(0, 166, 237)"
       :table-stroke "rgb(127, 184, 0)"
       :stool-stroke "rgb(127, 184, 0)"
       :stool-small-stroke "rgb(13, 44, 84)"
       :background "rgb(163, 206, 199)"
       :text "rgb(246, 81, 29)"}})


(def current-palete :0)

(def palete  (current-palete paletes))
