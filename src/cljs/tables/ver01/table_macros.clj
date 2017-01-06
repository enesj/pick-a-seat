(ns tables.ver01.table-macros)


;[:rect (assoc stool-h :dir :l :id 1)]



;(defmacro stool-data [stool dir id t1 t2]
;  `( assoc ~stool :dir ~dir :id ~id :transform (str " translate(" ~t1 ", " ~t2 ")"))
;  )

;(macroexpand (stool-data {} 1 2 3 4 ))


;(stool-data {} 1 2 3 4 )



(defmacro stool-data [stool dir id t1 t2]
  `(conj [:rect] ( assoc ~stool :dir ~dir :id ~id :transform (str " translate(" ~t1 ", " ~t2 ")"))))






;(stool-data1 {} 1 2 3 4 )
