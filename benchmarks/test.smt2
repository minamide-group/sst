(declare-const x0 String)
(declare-const x1 String)
(declare-const y0 String)
(declare-const y1 String)
(declare-const y2 String)
(declare-const i Int)



(assert (= y0 (str.replaceall x0 "<script>" "x")))
(assert (= y1 (str.replaceall x1 "<script>" "x")))

(assert (< 0 (str.len x0) ))

(assert (< 0 (str.len x1) ))

(assert (= (str.len y0) (str.len y1)))


(check-sat)


;c   12.59s  // time out for a witness
;s   2.082s  // no witness
;o   // not handle