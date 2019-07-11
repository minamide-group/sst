(declare-const x0 String)
(declare-const x1 String)
(declare-const x2 String)

(assert (= x1 (str.reverse x0)) )

(assert (= x2 (str.replaceall x1 "ab" "c")) )


(assert (>= (str.len x1) (+ 5 (str.len x2) )  ))


(check-sat)
(get-model)