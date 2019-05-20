(declare-const x0 String)
(declare-const x1 String)
(declare-const x2 String)
(declare-const i Int)

(assert (= 0 (str.len x2)))


(check-sat)
(get-model)