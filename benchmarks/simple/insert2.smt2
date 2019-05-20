(declare-const x0 String)
(declare-const x1 String)
(declare-const x2 String)
(declare-const i Int)

(assert (= x1  (str.insert x0 3 "aba" )))
(assert (str.in.re x0 (str.to.re "")))

(check-sat)