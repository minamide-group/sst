;fixed

(declare-const x0 String)
(declare-const x1 String)
(declare-const i Int)

(assert (str.in.re x0 (str.to.re "ba")))
(assert (= x1 "adsc"))
(assert (= i (str.len x1)))
;(assert (str.in.re x1 (str.to.re "ac")))

(check-sat)
(get-model)