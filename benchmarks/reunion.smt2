(declare-const x0 String)
(declare-const x1 String)
(declare-const x2 String)
(declare-const x3 String)
(declare-const x4 String)
(declare-const x5 String)


(assert (str.in.re x0 (re.union (str.to.re "a") )))
(assert (str.in.re x1 (re.union (str.to.re "b") (str.to.re "a") )))
(assert (str.in.re x2 (re.union (str.to.re "c") (str.to.re "b") (str.to.re "a") )))
(assert (str.in.re x3 (re.union (str.to.re "d") (str.to.re "c") (str.to.re "b") (str.to.re "a") )))


(assert (str.in.re x4 (re.union (str.to.re "a") (str.to.re "a") (str.to.re "a") (str.to.re "a"))))


(check-sat)
(get-model)
