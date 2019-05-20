(declare-const x0 String)
(declare-const x1 String)
(declare-const x2 String)
(declare-const i Int)


(assert (= (str.substr x0 2 5) x1))
(assert (str.in.re x0 (str.to.re "abaaaaaa")))
(assert (= 5 (str.len x1)))
(assert (str.in.re x1 (re.* (str.to.re "a"))))


(check-sat)
(get-model)