;fixed

(declare-const x0 String)
(declare-const x1 String)
(declare-const x2 String)
(declare-const x3 String)
(declare-const i Int)

(assert (= (str.replaceall x0 "a" "c") x1))
(assert (str.in.re x0 (str.to.re "aabc")) )
(assert (= i (str.len x1)))

(assert (= (str.replaceall x1 "b" "d") x2))
(assert (= (str.replaceall x2 "c" "a") x3))


(check-sat)
(get-model)