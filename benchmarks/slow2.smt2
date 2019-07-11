(declare-const x0 String)
(declare-const x1 String)
(declare-const x2 String)
(declare-const x3 String)
(declare-const x4 String)

(assert (= x1 (str.replaceall x0 "<sc>" "x")))
(assert (= x3 (str.replaceall x2 "<sc>" "x")))
(assert (= x4 (str.++ x1 x3)))
(assert (str.in.re x4 (str.to.re "<sc>")))

(check-sat)
(get-model)