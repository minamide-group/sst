(declare-const x String)
(declare-const w String)

(assert (= x (str.replaceall w "aa" "a")))
(assert (str.in.re x (str.to.re "qwertyuiasdfSDFZXC123456778"))) 


(check-sat)
(get-model)
