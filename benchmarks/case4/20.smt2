(declare-const x0 String)
(declare-const x1 String)
(declare-const x2 String)
(declare-const x3 String)
(declare-const x4 String)
(declare-const x5 String)
(declare-const x6 String)
(declare-const x7 String)
(declare-const x8 String)
(declare-const x9 String)
(declare-const x10 String)
(declare-const x11 String)
(declare-const x12 String)
(declare-const x13 String)
(declare-const x14 String)
(declare-const x15 String)
(declare-const x16 String)
(declare-const x17 String)
(declare-const x18 String)
(declare-const i Int)

(assert (= (str.replaceall x0 "a" "c") x1))
(assert (str.in.re x0 (str.to.re "aabc")))
(assert (= i (str.len x1)))
(assert (= (str.replaceall x1 "b" "d") x2))
(assert (= (str.replaceall x2 "c" "a") x3))
(assert (= (str.replaceall x3 "d" "e") x4))
(assert (= (str.replaceall x4 "e" "f") x5))
(assert (= (str.replaceall x5 "f" "g") x6))
(assert (= x7 (str.++ x5 "f" "g")))
(assert (= x8 (str.++ x7 x6)))
(assert (= x9  (str.replaceall x8 "g" "h")))
(assert (= x10  (str.replaceall x9 "h" "i")))
(assert (= x11  (str.replaceall x10 "i" "j")))
(assert (= x12 (str.++ x10 x11)))
(assert (= x13 (str.++ x10 x11 x12)))
(assert (= x14  (str.replaceall x13 "j" "k")))
(assert (= x15  (str.replaceall x14 "k" "l")))
(assert (= x16  (str.replaceall x15 "l" "m")))
(assert (= x17 (str.++ x16 x15)))
(assert (= x18 (str.++ x16 x17 x15)))



(check-sat)
(get-model)