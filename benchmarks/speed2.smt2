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
(declare-const xa String)
(declare-const xb String)
(declare-const xc String)
(declare-const xd String)
(declare-const xe String)
(declare-const i Int)

(assert (= (str.replaceall x0 "a" "c") x1))
(assert (str.in.re x0 (str.to.re "aabc")))
(assert (= i (str.len x1)))

(assert (= (str.replaceall x1 "b" "d") x2))

(assert (= (str.replaceall x2 "c" "a") x3))

(assert (= (str.replaceall x3 "d" "e") x4))

(assert (= (str.replaceall x4 "e" "f") x5))

(assert (= (str.replaceall x5 "f" "g") x6))

(assert (= (str.replaceall x6 "g" "h") x7))

(assert (= (str.replaceall x7 "h" "i") x8))

(assert (= (str.replaceall x8 "i" "j") x9))

(assert (= (str.replaceall x9 "j" "k") xa))

(assert (= (str.replaceall xa "k" "l") xb))

(assert (= (str.replaceall xb "l" "m") xc))

(assert (= (str.replaceall xc "m" "n") xd))

(assert (= (str.replaceall xd "n" "o") xe))


(check-sat)
(get-model)
