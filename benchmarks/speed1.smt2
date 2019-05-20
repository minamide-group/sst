
(declare-fun x () String)
(declare-fun w () String)

(assert (= x (str.replaceall w "abc" "aaa")))
(assert (str.in.re x (str.to.re "aaabdshzxcbnmqwerrt12345ACBDEFH")))

(check-sat)
(get-model)