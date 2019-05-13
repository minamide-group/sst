# String Constraint Solver

A string constraint solver supporting concatenation and transduers.

## Getting Started

### Prerequisites

Z3 SMT solver : https://github.com/Z3Prover/z3.


## Running the tests

Suppose we have a file substr.smt2 with following content:
```
(declare-const x0 String)
(declare-const x1 String)
(declare-const x2 String)
(declare-const i Int)

(assert (= (str.substr x0 3) x1))
(assert (str.in.re x0 (str.to.re "aba")))
(assert (= 0 (str.len x1)))

(check-sat)
(get-model)
```

Execute:
```
java -jar checker.jar substr.smt2
```

then the result is:
```
sat
(model
  (define-fun x0 () String
    "aba")
  (define-fun x1 () String
    "")
)

```