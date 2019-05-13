# String Constraint Solver

A string constraint solver supporting concatenation and transduers.

## Getting Started

### Prerequisites

Z3 SMT solver : https://github.com/Z3Prover/z3.


## Running the tests

Explain how to run the automated tests for this system

### Break down into end to end tests

Explain what these tests test and why

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

```
sat
(model
  (define-fun x0 () String
    "aba")
)
```