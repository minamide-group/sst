package deterministic.copyless

case class SST[Q, Σ, Γ, X](
                            states: Set[Q],
                            s0: Q,
                            vars: Set[X],
                            δ: Map[(Q, Σ), Q],

                            //foreach transition, assert each variable appears at most once on the rhs of all the assignments
                            η: Map[(Q, Σ), Map[X, List[Either[X, Γ]]]],
                            f: Map[Q, List[Either[X, Γ]]]
                          ) {

  def toBoundedCopy: deterministic.boundedcopy.SST[Q, Σ, Γ, X] = deterministic.boundedcopy.SST(states, s0, vars, δ, η, f)

}

