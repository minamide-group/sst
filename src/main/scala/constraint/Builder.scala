package constraint

import constraint.regular.RegCons
import constraint.relational._
import constraint.vars._
import deterministic.DFA
import deterministic.boundedcopy.{Composition, SST}

case class Builder[Σ](alphabets: Set[Σ],
                      split: Σ) {

  def constraintsToSST(list: List[RelCons], set: Set[RegCons[FAState, Σ]]): SST[SST_State, Σ, Σ, SST_Var] = {
    //(S1 S2) S3
    def _compose(num: Int, sst: SST[SST_State, Σ, Σ, SST_Var], list: List[SST[SST_State, Σ, Σ, SST_Var]]): SST[SST_State, Σ, Σ, SST_Var] = {
      list match {
        case Nil => sst
        case e :: rest => _compose(num + 1, rename(num, Composition.compose(sst, e)), rest)
      }
    }

    val sstList: List[SST[SST_State, Σ, Σ, SST_Var]] =
      if (set.size > 0)
        list.map(cons => constraintToSST(cons)) ::: List(regularToSST(list.last.getLeftIdx() + 1, set.map(cons => cons.x -> cons.R).toMap))
      else
        list.map(cons => constraintToSST(cons))

    if (sstList.size == 1)
      sstList(0)
    else
      _compose(1, rename(0, Composition.compose(sstList(0), sstList(1))), sstList.drop(2))

    //S1(S2 S3)
    //    def _compose(num: Int, sst: SST[SST_State, Σ, Σ, SST_Var], list: List[SST[SST_State, Σ, Σ, SST_Var]]): SST[SST_State, Σ, Σ, SST_Var] = {
    //      list match {
    //        case Nil => sst
    //        case e :: rest => _compose(num + 1, rename(num, Composition.compose(e, sst)), rest)
    //      }
    //    }
    //
    //    val sstList: List[SST[SST_State, Σ, Σ, SST_Var]] =
    //      if (set.size > 0)
    //        list.map(cons => constraintToSST(cons)) ::: List(regularToSST(list.last.getLeftIdx() + 1, set.map(cons => cons.x -> cons.R).toMap))
    //      else
    //        list.map(cons => constraintToSST(cons))
    //
    //    if (sstList.size == 1)
    //      sstList(0)
    //    else
    //      _compose(1, rename(0, Composition.compose(sstList(sstList.size-2), sstList(sstList.size-1))), sstList.dropRight(2).reverse)
  }

  def constraintToSST(cons: RelCons): SST[SST_State, Σ, Σ, SST_Var] = {
    cons match {
      case c: Concatenation => _constraintToSST0(c)
      case t: TransducerConstraint[TransState, Σ] => _constraintToSST1(t)
    }
  }

  private def _constraintToSST0(cons: Concatenation): SST[SST_State, Σ, Σ, SST_Var] = {

    val num = cons.left.id + 1

    val sstName = "sst" + num

    val vars: List[SST_Var] = List.range(0, num - 1).map(x => SST_Var(x, sstName))

    val states: List[SST_State] = List.range(0, num).map(x => SST_State(x, sstName))

    val s0: SST_State = states(0)

    val delta: Map[(SST_State, Σ), SST_State] = List.range(0, num - 1).map(i =>
      alphabets.map(c => (states(i), c) -> states(i))
    ).foldLeft(List.range(0, num - 1).map(i => (states(i), split) -> states(i + 1))) { (x, y) => x ++ y }.toMap

    val eta: Map[(SST_State, Σ), Map[SST_Var, List[Either[SST_Var, Σ]]]] = List.range(0, num - 1).map(i =>
      alphabets.map(c =>
        (states(i), c) ->
          List.range(0, num - 1).map(j => vars(j) -> (if (j == i) List(Left(vars(j)), Right(c)) else List(Left(vars(j))))).toMap
      )
    ).foldLeft(
      List.range(0, num - 1).map(i =>
        (states(i), split) -> vars.map(x => x -> List[Either[SST_Var, Σ]](Left(x))).toMap
      )
    ) { (x, y) => x ++ y }.toMap

    val f: Map[SST_State, List[Either[SST_Var, Σ]]] = Map(
      states(num - 1) -> (
        vars.foldLeft(List[Either[SST_Var, Σ]]()) { (x, y) => x ++ List(Left(y), Right(split)) } ++
          List(Left(vars(cons.right1.id)), Left(vars(cons.right2.id)), Right(split))
        )
    )

    val sink = SST_State(num, sstName)

    SST(states.toSet + sink,
      s0,
      vars.toSet,
      delta.withDefaultValue(sink),
      eta.withDefaultValue(vars.map(x => x -> List[Either[SST_Var, Σ]]()).toMap),
      f)
  }

  private def _constraintToSST1(cons: TransducerConstraint[TransState, Σ]): SST[SST_State, Σ, Σ, SST_Var] = {

    val num = cons.left.id + 1

    val sstName = "sst" + num

    val idx = cons.right2.id

    val tName = "t" + idx

    val trans = cons.right1

    val transToSST: Map[TransState, SST_State] = trans.states.map(s => s -> SST_State(s.id, tName)).toMap

    val vars: List[SST_Var] = List.range(0, num).map(x => SST_Var(x, sstName))

    val states: List[SST_State] = List.range(0, num).map(x => SST_State(x, sstName))

    val s0: SST_State = if (idx == 0) transToSST(trans.initialStates) else states(0)

    val delta0: Map[(SST_State, Σ), SST_State] =
      List.range(0, num - 1).filter(i => i != idx).map(i =>
        alphabets.map(c => (states(i), c) -> states(i))
      ).foldLeft(
        List.range(0, num - 1).filter(i => i != idx).filter(i => i != idx - 1).map(i => (states(i), split) -> states(i + 1))
      ) { (x, y) => x ++ y }.toMap //original

    val delta1: Map[(SST_State, Σ), SST_State] = trans.δ.map(r => (transToSST(r._1._1), r._1._2) -> transToSST(r._2))

    val delta2: Map[(SST_State, Σ), SST_State] = trans.F.map(s => (transToSST(s), split) -> states(idx + 1)).toMap

    val delta3: Map[(SST_State, Σ), SST_State] = if (idx == 0) Map()
    else Map((states(idx - 1), split) -> transToSST(trans.initialStates))

    val delta: Map[(SST_State, Σ), SST_State] = delta0 ++ delta1 ++ delta2 ++ delta3

    val eta0: Map[(SST_State, Σ), Map[SST_Var, List[Either[SST_Var, Σ]]]] =
      List.range(0, num - 1).filter(i => i != idx).map(i =>
        alphabets.map(c =>
          (states(i), c) ->
            List.range(0, num).map(j =>
              vars(j) -> (if (j == i) List(Left(vars(j)), Right(c)) else List(Left(vars(j))))
            ).toMap
        )
      ).foldLeft(
        List.range(0, num - 1).filter(i => i != idx).filter(i => i != idx - 1).map(i =>
          (states(i), split) ->
            vars.map(x => x -> List[Either[SST_Var, Σ]](Left(x))).toMap
        )
      ) { (x, y) => x ++ y }.toMap

    val eta1: Map[(SST_State, Σ), Map[SST_Var, List[Either[SST_Var, Σ]]]] =
      trans.η.map(r =>
        (transToSST(r._1._1), r._1._2) ->
          List.range(0, num).map(j =>
            if (j == idx)
              vars(j) -> List(Left(vars(j)), Right(r._1._2))
            else if (j == num - 1)
              vars(j) -> (List(Left(vars(j))) ::: r._2.map(c => Right(c)))
            else
              vars(j) -> List(Left(vars(j)))
          ).toMap
      )

    val eta2: Map[(SST_State, Σ), Map[SST_Var, List[Either[SST_Var, Σ]]]] =
      trans.F.map(s => (transToSST(s), split) ->
        vars.map(x => x -> List[Either[SST_Var, Σ]](Left(x))).toMap
      ).toMap

    val eta3: Map[(SST_State, Σ), Map[SST_Var, List[Either[SST_Var, Σ]]]] =
      if (idx == 0) Map()
      else Map((states(idx - 1), split) -> vars.map(x => x -> List[Either[SST_Var, Σ]](Left(x))).toMap)

    val eta: Map[(SST_State, Σ), Map[SST_Var, List[Either[SST_Var, Σ]]]] = eta0 ++ eta1 ++ eta2 ++ eta3

    val f: Map[SST_State, List[Either[SST_Var, Σ]]] = Map(states(num - 1) ->
      vars.foldLeft(List[Either[SST_Var, Σ]]()) { (x, y) => x ++ List(Left(y), Right(split)) })

    val sink = SST_State(num, sstName)

    SST(states.filter(s => s.id != idx).toSet ++ transToSST.map(x => x._2).toSet + sink,
      s0,
      vars.toSet,
      delta.withDefaultValue(sink),
      eta.withDefaultValue(vars.map(x => x -> List[Either[SST_Var, Σ]]()).toMap),
      f)
  }

  def regularToSST(num: Int, consMap: Map[StringVariable, DFA[FAState, Σ]]): SST[SST_State, Σ, Σ, SST_Var] = {

    val sstName = "sst" + (num + 1)

    def getTName(id: Int) = "t" + id

    val states: List[SST_State] = List.range(0, num + 1).map(i => SST_State(i, sstName))

    val toSSTStates: Map[FAState, SST_State] = consMap.flatMap(r => {
      r._2.states.map(s => s -> SST_State(s.id, getTName(r._1.id)))
    })

    val statesToInit: Map[SST_State, SST_State] = states.map(s =>
      if (consMap.contains(StringVariable(s.id))) s -> toSSTStates(consMap(StringVariable(s.id)).s0)
      else s -> s
    ).toMap

    val statesToFinal: Map[SST_State, Set[SST_State]] = states.map(s =>
      if (consMap.contains(StringVariable(s.id))) s -> consMap(StringVariable(s.id)).f.map(s => toSSTStates(s))
      else s -> Set(s)
    ).toMap

    val stateSet: Set[SST_State] = states.flatMap(s =>
      if (consMap.contains(StringVariable(s.id))) consMap(StringVariable(s.id)).states.map(q => toSSTStates(q))
      else Set(s)
    ).toSet

    val s0: SST_State = statesToInit(states(0))

    val vars: List[SST_Var] = List.range(0, num).map(i => SST_Var(i, sstName))

    val f: Map[SST_State, List[Either[SST_Var, Σ]]] = Map(
      states(num) -> vars.foldLeft(List[Either[SST_Var, Σ]]()) { (x, y) => x ::: List(Left(y), Right(split)) }
    )

    val delta0: Map[(SST_State, Σ), SST_State] = List.range(0, num).map(i => SST_State(i, sstName)).flatMap(s =>
      if (consMap.contains(StringVariable(s.id)))
        consMap(StringVariable(s.id)).σ.map(r => (toSSTStates(r._1._1), r._1._2) -> toSSTStates(r._2))
      else
        alphabets.map(c => ((s, c) -> s))
    ).toMap

    val delta1 = List.range(0, num).flatMap(i => statesToFinal(states(i)).map(s => (s, split) -> statesToInit(states(i + 1))).toMap)

    val delta: Map[(SST_State, Σ), SST_State] = delta0 ++ delta1

    val eta0: Map[(SST_State, Σ), Map[SST_Var, List[Either[SST_Var, Σ]]]] = List.range(0, num).map(i => states(i)).flatMap(s =>
      if (consMap.contains(StringVariable(s.id)))
        consMap(StringVariable(s.id)).σ.map(r =>
          (toSSTStates(r._1._1), r._1._2) -> vars.map(x => if (x.id == s.id) x -> List(Left(x), Right(r._1._2)) else x -> List(Left(x))).toMap)
      else alphabets.map(c => (s, c) -> vars.map(x => if (x.id == s.id) x -> List(Left(x), Right(c)) else x -> List(Left(x))).toMap)
    ).toMap

    val eta1: Map[(SST_State, Σ), Map[SST_Var, List[Either[SST_Var, Σ]]]] = List.range(0, num).flatMap(i =>
      statesToFinal(states(i)).map(s =>
        (s, split) -> vars.map(x => x -> List[Either[SST_Var, Σ]](Left(x))).toMap
      )).toMap

    val eta: Map[(SST_State, Σ), Map[SST_Var, List[Either[SST_Var, Σ]]]] = eta0 ++ eta1

    val sink = SST_State(num + 1, sstName)

    SST(stateSet + sink,
      s0,
      vars.toSet,
      delta.withDefaultValue(sink),
      eta.withDefaultValue(vars.map(x => x -> List[Either[SST_Var, Σ]]()).toMap),
      f)
  }

  private def rename[Q, X](num: Int, sst: SST[Q, Σ, Σ, X]): SST[SST_State, Σ, Σ, SST_Var] = {

    val sstName = "r" + num

    val toNewState: Map[Q, SST_State] = sst.states.toList.zipWithIndex.map(x => x._1 -> SST_State(x._2, sstName)).toMap

    val toNewVar: Map[X, SST_Var] = sst.vars.toList.zipWithIndex.map(x => x._1 -> SST_Var(x._2, sstName)).toMap

    val states: Set[SST_State] = sst.states.map(s => toNewState(s))

    val s0: SST_State = toNewState(sst.s0)

    val vars: Set[SST_Var] = sst.vars.map(x => toNewVar(x))

    val delta: Map[(SST_State, Σ), SST_State] = sst.δ.map(r => (toNewState(r._1._1), r._1._2) -> toNewState(r._2))

    val eta: Map[(SST_State, Σ), Map[SST_Var, List[Either[SST_Var, Σ]]]] = sst.η.map(r =>
      (toNewState(r._1._1), r._1._2) -> r._2.map(
        t => toNewVar(t._1) -> t._2.map(
          e => if (e.isLeft) Left(toNewVar(e.left.get)) else Right(e.right.get)
        )
      )
    )

    val f: Map[SST_State, List[Either[SST_Var, Σ]]] = sst.f.map(r =>
      toNewState(r._1) -> r._2.map(
        e => if (e.isLeft) Left(toNewVar(e.left.get)) else Right(e.right.get)
      )
    )

    SST(states, s0, vars, delta, eta, f)
  }

}
