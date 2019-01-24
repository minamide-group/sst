package constraint

import constraint.regular.RegCons
import constraint.relational._
import constraint.vars._
import deterministic.DFA
import deterministic.boundedcopy.{Composition, SST}

case class SSTBuilder[Σ](charSet: Set[Σ],
                         //assert split is not in alphabets
                         split: Σ) {

  def constraintsToSST(list: List[RelCons], set: Set[RegCons[Σ]]): SST[SST_State, Σ, Int, SST_Var] = {

    def _compose(sst: SST[SST_State, Σ, Σ, SST_Var], list: List[SST[SST_State, Σ, Σ, SST_Var]]): SST[SST_State, Σ, Σ, SST_Var] = {
      list match {
        case Nil => sst
        case e :: rest => _compose(compose0(sst, e), rest)
      }
    }

    def compose0[Y](sst1: SST[SST_State, Σ, Σ, SST_Var], sst2: SST[SST_State, Σ, Y, SST_Var]) = Composition.compose(sst1, sst2).rename("r0").trim

    val sstList: List[SST[SST_State, Σ, Σ, SST_Var]] =
      list.map(cons => relationalToSST(cons))

    val last = regularToSST_Int(list.last.getLeftIdx() + 1, set.map(cons => cons.x -> cons.R).toMap)

    val sst0 = if (sstList.size == 1)
      sstList(0)
    else
      _compose(compose0(sstList(0), sstList(1)), sstList.drop(2))

    compose0(sst0, last)
  }

  def relationalToSST(cons: RelCons): SST[SST_State, Σ, Σ, SST_Var] = {

    def _constraintToSST0(cons: Concatenation[Σ]): SST[SST_State, Σ, Σ, SST_Var] = {

      val num = cons.left.id + 1

      val sstName = "sst" + num

      val vars: List[SST_Var] = List.range(0, num - 1).map(x => SST_Var(x, sstName))

      val states: List[SST_State] = List.range(0, num).map(x => SST_State(x, sstName))

      val s0: SST_State = states(0)

      val delta: Map[(SST_State, Σ), SST_State] = List.range(0, num - 1).map(i =>
        charSet.map(c => (states(i), c) -> states(i))
      ).foldLeft(List.range(0, num - 1).map(i => (states(i), split) -> states(i + 1))) { (x, y) => x ++ y }.toMap

      val eta: Map[(SST_State, Σ), Map[SST_Var, List[Either[SST_Var, Σ]]]] = List.range(0, num - 1).map(i =>
        charSet.map(c =>
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
            cons.list.flatMap(x => {
              x match {
                case Left(v) => List(Left(vars(v.id)))
                case Right(str) => str.map(c => Right(c))
              }
            }) ++ List(Right(split))

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

    def _constraintToSST1(cons: TransducerConstraint[Σ]): SST[SST_State, Σ, Σ, SST_Var] = {

      val num = cons.left.id + 1

      val sstName = "sst" + num

      val idx = cons.right2.id

      val tName = "t" + idx

      val trans = cons.right1

      val toNewState: Map[TransState, SST_State] = trans.states.map(s => s -> SST_State(s.id, tName)).toMap

      val vars: List[SST_Var] = List.range(0, num).map(x => SST_Var(x, sstName))

      val states: List[SST_State] = List.range(0, num).map(x => SST_State(x, sstName))

      val s0: SST_State = if (idx == 0) toNewState(trans.s0) else states(0)

      val delta0: Map[(SST_State, Σ), SST_State] =
        List.range(0, num - 1).filter(i => i != idx).map(i =>
          charSet.map(c => (states(i), c) -> states(i))
        ).foldLeft(
          List.range(0, num - 1).filter(i => i != idx).filter(i => i != idx - 1).map(i => (states(i), split) -> states(i + 1))
        ) { (x, y) => x ++ y }.toMap //original

      val delta1: Map[(SST_State, Σ), SST_State] = trans.δ.map(r => (toNewState(r._1._1), r._1._2) -> toNewState(r._2))

      val delta2: Map[(SST_State, Σ), SST_State] = trans.f.map(s => (toNewState(s), split) -> states(idx + 1)).toMap

      val delta3: Map[(SST_State, Σ), SST_State] = if (idx == 0) Map()
      else Map((states(idx - 1), split) -> toNewState(trans.s0))

      val delta: Map[(SST_State, Σ), SST_State] = delta0 ++ delta1 ++ delta2 ++ delta3

      val eta0: Map[(SST_State, Σ), Map[SST_Var, List[Either[SST_Var, Σ]]]] =
        List.range(0, num - 1).filter(i => i != idx).map(i =>
          charSet.map(c =>
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
          (toNewState(r._1._1), r._1._2) ->
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
        trans.f.map(s => (toNewState(s), split) ->
          vars.map(x => x -> List[Either[SST_Var, Σ]](Left(x))).toMap
        ).toMap

      val eta3: Map[(SST_State, Σ), Map[SST_Var, List[Either[SST_Var, Σ]]]] =
        if (idx == 0) Map()
        else Map((states(idx - 1), split) -> vars.map(x => x -> List[Either[SST_Var, Σ]](Left(x))).toMap)

      val eta: Map[(SST_State, Σ), Map[SST_Var, List[Either[SST_Var, Σ]]]] = eta0 ++ eta1 ++ eta2 ++ eta3

      val f: Map[SST_State, List[Either[SST_Var, Σ]]] = Map(states(num - 1) ->
        vars.foldLeft(List[Either[SST_Var, Σ]]()) { (x, y) => x ++ List(Left(y), Right(split)) })

      val sink = SST_State(num, sstName)

      SST(states.toSet - states(idx) ++ toNewState.map(x => x._2).toSet + sink,
        s0,
        vars.toSet,
        delta.withDefaultValue(sink),
        eta.withDefaultValue(vars.map(x => x -> List[Either[SST_Var, Σ]]()).toMap),
        f)
    }

    def _constraintToSST2(cons: SSTConstraint[Σ]): SST[SST_State, Σ, Σ, SST_Var] = {

      val num = cons.left.id + 1

      val sstName = "sst" + num

      val idx = cons.right2.id

      val consSST = cons.right1.rename("t" + idx)

      val states: List[SST_State] = List.range(0, num).map(x => SST_State(x, sstName))

      val vars: List[SST_Var] = List.range(0, num).map(x => SST_Var(x, sstName))

      val sink = SST_State(num, sstName)

      val stateSet = states.toSet - states(idx) ++ consSST.states + sink

      val varSet = vars.toSet ++ consSST.vars

      val s0: SST_State = if (idx == 0) consSST.s0 else states(0)

      val delta0: Map[(SST_State, Σ), SST_State] =
        List.range(0, num - 1).filter(i => i != idx).map(i =>
          charSet.map(c => (states(i), c) -> states(i))
        ).foldLeft(
          List.range(0, num - 1).filter(i => i != idx).filter(i => i != idx - 1).map(i => (states(i), split) -> states(i + 1))
        ) { (x, y) => x ++ y }.toMap //original

      val delta1: Map[(SST_State, Σ), SST_State] = consSST.δ

      val delta2: Map[(SST_State, Σ), SST_State] = consSST.f.keySet.map(s => (s, split) -> states(idx + 1)).toMap

      val delta3: Map[(SST_State, Σ), SST_State] = if (idx == 0) Map() else Map((states(idx - 1), split) -> consSST.s0)

      val delta: Map[(SST_State, Σ), SST_State] = delta0 ++ delta1 ++ delta2 ++ delta3

      val eta0: Map[(SST_State, Σ), Map[SST_Var, List[Either[SST_Var, Σ]]]] =
        List.range(0, num - 1).filter(i => i != idx).map(i =>
          charSet.map(c =>
            (states(i), c) -> (
              List.range(0, num).map(j =>
                vars(j) -> (if (j == i) List(Left(vars(j)), Right(c)) else List(Left(vars(j))))
              ).toMap ++ consSST.vars.map(x => x -> List()).toMap
              )
          )
        ).foldLeft(
          List.range(0, num - 1).filter(i => i != idx).filter(i => i != idx - 1).map(i =>
            (states(i), split) ->
              (vars.map(x => x -> List[Either[SST_Var, Σ]](Left(x))).toMap ++ consSST.vars.map(x => x -> List()).toMap)
          )
        ) { (x, y) => x ++ y }.toMap

      val eta1: Map[(SST_State, Σ), Map[SST_Var, List[Either[SST_Var, Σ]]]] =
        consSST.η.map(r =>
          r._1 -> (
            List.range(0, num).map(j =>
              if (j == idx)
                vars(j) -> List(Left(vars(j)), Right(r._1._2))
              else
                vars(j) -> List(Left(vars(j)))
            ).toMap ++ r._2
            )
        )

      val eta2: Map[(SST_State, Σ), Map[SST_Var, List[Either[SST_Var, Σ]]]] =
        consSST.f.map(t =>
          (t._1, split) -> (
            vars.map(x => x -> List[Either[SST_Var, Σ]](Left(x))).toMap + (vars(num - 1) -> t._2) ++ consSST.vars.map(x => x -> List()).toMap
            )
        )

      val eta3: Map[(SST_State, Σ), Map[SST_Var, List[Either[SST_Var, Σ]]]] =
        if (idx == 0) Map()
        else Map(
          (states(idx - 1), split) -> (
            vars.map(x => x -> List[Either[SST_Var, Σ]](Left(x))).toMap ++ consSST.vars.map(x => x -> List()).toMap
            )
        )

      val eta: Map[(SST_State, Σ), Map[SST_Var, List[Either[SST_Var, Σ]]]] = eta0 ++ eta1 ++ eta2 ++ eta3

      val f: Map[SST_State, List[Either[SST_Var, Σ]]] = Map(states(num - 1) ->
        vars.foldLeft(List[Either[SST_Var, Σ]]()) { (x, y) => x ++ List(Left(y), Right(split)) })

      SST(stateSet,
        s0,
        varSet,
        delta.withDefaultValue(sink),
        eta.withDefaultValue(varSet.map(x => x -> List[Either[SST_Var, Σ]]()).toMap),
        f)
    }

    cons match {
      case c: Concatenation[Σ] => _constraintToSST0(c)
      case t: TransducerConstraint[Σ] => _constraintToSST1(t)
      case s: SSTConstraint[Σ] => _constraintToSST2(s)
    }
  }

  def regularToSST_Int(num: Int, consMap: Map[StringVariable, DFA[FAState, Σ]]): SST[SST_State, Σ, Int, SST_Var] = {

    val sstName = "sst" + (num + 1)

    val split_out = -1

    def getTName(id: Int) = "t" + id

    val states: List[SST_State] = List.range(0, num + 1).map(i => SST_State(i, sstName))

    def toSSTStates(q: FAState, idx: Int) = SST_State(q.id, getTName(idx))

    val statesToInit: Map[SST_State, SST_State] = states.map(s =>
      if (consMap.contains(StringVariable(s.id))) s -> toSSTStates(consMap(StringVariable(s.id)).s0, s.id)
      else s -> s
    ).toMap

    val statesToFinal: Map[SST_State, Set[SST_State]] = states.map(s =>
      if (consMap.contains(StringVariable(s.id))) s -> consMap(StringVariable(s.id)).f.map(q => toSSTStates(q, s.id))
      else s -> Set(s)
    ).toMap

    val stateSet: Set[SST_State] = states.flatMap(s =>
      if (consMap.contains(StringVariable(s.id))) consMap(StringVariable(s.id)).states.map(q => toSSTStates(q, s.id))
      else Set(s)
    ).toSet

    val s0: SST_State = statesToInit(states(0))

    val vars: List[SST_Var] = List.range(0, num).map(i => SST_Var(i, sstName))

    val f: Map[SST_State, List[Either[SST_Var, Int]]] = Map(
      states(num) -> vars.foldLeft(List[Either[SST_Var, Int]]()) { (x, y) => x ::: List(Left(y), Right(split_out)) }
    )

    val delta0: Map[(SST_State, Σ), SST_State] = List.range(0, num).flatMap(i =>
      if (consMap.contains(StringVariable(i)))
        consMap(StringVariable(i)).δ.map(r => (toSSTStates(r._1._1, i), r._1._2) -> toSSTStates(r._2, i))
      else
        charSet.map(c => ((states(i), c) -> states(i)))
    ).toMap

    val delta1 = List.range(0, num).flatMap(i => statesToFinal(states(i)).map(s => (s, split) -> statesToInit(states(i + 1))).toMap)

    val delta: Map[(SST_State, Σ), SST_State] = delta0 ++ delta1

    val eta0: Map[(SST_State, Σ), Map[SST_Var, List[Either[SST_Var, Int]]]] = List.range(0, num).flatMap(i =>
      if (consMap.contains(StringVariable(i)))
        consMap(StringVariable(i)).δ.map(r =>
          (toSSTStates(r._1._1, i), r._1._2) -> vars.map(x => if (x.id == i) x -> List(Left(x), Right(i)) else x -> List(Left(x))).toMap)
      else
        charSet.map(c => (states(i), c) -> vars.map(x => if (x.id == i) x -> List(Left(x), Right(i)) else x -> List(Left(x))).toMap)
    ).toMap

    val eta1: Map[(SST_State, Σ), Map[SST_Var, List[Either[SST_Var, Int]]]] = List.range(0, num).flatMap(i =>
      statesToFinal(states(i)).map(s =>
        (s, split) -> vars.map(x => x -> List[Either[SST_Var, Int]](Left(x))).toMap
      )).toMap

    val eta: Map[(SST_State, Σ), Map[SST_Var, List[Either[SST_Var, Int]]]] = eta0 ++ eta1

    val sink = SST_State(num + 1, sstName)

    SST(stateSet + sink,
      s0,
      vars.toSet,
      delta.withDefaultValue(sink),
      eta.withDefaultValue(vars.map(x => x -> List[Either[SST_Var, Int]]()).toMap),
      f)
  }

}