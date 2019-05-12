package builder

import constraint.atomicSL.{AtomicSLCons, Concatenation, SSTConstraint, TransducerConstraint}
import constraint.regular.RegCons
import constraint.vars.{FAState, SST_State, SST_Var, TransState}
import deterministic.boundedcopy.{Composition, SST}
import deterministic.factory.SSTFactory
import deterministic.{DFA, Transducer}

//from constraints to SSTs, and compose SST
case class SSTBuilder[Σ](atomicSLCons: List[AtomicSLCons],
                         regCons: Set[RegCons[Σ]],
                         chars: Set[Σ],
                         split: Σ,
                         varNum : Int,
                         getModel : Boolean,
                         getLength : Boolean) {

  type MySST[X] = SST[SST_State, Σ, X, SST_Var]
  type Out[X] = Either[SST_Var, X]

  def output: (List[MySST[Σ]], MySST[Int], MySST[Σ], Boolean) = {
    if (atomicSLCons.isEmpty && regCons.isEmpty)
      (List(), null, null, true)
    else {
      val sstListOption = constraintsToSSTs(atomicSLCons, regCons)

      if(sstListOption.isEmpty)
        (List(), null, null, false)
      else{
        val sstList = sstListOption.get
        val (sst_int, sst_char, sstNonEmpty) = composeSSTs(sstList, getModel, getLength)
        (sstList, sst_int, sst_char, sstNonEmpty)
      }
    }
  }

  def getTimeSecond(start : Long, end : Long) : String = ((end-start)*1.0/1000).toString

  def getStem(num: Int, name: String): MySST[Σ] = {
    //return a sst with |vars| = num, and |states| = num + 1
    val states = List.range(0, num + 1).map(i => SST_State(i, name))
    val vars = List.range(0, num).map(i => SST_Var(i, name))
    val s0 = states(0)
    val f = Map(states(num) -> vars.foldLeft(listC()) { (x, y) => x ::: listC(y, split) })

    val delta = List.range(0, num).flatMap(i => {
      chars.map(c => {
        (states(i), c) -> states(i)
      }) + ((states(i), split) -> states(i + 1))
    }).toMap

    val unit = vars.map(x => x -> listC(x)).toMap

    val eta = List.range(0, num).flatMap(i => {
      chars.map(c => {
        (states(i), c) -> (unit + (vars(i) -> listC(vars(i), c)))
      }) + ((states(i), split) -> unit)
    }).toMap

    SST(states.toSet, s0, vars.toSet, delta, eta, f)
  }

  def getOne(relCons: AtomicSLCons, varDFA: Map[Int, DFA[FAState, Σ]]): MySST[Σ] = {
    val num = relCons.getLeftIdx
    val name = "sst" + num
    val sst0 = getStem(num, name)

    val sst1 = relCons match {
      case c: Concatenation[Σ] => {
        val newF = sst0.f.map(t =>
          t._1 -> (
            t._2 :::
              c.list.map(e => {
                if (e.isLeft)
                  Left(SST_Var(e.left.get, name))
                else
                  Right(e.right.get)
              }) :::
              listC(split)
            )
        )
        SST(sst0.states, sst0.s0, sst0.vars, sst0.δ, sst0.η, newF)
      }
      case t: TransducerConstraint[Σ] => replace(sst0, SST_State(t.source, name), t.fst)
      case s: SSTConstraint[Σ] => replace(sst0, SST_State(s.source, name), s.sst)
    }
    val sst2 = replaceAllDFA(sst1, varDFA.toList, name)

    sst2
  }

  def constraintsToSSTs(list: List[AtomicSLCons], set: Set[RegCons[Σ]]): Option[List[MySST[Σ]]] = {
    def star(relCons: List[AtomicSLCons], varDFA: Map[Int, DFA[FAState, Σ]], res: List[MySST[Σ]]): Option[List[MySST[Σ]]] = {
      relCons match {
        case x :: rest => {
          val leftId = x.getLeftIdx
          x match {
            case _: Concatenation[Σ] => {
              val regCons = varDFA.filter(p => p._1 < leftId)
              star(rest, varDFA.filterNot(p => p._1 < leftId), res ::: List(getOne(x, regCons)))
            }
            case t: TransducerConstraint[Σ] => {
              val regCons = varDFA.filter(p => p._1 < leftId && p._1 != t.source)
              val fst = if (varDFA.contains(t.source)) addDefault(t.fst).intersect(addDefault(varDFA(t.source))).trim.rename else t.fst
              if(fst.states.isEmpty)
                None
              else
                star(rest, varDFA.filterNot(p => p._1 < leftId), res ::: List(getOne(TransducerConstraint(t.left, fst, t.source), regCons)))
            }
            case s: SSTConstraint[Σ] => {
              val regCons = varDFA.filter(p => p._1 < leftId && p._1 != s.source)
              val sst = if (varDFA.contains(s.source)) compose(addDefault(dfaToSST(varDFA(s.source))), addDefault(s.sst)) else s.sst
              if(sst.states.isEmpty)
                None
              else
                star(rest, varDFA.filterNot(p => p._1 < leftId), res ::: List(getOne(SSTConstraint(s.left, sst, s.source), regCons)))
            }
          }
        }
        case Nil if varDFA.isEmpty => Some(res)
        case Nil => Some(res ::: List(getLast(varNum, varDFA)))
      }
    }

    val varDFA = set.map(t => t.x -> t.R).toMap
    star(list, varDFA, List())
  }

  def composeSSTs(sstList: List[MySST[Σ]], getModel : Boolean, getLength : Boolean): (MySST[Int], MySST[Σ], Boolean) = {
    val list = sstList.dropRight(1)
    val last_char = sstList.last.trim
    if(list.size==0) {
      if(getLength && getModel)
        (renameToInt(last_char), last_char, last_char.states.nonEmpty)
      else if(getLength){
        (renameToInt(last_char), null, last_char.states.nonEmpty)
      }
      else{
        (null, last_char, last_char.states.nonEmpty)
      }
    }
    else{
      val sst0 = composeSSTs(list)
      if(getLength && getModel) {
        val sst_int = compose(sst0, renameToInt(last_char))
        val sst_char = compose(sst0, last_char)
        (sst_int, sst_char, sst_char.states.nonEmpty)
      }
      else if(getLength){
        val sst_int = compose(sst0, renameToInt(last_char))
        (sst_int, null, sst_int.states.nonEmpty)
      }
      else{
        val sst_char = compose(sst0, last_char)
        ( null, sst_char, sst_char.states.nonEmpty)
      }
    }
  }

  def composeSSTs(sstList: List[MySST[Σ]]) : MySST[Σ] = {
    if(sstList.size==1)
      return sstList.head.trim
    sstList.drop(2).foldLeft(compose(sstList(0), sstList(1))){(x,y)=> compose(x,y)}
  }

  def dfaToSST(dfa: DFA[FAState, Σ]): MySST[Σ] = {
    val toNewStates = dfa.states.map(s => s -> SST_State(s.id, "d")).toMap
    val states = dfa.states.map(toNewStates(_))
    val s0 = toNewStates(dfa.s0)
    val x = SST_Var(0, "d")
    val vars = Set(x)
    val delta = dfa.δ.map(r => (toNewStates(r._1._1), r._1._2) -> toNewStates(r._2))
    val eta = delta.map(r => r._1 -> Map(x -> listC(x, r._1._2)))
    val f = dfa.f.map(q => toNewStates(q) -> listC(x)).toMap
    SST(states, s0, vars, delta, eta, f)
  }

  private def listC(list: Any*): List[Out[Σ]] = list.collect {
    case x: SST_Var => Left(x)
    case c: Σ => Right(c)
  }.toList

  private def replace(sst: MySST[Σ], q: SST_State, replacement: DFA[FAState, Σ]): MySST[Σ] = {
    def tName(s: FAState) = "t" + s.id

    val v = SST_Var(q.id, q.name)
    val toNewStates = replacement.states.map(x => x -> SST_State(q.id, tName(x))).toMap
    val newStates = sst.states - q ++ replacement.states.map(toNewStates(_))
    val newS0 = if (sst.s0 == q) toNewStates.withDefaultValue(SST_State(0, "t0"))(replacement.s0) else sst.s0

    val newDelta = sst.δ.filterNot(r => r._1._1 == q || r._2 == q) ++
      replacement.δ.map(r => (toNewStates(r._1._1), r._1._2) -> toNewStates(r._2)) ++
      sst.δ.filter(r => r._2 == q && r._1._2 == split).map(r => r._1 -> toNewStates(replacement.s0)) ++
      replacement.f.map(qf => (toNewStates(qf), split) -> sst.δ(q, split))

    val unit = sst.vars.map(x => x -> listC(x)).toMap

    val newEta = sst.η.filterNot(r => r._1._1 == q) ++
      replacement.δ.map(r => (toNewStates(r._1._1), r._1._2) -> (unit + (v -> listC(v, r._1._2)))) ++
      replacement.f.map(qf => (toNewStates(qf), split) -> unit)

    val res = SST(newStates, newS0, sst.vars, newDelta, newEta, sst.f)
    res
  }

  private def replace(sst: MySST[Σ], q: SST_State, replacement: MySST[Σ]): MySST[Σ] = {
    def tName(id: Int) = "t" + id

    val from = SST_Var(q.id, q.name)
    val to = SST_Var(sst.vars.size, q.name)
    val toNewStates = replacement.states.map(x => x -> SST_State(q.id, tName(x.id))).toMap
    val toNewVars = replacement.vars.map(x => x -> SST_Var(to.id, tName(x.id))).toMap
    val newStates = sst.states - q ++ replacement.states.map(toNewStates(_))
    val newS0 = if (sst.s0 == q) toNewStates(replacement.s0) else sst.s0
    val newVars = sst.vars + to ++ replacement.vars.map(toNewVars(_))
    val newF = sst.f.map(t => t._1 ->
      List.range(0, to.id + 1).map(i => SST_Var(i, q.name)).foldLeft(listC()) { (x, y) => x ::: listC(y, split) }
    )

    val newDelta = sst.δ.filterNot(r => r._1._1 == q || r._2 == q) ++
      replacement.δ.map(r => (toNewStates(r._1._1), r._1._2) -> toNewStates(r._2)) ++
      sst.δ.filter(r => r._2 == q && r._1._2 == split).map(r => r._1 -> toNewStates(replacement.s0)) ++
      replacement.f.keySet.map(qf => (toNewStates(qf), split) -> sst.δ(q, split))

    val unit = (sst.vars + to).map(x => x -> listC(x)).toMap ++
      replacement.vars.map(toNewVars(_)).map(x => x -> listC()).toMap

    val newEta = sst.η.filterNot(r => r._1._1 == q).map(r => r._1 -> (unit ++ r._2)) ++
      replacement.η.map(r => (
        toNewStates(r._1._1), r._1._2) -> (
        unit ++
          r._2.map(t => toNewVars(t._1) -> t._2.collect {
            case Left(v) => Left(toNewVars(v))
            case Right(c) => Right(c)
          }) +
          (from -> listC(from, r._1._2))
        )
      ) ++
      replacement.f.map(t =>
        (toNewStates(t._1), split) -> (
          unit + (to -> t._2.collect {
            case Left(v) => Left(toNewVars(v))
            case Right(c) => Right(c)
          })
          )
      )


    SST(newStates, newS0, newVars, newDelta, newEta, newF)
  }

  private def replace(sst: MySST[Σ], q: SST_State, replacement: Transducer[TransState, Σ, List[Σ]]): MySST[Σ] = {
    def tName(id: Int) = "t" + id

    val from = SST_Var(q.id, q.name)
    val to = SST_Var(sst.vars.size, q.name)
    val toNewStates = replacement.states.map(x => x -> SST_State(q.id, tName(x.id))).toMap
    val newStates = sst.states - q ++ replacement.states.map(toNewStates(_))
    val newS0 = if (sst.s0 == q) toNewStates(replacement.s0) else sst.s0
    val newVars = sst.vars + to
    val newF = sst.f.map(t => t._1 ->
      List.range(0, to.id + 1).map(i => SST_Var(i, q.name)).foldLeft(listC()) { (x, y) => x ::: listC(y, split) }
    )

    val newDelta = sst.δ.filterNot(r => r._1._1 == q || r._2 == q) ++
      replacement.δ.map(r => (toNewStates(r._1._1), r._1._2) -> toNewStates(r._2)) ++
      sst.δ.filter(r => r._2 == q && r._1._2 == split).map(r => r._1 -> toNewStates(replacement.s0)) ++
      replacement.f.map(qf => (toNewStates(qf), split) -> sst.δ(q, split))

    val unit = newVars.map(x => x -> listC(x)).toMap

    val newEta = sst.η.filterNot(r => r._1._1 == q).map(r => r._1 -> (unit ++ r._2)) ++
      replacement.η.map(r => (
        toNewStates(r._1._1), r._1._2) -> (
        unit + (from -> listC(from, r._1._2)) + (to -> (listC(to) ::: r._2.map(Right(_))))
        )
      ) ++
      replacement.f.map(t => (toNewStates(t), split) -> unit)

    SST(newStates, newS0, newVars, newDelta, newEta, newF)
  }

  private def renameToInt(sst: MySST[Σ]): MySST[Int] = {
    val newF = sst.f.map(t => t._1 ->
      List.range(0, sst.vars.filter(v=>v.name.contains("sst")).size).map(i => Left(SST_Var(i, t._1.name)))
    )

    val newEta = sst.η.map(r =>
      r._1 -> r._2.map(t =>
        t._1 -> t._2.collect {
          case Left(v) => Left(v)
          case Right(_) => Right(t._1.id)
        }
      )
    )
    SST(sst.states, sst.s0, sst.vars, sst.δ, newEta, newF)
  }

  private def replaceAllDFA(sst: MySST[Σ], list: List[(Int, DFA[FAState, Σ])], name: String): MySST[Σ] = {
    list match {
      case x :: rest => replaceAllDFA(replace(sst, SST_State(x._1, name), x._2), rest, name)
      case Nil => sst
    }
  }

  private def getLast(num: Int, varDFA: Map[Int, DFA[FAState, Σ]]): MySST[Σ] = {
    val sstName = "sst" + num
    replaceAllDFA(getStem(num, sstName), varDFA.toList, sstName).trim
  }

  private def addDefault[X](sst: MySST[X]): MySST[X] = {
    val sink = SST_State(-1, sst.s0.name + "sink")
    val states = sst.states + sink
    val alphabet = chars + split
    val variables = if(sst.vars.nonEmpty) sst.vars else Set(SST_Var(-1, sst.s0.name))
    val defaultDelta = states.flatMap(s=>alphabet.map(c=> (s,c)->sink)).toMap
    val defaultUpdate = variables.map(x=> x->List[Either[SST_Var, X]]()).toMap
    val defaultEta = states.flatMap(s=>alphabet.map(c=> (s,c)->defaultUpdate)).toMap
    SST(states, sst.s0, variables,
      defaultDelta ++ sst.δ,
      defaultEta ++ sst.η.map(r=> r._1->(defaultUpdate ++ r._2)),
      sst.f
    )
  }

  private def addDefault(dfa: DFA[FAState, Σ]): DFA[FAState, Σ] = {
    val sink = FAState(-1)
    DFA(dfa.states + sink, dfa.s0, dfa.δ.withDefaultValue(sink), dfa.f)
  }

  private def addDefault(trans: Transducer[TransState, Σ, List[Σ]]): Transducer[TransState, Σ, List[Σ]] = {
    val sink = TransState(-1)

    implicit def e = trans.e

    Transducer(trans.states + sink, trans.s0, trans.δ.withDefaultValue(sink), trans.η, trans.f)
  }

  def compose[X](sst1: MySST[Σ], sst2: MySST[X]): MySST[X] = Composition.compose(addDefault(sst1.trim), addDefault(sst2.trim)).trim.rename("r0")

}
