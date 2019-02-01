package constraint

import constraint.regular.RegCons
import constraint.relational._
import constraint.vars._
import deterministic.boundedcopy.{Composition, SST}
import deterministic.{DFA, Transducer}

case class SSTBuilder[Σ](charSet: Set[Σ],
                         //assert split is not in alphabets
                         split: Σ) {

  type MySST[X] = SST[SST_State, Σ, X, SST_Var]
  type Out[X] = Either[SST_Var, X]

  private def listC(list: Any*): List[Out[Σ]] =
    list.collect {
      case x: SST_Var => Left(x)
      case c: Σ => Right(c)
    }.toList

  def getStem(num: Int, name: String): MySST[Σ] = {
    //return a sst with |vars| = num, and |states| = num + 1
    val states = List.range(0, num + 1).map(i => SST_State(i, name))
    val vars = List.range(0, num).map(i => SST_Var(i, name))
    val s0 = states(0)
    val f = Map(states(num) -> vars.foldLeft(listC()) { (x, y) => x ::: listC(y, split) })

    val delta = List.range(0, num).flatMap(i => {
      charSet.map(c => {
        (states(i), c) -> states(i)
      }) + ((states(i), split) -> states(i + 1))
    }).toMap

    val unit = vars.map(x => x -> listC(x)).toMap

    val eta = List.range(0, num).flatMap(i => {
      charSet.map(c => {
        (states(i), c) -> (unit + (vars(i) -> listC(vars(i), c)))
      }) + ((states(i), split) -> unit)
    }).toMap

    SST(states.toSet, s0, vars.toSet, delta, eta, f)
  }

  private def replace(sst: MySST[Σ], q: SST_State, replacement: DFA[FAState, Σ]): MySST[Σ] = {
    def tName(s: FAState) = "t" + s.id

    val v = SST_Var(q.id, q.name)
    val toNewStates = replacement.states.map(x => x -> SST_State(q.id, tName(x))).toMap
    val newStates = sst.states - q ++ replacement.states.map(toNewStates(_))
    val newS0 = if (sst.s0 == q) toNewStates(replacement.s0) else sst.s0

    val newDelta = sst.δ.filterNot(r => r._1._1 == q || r._2 == q) ++
      replacement.δ.map(r => (toNewStates(r._1._1), r._1._2) -> toNewStates(r._2)) ++
      sst.δ.filter(r => r._2 == q && r._1._2 == split).map(r => r._1 -> toNewStates(replacement.s0)) ++
      replacement.f.map(qf => (toNewStates(qf), split) -> sst.δ(q, split))

    val unit = sst.vars.map(x => x -> listC(x)).toMap

    val newEta = sst.η.filterNot(r => r._1._1 == q) ++
      replacement.δ.map(r => (toNewStates(r._1._1), r._1._2) -> (unit + (v -> listC(v, r._1._2)))) ++
      replacement.f.map(qf => (toNewStates(qf), split) -> unit)

    SST(newStates, newS0, sst.vars, newDelta, newEta, sst.f)
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
      List.range(0, sst.vars.map(v => v.id).size).map(i => Left(SST_Var(i, t._1.name)))
    )
    val vink = sst.vars.map(x => x -> List[Out[Int]]()).toMap
    val newEta = sst.η.map(r =>
      r._1 -> r._2.map(t =>
        t._1 -> t._2.collect {
          case Left(v) => Left(v)
          case Right(_) => Right(t._1.id)
        }
      )
    ).withDefaultValue(vink)
    SST(sst.states, sst.s0, sst.vars, sst.δ, newEta, newF)
  }

  private def replaceAllDFA(sst: MySST[Σ], list: List[RegCons[Σ]], name: String): MySST[Σ] = {
    list match {
      case x :: rest => replaceAllDFA(replace(sst, SST_State(x.x.id, name), x.R), rest, name)
      case Nil => sst
    }
  }

  def getOne(relCons: RelCons, set: Set[RegCons[Σ]]): MySST[Σ] = {
    val num = relCons.getLeftIdx
    val name = "sst" + num
    val sst0 = getStem(num, name)
    val sst1 = relCons match {
      case c: Concatenation[Σ] => {
        val newF = sst0.f.map(t =>
          t._1 -> (
            t._2 :::
              c.list.flatMap(e => {
                if (e.isLeft)
                  listC(SST_Var(e.left.get.id, name))
                else
                  e.right.get.map(ch => Right(ch))
              }) :::
              listC(split)
            )
        )
        SST(sst0.states, sst0.s0, sst0.vars, sst0.δ, sst0.η, newF)
      }
      case t: TransducerConstraint[Σ] => replace(sst0, SST_State(t.right2.id, name), t.right1)
      case s: SSTConstraint[Σ] => replace(sst0, SST_State(s.right2.id, name), s.right1)
    }
    val sst2 = replaceAllDFA(sst1, set.toList, name)
    val sst3 = addDefault(sst2)
    sst3
  }

  private def getLast(num: Int, set: Set[RegCons[Σ]]): MySST[Σ] = {
    val sstName = "sst" + num
    addDefault(replaceAllDFA(getStem(num, sstName), set.toList, sstName))
  }

  private def addDefault(sst: MySST[Σ]): MySST[Σ] = {
    val sink = SST_State(-1, sst.s0.name + "sink")
    val vink = sst.vars.map(x => x -> listC()).toMap
    SST(sst.states + sink, sst.s0, sst.vars,
      sst.δ.withDefaultValue(sink),
      sst.η.withDefaultValue(vink),
      sst.f
    )
  }

  def constraintsToSSTs(list: List[RelCons], set: Set[RegCons[Σ]]): List[MySST[Σ]] = {
    def star(relCons: List[RelCons], set: Set[RegCons[Σ]], res: List[MySST[Σ]]): List[MySST[Σ]] = {
      relCons match {
        case x :: rest => {
          val leftId = x.getLeftIdx
          val regCons = x match {
            //Todo: optimized with intersection of DFA and SST
            case _: Concatenation[Σ] => set.filter(p => p.x.id < leftId)
            case t: TransducerConstraint[Σ] => set.filter(p => p.x.id < leftId && p.x.id != t.right2.id)
            case s: SSTConstraint[Σ] => set.filter(p => p.x.id < leftId && p.x.id != s.right2.id)
          }
          star(rest, set -- regCons, res ::: List(getOne(x, regCons)))
        }
        case Nil if set.isEmpty => res
        case Nil => res ::: List(getLast(list.last.getLeftIdx + 1, set))
      }
    }

    star(list, set, List())
  }

  def composeSSTs(ssts: List[MySST[Σ]]): Option[MySST[Int]] = {
    val list = ssts.dropRight(1)
    val last = renameToInt(ssts.last)
    if (list.isEmpty)
      Some(last)
    else if (list.size == 1) {
      val sst = compose(list(0), last)
      if (sst.f.isEmpty)
        None
      else
        Some(sst)
    }
    else {
      def star(sst: MySST[Σ], list: List[MySST[Σ]], last: MySST[Int]): Option[MySST[Int]] = {
        list match {
          case Nil => {
            val sst1 = compose(sst, last)
            if (sst1.f.nonEmpty)
              Some(sst1)
            else
              None
          }
          case x :: rest => {
            val sst1 = compose(sst, x)
            if (sst1.f.nonEmpty)
              star(sst1, rest, last)
            else
              None
          }
        }
      }

      star(compose(list(0), list(1)), list.drop(2), last)
    }
  }

  private def compose[X](sst1: MySST[Σ], sst2: MySST[X]): MySST[X] = Composition.compose(sst1, sst2).trim.rename("r0")
}