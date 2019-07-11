package builder

import constraint.vars.{SST_State, SST_Var, TransState}
import deterministic.boundedcopy.SST
import formula.str.StrV

case class WitnessBuilder(z3output: String,
                          nameToIdx: Map[StrV, Int],
                          sst_Char: SST[SST_State, Char, Char, SST_Var],
                          trans: nondeterministic.Transducer[TransState, Char, Map[Int, Int]],
                          chars: Set[Char],
                          split: Char,
                          sstList : List[SST[SST_State, Char, Char, SST_Var]] = null
                         ) {

  def output: String = {

    val z3Witness = parse
    val idxToName = nameToIdx.map(t => t._2 -> t._1)
    val len = z3Witness.filter(t => t._1.startsWith("len_")).map(t => t._1.drop(4) -> t._2)
    println(len)

    val strVLength = len.filter(t => t._1.forall(_.isDigit)).map(t => t._1.toInt -> t._2)
    val witness0 = stringWitness(strVLength).zipWithIndex.map(t => idxToName(t._2) -> t._1).map(t => toModel(t._1.name, "\"" + t._2 + "\"", "String"))
    val witness1 = len.filterNot(t => t._1.forall(_.isDigit)).map(t => t._1 -> List.fill(t._2)(chars.head).mkString).map(t => toModel(t._1, "\"" + t._2 + "\"", "String"))
    val intVsWitness = z3Witness.filter(t => t._1.startsWith("intV_")).map(t => t._1.drop(5) -> t._2).map(t => toModel(t._1, t._2.toString, "Int"))

    "(model\n" + witness0.mkString + witness1.mkString + intVsWitness.mkString + ")"
  }

  def toModel(name: String, value: String, typ: String) = "  (define-fun " + name + " () " + typ + "\n    " + value + ")\n"


  def parse: Map[String, Int] = {
    //(define-fun, y, (), Int, 0), (define-fun, x, (), Int, 0),
    val tokens = z3output.replace(10.toChar.toString, " ").
      replace(13.toChar.toString, " ").
      split(" ").filterNot(_.isEmpty).toList.drop(2).dropRight(1)

    def loop(temp: List[String], map: Map[String, Int]): Map[String, Int] = {
      temp match {
        case Nil => map
        case _ => loop(temp.drop(5), map + (temp(1) -> temp(4).dropRight(1).toInt))
      }
    }

    loop(tokens, Map())
  }

  def stringWitness(strVLength: Map[Int, Int]): List[String] = {
    if (sst_Char == null)
      return List()

    val wholeWitness = if (sst_Char.f.contains(sst_Char.s0)) {
      sst_Char.process("")._3
    }
    else if (strVLength == null || strVLength.isEmpty) {
      val sourceWitness = search(sst_Char)
      sst_Char.process(sourceWitness)._3
    }
    else {
      val sourceWitness = search(trans, strVLength)
      if(sstList.size==1)
        sst_Char.process(sourceWitness)._3
      else{
        sstList.last.process(sst_Char.process(sourceWitness)._3)._3
      }
    }
    wholeWitness.mkString.split(split.toString, -1).toList.dropRight(1)
  }

  def search[Q, X](sst: SST[Q, Char, Char, X]): String = {
    //sst.printDetail
    def bfs(queue: List[(Q, String)]): String = {
      queue match {
        case x :: xs => {
          val (q0, s0) = x
          if (sst.f.contains(q0))
            s0
          else {
            val next = sst.δ.filter(r => r._1._1 == q0).map(r => (r._2, s0 + r._1._2)).toList
            bfs(xs ::: next)
          }
        }
      }
    }

    bfs(List((sst.s0, "")))
  }

  def search[Q](trans: nondeterministic.Transducer[Q, Char, Map[Int, Int]], lengths: Map[Int, Int]): String = {
    //println(lengths)
    //trans.print
    def bfs(queue: List[(Q, String, Map[Int, Int])], set : Set[(Q, Map[Int, Int])]): String = {
      queue match {
        case x :: xs => {
          val (q0, s0, m0) = x
          //println(q0 + ", " + m0)
          if (trans.f(q0) && mapEq(m0, lengths))
            s0
          else{
            val next = trans.δ.filter(r => r._1 == q0).map(r => (r._3, s0 + r._2, mapAdd(m0, r._4)))
                          .filter(t => mapLeq(t._3, lengths)).filterNot(x=> set((x._1, x._3)))
            bfs(xs ::: next.toList, set ++ next.map(x=> (x._1, x._3)))
          }
        }
      }
    }

    val initials = trans.s0.map(q => (q, lengths.map(t => t._1 -> 0)))
    bfs(initials.map(x=> (x._1, "", x._2)).toList, initials)
  }

  def mapAdd[X](map1: Map[X, Int], map2: Map[X, Int]): Map[X, Int] = {
    map1.keySet.map(k => k -> (map1(k) + map2.withDefaultValue(0)(k))).toMap
  }

  def mapLeq[X](map1: Map[X, Int], map2: Map[X, Int]): Boolean = {
    map1.keySet.forall(k => map1(k) <= map2.withDefaultValue(0)(k))
  }

  def mapEq[X](map1: Map[X, Int], map2: Map[X, Int]): Boolean = {
    map1.keySet.forall(k => map1(k) == map2.withDefaultValue(0)(k))
  }
}
