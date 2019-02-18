package deterministic.factory

import constraint.vars.{FAState, SST_State, SST_Var}
import deterministic.DFA
import deterministic.boundedcopy.SST

case class SSTFactory(charSet: Set[Char]) {

  def replaceFirst(str: String, replacement: String): SST[SST_State, Char, Char, SST_Var] = {

    val sst0 = replaceAll(str, replacement)

    val sstName = "rp"

    val s_last = SST_State(str.length, sstName)

    val states = sst0.states + s_last

    val s0 = sst0.s0

    val v = SST_Var(0, sstName)

    val f: Map[SST_State, List[Either[SST_Var, Char]]] = sst0.f + (s_last -> List(Left(v)))

    val delta = sst0.δ + ((SST_State(str.length - 1, sstName), str.last) -> s_last) ++ charSet.map(ch => (s_last, ch) -> s_last).toMap

    val eta = sst0.η ++ charSet.map(ch => (s_last, ch) -> Map(v -> List(Left(v), Right(ch)))).toMap

    SST(states, s0, Set(v), delta, eta, f)
  }

  def replaceAll(str: String, replacement: String): SST[SST_State, Char, Char, SST_Var] = {

    val next = extendNext(getNext(str))
    val sstName = "rp"
    val states = List.range(0, str.length).map(i => SST_State(i, sstName))
    val s0 = states(0)
    val v = SST_Var(0, sstName)

    val f = List.range(0, str.length).map(i =>
      states(i) -> (List(Left(v)) ::: str.substring(0, i).toList.map(c => Right(c)))
    ).toMap

    val delta0 = List.range(0, str.length).flatMap(i =>
      charSet.map(c => (states(i), c) -> states(0))
    ).toMap

    def getBackDelta(i: Int, list: List[Int], res: Map[(SST_State, Char), SST_State]): Map[(SST_State, Char), SST_State] = {
      list match {
        case Nil => res
        case j :: rest => getBackDelta(i, rest, res + ((states(i), str.charAt(j)) -> states(j + 1)))
      }
    }

    val delta1 = List.range(1, str.length).flatMap(i => getBackDelta(i, next(i), Map()))

    val delta2 = List.range(0, str.length).map(i =>
      (states(i), str.charAt(i)) -> states((i + 1) % str.length)
    ).toMap

    val delta = delta0 ++ delta1 ++ delta2

    val eta0 = List.range(0, str.length).flatMap(i =>
      (charSet - str.charAt(i)).map(c =>
        (states(i), c) -> Map(
          v -> (Left(v) :: str.substring(0, i).toList.map(ch => Right(ch)) ::: List(Right(c)))
        )
      )
    ).toMap

    def getBackEta(i: Int, list: List[Int], res: Map[(SST_State, Char), Map[SST_Var, List[Either[SST_Var, Char]]]]): Map[(SST_State, Char), Map[SST_Var, List[Either[SST_Var, Char]]]] = {
      list match {
        case Nil => res
        case j :: rest => getBackEta(i, rest, res + ((states(i), str.charAt(j)) -> Map(
          v -> (Left(v) :: str.substring(0, i - j).toList.map(ch => Right(ch)))
        )))
      }
    }

    val eta1 = List.range(1, str.length).flatMap(i => getBackEta(i, next(i), Map()))

    val eta2 = List.range(0, str.length).map(i =>
      (states(i), str.charAt(i)) -> Map(
        if (i == str.length - 1) v -> (List(Left(v)) ::: replacement.toList.map(ch => Right(ch)))
        else v -> List(Left(v))
      )
    ).toMap

    val eta = eta0 ++ eta1 ++ eta2

    SST(states.toSet, s0, Set(v), delta, eta, f)
  }

  def reverse: SST[SST_State, Char, Char, SST_Var] = {

    val sstName = "rvs"

    val s0 = SST_State(0, sstName)

    val v0 = SST_Var(0, sstName)

    val f = Map(s0 -> List(Left(v0)))

    val delta = charSet.map(ch => (s0, ch) -> s0).toMap

    val eta = charSet.map(ch => (s0, ch) -> Map(v0 -> List(Right(ch), Left(v0)))).toMap

    SST(Set(s0), s0, Set(v0), delta, eta, f)
  }

  def getNext(str: String): List[Int] = {
    if (str == null || str.length == 0) List()
    else if (str.length == 1) List(-1)
    else if (str.length == 2) List(-1, 0)
    else {
      def findIth(i: Int, cn: Int, str: String, next: List[Int]): Int = {
        if (str.charAt(i - 1) == str.charAt(cn)) cn + 1
        else if (next(cn) >= 0) findIth(i, next(cn), str, next)
        else 0
      }

      def findAll(i: Int, cn: Int, next: List[Int], str: String): List[Int] = {
        if (i < str.length) {
          val newCn = findIth(i, cn, str, next)
          findAll(i + 1, newCn, next ::: List(newCn), str)
        }
        else
          next
      }

      findAll(2, 0, List(-1, 0), str)
    }
  }

  def extendNext(next: List[Int]): List[List[Int]] = {
    def star(list: List[Int]): List[Int] = {
      if (list(0) <= 0) list
      else star(next(list(0)) :: list)
    }

    next.map(c => star(List(c)))
  }

}
