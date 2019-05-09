package deterministic.factory

import constraint.vars.TransState
import deterministic.Transducer
import scalaz.Monoid

case class TransducerFactory(charSet: Set[Char]) {

  def replaceFirst(from: Char, to: String): Transducer[TransState, Char, List[Char]] = {

    implicit def monoid = strMonoid

    val states = List(TransState(0), TransState(1))

    val delta = charSet.map(c => (states(0), c) -> states(if (c == from) 1 else 0)).toMap ++ charSet.map(c => (states(1), c) -> states(1)).toMap

    val eta = charSet.map(c => (states(0), c) -> (if (c == from) to.toList else List(c))).toMap ++ charSet.map(c => (states(1), c) -> List(c)).toMap

    Transducer(states.toSet, states(0), delta, eta, states.toSet)

  }

  def replaceAll(from: Char, to: String): Transducer[TransState, Char, List[Char]] = {

    implicit def monoid = strMonoid

    val states = List(TransState(0))

    val delta = charSet.map(c => (states(0), c) -> states(0)).toMap

    val eta = charSet.map(c =>
      (states(0), c) -> (if (c == from) to.toList else List(c))
    ).toMap

    Transducer(states.toSet, states(0), delta, eta, states.toSet)

  }

  def subString(begin: Int, end: Int): Transducer[TransState, Char, List[Char]] = {

    implicit def monoid = strMonoid

    if (begin > end || begin < 0)
      return empty()
    if(begin == end)
      return epsilon()

    val states = List.range(0, end + 1).map(i => TransState(i))

    val delta = List.range(0, end + 1).flatMap(i =>
      charSet.map(c => (states(i), c) -> states(if (i == end) end else i + 1))
    ).toMap

    val eta = List.range(0, end + 1).flatMap(i =>
      charSet.map(c => (states(i), c) -> (if (i < begin || i == end) List() else List(c)))
    ).toMap

    val f = Set(states(end))

    Transducer(states.toSet, states(0), delta, eta, f)
  }

  def epsilon(): Transducer[TransState, Char, List[Char]] = {

    implicit def monoid = strMonoid

    val states: List[TransState] = List(TransState(0))
    val delta: Map[(TransState, Char), TransState] = charSet.map(c => (states(0), c) -> states(0)).toMap
    val eta: Map[(TransState, Char), List[Char]] = charSet.map(c => (states(0), c) -> List()).toMap
    Transducer(states.toSet, states(0), delta, eta, states.toSet)
  }

  def empty(): Transducer[TransState, Char, List[Char]] ={
    implicit def monoid = strMonoid
    Transducer(Set(), TransState(0), Map(), Map(), Set())
  }

  def subString(begin: Int): Transducer[TransState, Char, List[Char]] = {

    implicit def monoid = strMonoid

    if (begin < 0)
      return empty()

    val states = List.range(0, begin + 1).map(i => TransState(i))

    val delta = List.range(0, begin + 1).flatMap(i =>
      charSet.map(c => (states(i), c) -> states(if (i < begin) i + 1 else begin))
    ).toMap

    val eta = List.range(0, begin + 1).flatMap(i =>
      charSet.map(c => (states(i), c) -> (if (i < begin) List() else List(c)))
    ).toMap

    val f = Set(states(begin))

    Transducer(states.toSet, states(0), delta, eta, f)
  }

  private def strMonoid: Monoid[List[Char]] = new Monoid[List[Char]] {
    def append(f1: List[Char], f2: => List[Char]): List[Char] = f1 ::: f2

    def zero: List[Char] = List()
  }
}