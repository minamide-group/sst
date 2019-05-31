package builderTest

import builder.{SLConsBuilder, SSTBuilder}
import formula.Conjunction
import formula.atomic.{IntegerEquation, StrInRe, WordEquation}
import formula.integer.{IntC, StrLen}
import formula.re.StrToRe
import formula.str.{StrReplace, StrV}
import org.scalatest.FlatSpec

class SSTBuilder_Test extends FlatSpec {

  "not getModel" should "run" in {
    val c1 = WordEquation(StrV("x"), StrReplace(StrV("y"), "p", "r"))
    val c2 = WordEquation(StrV("w"), StrReplace(StrV("z"), "1", "2"))
    val c3 = IntegerEquation(IntC(39), StrLen(StrV("v")), 1)
    val c4 = StrInRe(StrV("u"), StrToRe("aad"), false)
    val formula = Conjunction(Conjunction(c1, c2), Conjunction(c3, c4))

    val consList = SLConsBuilder(formula).output

    val cons = consList(0)
    val sst = SSTBuilder(cons._1, cons._2, cons._3, '#', 5, false, false).output

    sst._1.foreach(i => i.print)
    sst._2.print
  }

  "getModel" should "run" in {
    val c1 = WordEquation(StrV("x"), StrReplace(StrV("y"), "p", "r"))
    val c2 = WordEquation(StrV("w"), StrReplace(StrV("z"), "1", "2"))
    val c3 = IntegerEquation(IntC(39), StrLen(StrV("v")), 1)
    val c4 = StrInRe(StrV("u"), StrToRe("aad"), false)
    val formula = Conjunction(Conjunction(c1, c2), Conjunction(c3, c4))

    val consList = SLConsBuilder(formula).output

    val cons = consList(0)
    val sst = SSTBuilder(cons._1, cons._2, cons._3, '#', 5, true, false).output

    //sst._1.get.foreach(i=>i.print)
    sst._2.print
    sst._3.print
  }

  "not getModel only regular" should "run" in {
    val c3 = IntegerEquation(IntC(39), StrLen(StrV("v")), 1)
    val c4 = StrInRe(StrV("u"), StrToRe("aad"), false)
    val formula = Conjunction(c3, c4)

    val consList = SLConsBuilder(formula).output

    val cons = consList(0)
    val sst = SSTBuilder(cons._1, cons._2, cons._3, '#', 1, false, true).output

    sst._2.print
  }

  "getModel only regular" should "run" in {
    val c3 = IntegerEquation(IntC(39), StrLen(StrV("v")), 1)
    val c4 = StrInRe(StrV("u"), StrToRe("aad"), false)
    val formula = Conjunction(c3, c4)

    val consList = SLConsBuilder(formula).output

    val cons = consList(0)
    val sst = SSTBuilder(cons._1, cons._2, cons._3, '#', 1, true, false).output

    sst._2.printDetail
    sst._3.printDetail
  }

  "not getModel only regular unsat" should "run" in {
    val c3 = StrInRe(StrV("u"), StrToRe("ad"), false)
    val c4 = StrInRe(StrV("u"), StrToRe("aad"), false)
    val formula = Conjunction(c3, c4)

    val consList = SLConsBuilder(formula).output

    val cons = consList(0)
    val sst = SSTBuilder(cons._1, cons._2, cons._3, '#', 1, true, false).output

    sst._2.print
    sst._3.print
  }
}
