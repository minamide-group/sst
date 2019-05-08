package builderTest

import builder.{ParikhBuilder, SLConsBuilder, SSTBuilder}
import formula.{Conjunction, Disjunction, Negation}
import formula.atomic.{IntegerEquation, StrInRe, WordEquation}
import formula.integer.{IntC, StrLen}
import formula.re.StrToRe
import formula.str.{StrReplace, StrV}
import org.scalatest.FlatSpec

class ConstraintToSST_Test extends FlatSpec{

  "for" should "run" in{
    val c1 = WordEquation(StrV("x"), StrReplace(StrV("y"), "p", "r"))
    val c2 = WordEquation(StrV("w"), StrReplace(StrV("z"), "1", "2"))
    val c3 = IntegerEquation(IntC(39), StrLen(StrV("v")), 1)
    val c4 = StrInRe(StrV("u"), StrToRe("aad"), false)
    val formula = Conjunction(Conjunction(c1, c2), Conjunction(c3, c4))

    val cons = SLConsBuilder(formula).output(0)

    val sst = SSTBuilder(cons._1, cons._2, cons._3, '#').output

    sst._1.get.foreach(i=>i.print)
    sst._2.get.print
  }
}
