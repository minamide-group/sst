package builderTest

import builder.SLConsBuilder
import formula.{Conjunction, Disjunction, Negation}
import formula.atomic.{IntegerEquation, StrInRe, WordEquation}
import formula.integer.{IntC, StrLen}
import formula.re.StrToRe
import formula.str.{StrReplace, StrV}
import org.scalatest.FlatSpec

class FormulaToConstraint_Test extends FlatSpec{

  "for" should "run" in{
    val c1 = WordEquation(StrV("x"), StrReplace(StrV("y"), "pattern", "replacement"))
    val c2 = WordEquation(StrV("w"), StrReplace(StrV("z"), "pattern1", "replacement"))
    val c3 = IntegerEquation(IntC(39), StrLen(StrV("v")), 1)
    val c4 = StrInRe(StrV("u"), StrToRe("aad"), false)
    val formula = Disjunction(Conjunction(c1, c2), Negation(Conjunction(Negation(c3), c4)))

    SLConsBuilder(formula).output.foreach(println)
  }
}
