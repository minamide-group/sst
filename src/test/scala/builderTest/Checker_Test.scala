package builderTest

import java.io.File

import builder.Checker
import org.scalatest.FlatSpec

class Checker_Test extends FlatSpec{
  val path : String= "C:\\Users\\leaf6\\IdeaProjects\\Automata\\out\\artifacts\\checker\\reg2.smt2"
  val file = new File(path)

  "checker" should "run" in{
    val res = Checker(file).output
    println(res._1)
    res._2.foreach(println)
  }
}
