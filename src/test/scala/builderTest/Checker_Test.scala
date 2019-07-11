
package builderTest

import java.io.File

import builder.Checker
import org.scalatest.FlatSpec

import scala.io.Source

class Checker_Test extends FlatSpec {

  //bug2
  val list = List("at.smt2", "at2.smt2", "bug1.smt2", "bug2.smt2", "bug3.smt2", "bug4.smt2", "bug5.smt2",
    "insert.smt2", "insert2.smt2", "int.smt2", "int2.smt2", "int3.smt2", "reg.smt2", "reg2.smt2",
    "reg3.smt2", "substr.smt2", "substr2.smt2", "substr3.smt2", "reconcat.smt2", "reunion.smt2", "reallchar.smt2", "speed1.smt2", "speed2.smt2",
    "slow2.smt2")
  val path: String = "C:\\Users\\leaf6\\IdeaProjects\\Automata\\out\\artifacts\\checker\\"

  "checkAll" should "run" in {
    list.foreach(i => {
      val file = new File(path + i)
      val res = Checker(getString(file), Map()).output
      println(i)
      println(res._1)
      println(res._2)
      println()
    })
  }

  "checkOne" should "run" in {
    val file = new File(path + "test.smt2")
    val options = Map("-p" -> List(), "-parikh"->List())
    val res = Checker(getString(file), options).output
    println(res._1)
    println(res._2)
  }

  def getString(file : File) : String  ={
    val lines = Source.fromFile(file.getPath).getLines().toList
    val str = lines.filterNot(_.isEmpty).filterNot(_.startsWith(";")).filterNot(_.toList.forall(y => y.isWhitespace)).mkString
    str
  }
}