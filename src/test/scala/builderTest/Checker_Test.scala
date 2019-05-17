
package builderTest

import java.io.File

import builder.Checker
import org.scalatest.FlatSpec

//substr
//substr3
class Checker_Test extends FlatSpec{

  val list = List("at.smt2", "at2.smt2", "bug1.smt2", "bug2.smt2", "empty.smt2",
  "insert.smt2", "insert2.smt2", "int.smt2", "int2.smt2", "reg.smt2", "reg2.smt2",
  "reg3.smt2", "substr.smt2", "substr2.smt2", "substr3.smt2")
  val path : String= "C:\\Users\\leaf6\\IdeaProjects\\Automata\\out\\artifacts\\checker\\"

  "checkAll" should "run" in{
    list.foreach(i=>{
      val file = new File(path+i)
      val res = Checker(file, 0).output
      println(i)
      println(res._1)
      println(res._2)
      println()
    })
  }

  "checkOne" should "run" in{
     val file = new File(path+"empty.smt2")
     val res = Checker(file, 256).output
     println(res._1)
     println(res._2)
  }
}