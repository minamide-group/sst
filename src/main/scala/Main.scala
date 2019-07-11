import java.io.File

import builder.Checker

import scala.io.Source

//java -jar checker.jar path
object Main extends App {

  if (args.length == 0) {
    println("filename missing")
  }
  else {
    val filename = args(0)
    val options = toOptions(args.drop(1)).withDefaultValue(List("0"))

    val file = new File(filename)

    if (file.exists()) {
      processFile(file, options)
    }
    else {
      val path = System.getProperty("user.dir") + "\\.." + filename
      val file = new File(path)
      if (file.exists())
        processFile(file, options)
      else
        println("file not found : " + filename)
    }
  }

  def processFile(file: File, options: Map[String, List[String]]): Unit = {

    val lines = Source.fromFile(file.getPath).getLines().toList
    val str = lines.filterNot(_.isEmpty).filterNot(_.startsWith(";")).filterNot(_.toList.forall(y => y.isWhitespace)).mkString

    val (res, msg) = Checker(str, options).output

    if (res)
      println("sat")
    else
      println("unsat")

    if (msg.nonEmpty)
      println(msg)
  }

  def toOptions(args: Array[String]): Map[String, List[String]] = {
    def loop(args: List[String], temp1: String, temp2: List[String], res: Map[String, List[String]]): Map[String, List[String]] = {
      args match {
        case Nil => res + (temp1 -> temp2)
        case x :: xs if (x.startsWith("-")) => loop(xs, x, List(), res + (temp1 -> temp2))
        case x :: xs => loop(xs, temp1, temp2 ::: List(x), res)
      }
    }

    if (args.isEmpty)
      Map()
    else
      loop(args.drop(1).toList, args(0), List(), Map())
  }
}