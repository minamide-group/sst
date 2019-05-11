import java.io.File

import builder.Checker

//java -jar checker.jar path
object Main extends App {

  if (args.length == 0) {
    println("filename missing")
  }
  else {
    val filename = args(0)

    val file = new File(filename)

    if (file.exists()) {
      processFile(file)
    }
    else {
      val path = System.getProperty("user.dir") + "\\.." + filename
      val file = new File(path)
      if (file.exists())
        processFile(file)
      else
        println("file not found : " + filename)
    }
  }

  def processFile(file: File): Unit = {
    val (res, msg) = Checker(file).output

    if(res)
      println("sat")
    else
      println("unsat")

    if(msg.nonEmpty)
      println(msg)
  }
}