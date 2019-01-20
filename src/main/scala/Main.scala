import java.io.File

import constraint._

import scala.io.Source

//java -jar checker.jar path
object Main extends App {

  if (args.length == 0) {
    println("filename missing")
  }
  else{
    val filename = args(0)

    val file = new File(filename)

    if(file.exists()){
      processFile(file)
    }
    else{
      val path = System.getProperty("user.dir") + "\\.." + filename
      val file = new File(path)
      if(file.exists())
        processFile(file)
      else
        println("file not found : " + filename)
    }
  }

  def processFile(file : File): Unit ={
    if(!file.canRead)
      println("file can not be read : " + file.getName)
    else{
      val markSet = Set("chars:", "intCons:", "relCons:", "regCons:")
      val lines = Source.fromFile(file.getPath).getLines().filterNot(s=> s.isEmpty || s.startsWith("//") || s.toList.foldLeft(true){(x,y)=>x&&y.isWhitespace } ).toList
      val marks = lines.zipWithIndex.filter(t => markSet(t._1.filterNot(_.isWhitespace))).map(t=> (t._1.filterNot(_.isWhitespace), t._2))

      val chars = lines.slice(marks(0)._2+1, marks(1)._2).mkString
      val ic = lines.slice(marks(1)._2+1, marks(2)._2).mkString
      val rl = lines.slice(marks(2)._2+1, marks(3)._2)
      val rg = lines.slice(marks(3)._2+1, lines.length)

      val startTime = System.currentTimeMillis()
      val res = Checker.process( ConstraintBuilder(chars, rl, rg, ic).toConstraints, '#')
      val timeCost = System.currentTimeMillis() - startTime

      println(if(res) "sat" else "unsat")
      println("time cost : " + timeCost.toDouble/1000 + "s")
    }
  }
}