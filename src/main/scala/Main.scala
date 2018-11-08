import scala.util.matching.Regex

object Main extends App {

  val rex = "a|b".r
  println(rex.findFirstIn("bbb"))
}
