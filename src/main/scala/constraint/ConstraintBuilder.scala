package constraint

import constraint.regular.RegCons
import constraint.relational.{Concatenation, RelCons, SSTConstraint, TransducerConstraint}
import constraint.vars.StringVariable
import deterministic.factory.{DFAFactory, SSTFactory, TransducerFactory}

case class ConstraintBuilder(chars: String, rl: List[String], rg: List[String], ic: String) {
  val charSet = chars.toSet
  val tf = TransducerFactory(charSet)
  val df = DFAFactory(charSet)
  val sf = SSTFactory(charSet)

  //assert input is valid
  private def toRelCons_concat(t: Array[String]) = {
    val list: List[Either[StringVariable, List[Char]]] = t.toList.drop(1).map(s => {
      val content = s.substring(2, s.length - 1)
      s.charAt(0).toLower match {
        case 'v' => Left(StringVariable(content.toInt))
        case 'w' => Right(content.toList)
      }
    })
    Concatenation(StringVariable(t(0).toInt), list)
  }

  def toRelCons(str: String) = {
    val t = str.split(" ").filterNot(_.isEmpty)
    t(1).toLowerCase match {
      case "replacefirst" if (t(2).length == 1) => {
        val trans = tf.replaceFirst(t(2).charAt(0), t(3))
        TransducerConstraint(StringVariable(t(0).toInt), trans, StringVariable(t(4).toInt))
      }
      case "replacefirst" => {
        val sst = sf.replaceFirst(t(2), t(3))
        SSTConstraint(StringVariable(t(0).toInt), sst, StringVariable(t(4).toInt))
      }
      case "replaceall" | "replace" if (t(2).length == 1) => {
        val trans = tf.replaceAll(t(2).charAt(0), t(3))
        TransducerConstraint(StringVariable(t(0).toInt), trans, StringVariable(t(4).toInt))
      }
      case "replaceall" | "replace" => {
        val sst = sf.replaceAll(t(2), t(3))
        SSTConstraint(StringVariable(t(0).toInt), sst, StringVariable(t(4).toInt))
      }
      case "reverse" => {
        val sst = sf.reverse
        SSTConstraint(StringVariable(t(0).toInt), sst, StringVariable(t(2).toInt))
      }
      case "substring" if t.length > 4 => {
        val tran = tf.subString(t(2).toInt, t(3).toInt)
        TransducerConstraint(StringVariable(t(0).toInt), tran, StringVariable(t(4).toInt))
      }
      case "substring" => {
        val tran = tf.subString(t(2).toInt)
        TransducerConstraint(StringVariable(t(0).toInt), tran, StringVariable(t(3).toInt))
      }
      case "before" => {
        val tran = tf.before(t(2).charAt(0))
        TransducerConstraint(StringVariable(t(0).toInt), tran, StringVariable(t(3).toInt))
      }
      case "after" => {
        val tran = tf.after(t(2).charAt(0))
        TransducerConstraint(StringVariable(t(0).toInt), tran, StringVariable(t(3).toInt))
      }
      case "at"=>{
        val tran = tf.at(t(2).toInt)
        TransducerConstraint(StringVariable(t(0).toInt), tran, StringVariable(t(3).toInt))
      }
      case _ => toRelCons_concat(t)
    }
  }

  def toRegCons(str: String) = {
    val t = str.split(" ").filterNot(_.isEmpty)
    RegCons(StringVariable(t(0).toInt), df.getDFA(t.drop(1).mkString))
  }

  def toConstraints: (String, List[RelCons], Set[RegCons[Char]], Set[Char]) = {
    (ic,
      rl.map(s => toRelCons(s)),
      rg.map(s => toRegCons(s)).groupBy(_.x).map(t=>{
        val head = t._2.head
        val rest = t._2.drop(1)
        val dfa = rest.foldLeft(head.R){(x,y) => x.intersect(y.R).rename}
        RegCons(t._1, dfa.minimize.rename)
      }).toSet,
      charSet)
  }

}
