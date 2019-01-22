package constraint

import constraint.regular.RegCons
import constraint.relational.{Concatenation, RelCons, TransducerConstraint}
import constraint.vars.{FAState, StringVariable}
import deterministic.DFA
import deterministic.factory.{DFAFactory, TransducerFactory}

case class ConstraintBuilder(chars : String, rl : List[String], rg : List[String], ic: String) {
  val charSet = chars.toSet
  val tf = TransducerFactory(charSet)
  val df = DFAFactory(charSet)

  //assert input is valid
  private def toRelCons_concat(t : Array[String])={
    val list : List[Either[StringVariable, List[Char]]]= t.toList.drop(1).map(s=>{
      val content = s.substring(2, s.length-1)
      s.charAt(0).toLower match {
        case 'v' => Left(StringVariable( content.toInt ))
        case 'w' => Right(content.toList)
      }
    })
    Concatenation(StringVariable(t(0).toInt),  list)
  }

  private def toRelCons(str : String)={
    val t = str.split(" ").filterNot(_.isEmpty)
    t(1).toLowerCase match {
      case "replace" => {
        val tran = tf.replace(t(2).charAt(0), t(3))
        TransducerConstraint(StringVariable(t(0).toInt), tran, StringVariable(t(4).toInt))
      }
      case "replaceall" => {
        val tran = tf.replaceAll(t(2).charAt(0), t(3))
        TransducerConstraint(StringVariable(t(0).toInt), tran, StringVariable(t(4).toInt))
      }
      case "substring" if t.length>4 => {
        val tran = tf.subString(t(2).toInt, t(3).toInt)
        TransducerConstraint(StringVariable(t(0).toInt), tran, StringVariable(t(4).toInt))
      }
      case "substring"  => {
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
      case _ => toRelCons_concat(t)
    }
  }

  private def toRegCons(str : String)= {
    val t = str.split(" ").filterNot(_.isEmpty)
    t(0).toInt->df.getDFA(t.drop(1).mkString)
  }

  def toConstraints : (String, List[RelCons], Set[RegCons[Char]], Set[Char]) = {
    (ic,
      rl.map(s=>toRelCons(s)),
      rg.map(s=>toRegCons(s)).toMap.map(t => RegCons(StringVariable(t._1), t._2)).toSet,
      charSet)
  }

}
