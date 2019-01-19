package constraint

import constraint.regular.RegCons
import constraint.relational.{Concatenation, RelCons, TransducerConstraint}
import constraint.vars.{FAState, StringVariable}
import deterministic.DFA
import deterministic.factory.{DFAFactory, TransducerFactory}

class ConstraintBuilder(chars : String) {

  val charSet : Set[Char] = chars.toSet
  val tf = TransducerFactory(charSet)
  val df = DFAFactory()

  var relCons : List[RelCons] = List()
  var regCons : Map[Int, DFA[FAState, Char]] = Map()
  var intCons : String = ""

  //assert input is valid

  def addRelCons_0(str : String){
    val t = str.split(" ").filterNot(_.isEmpty)
    val list : List[Either[StringVariable, List[Char]]]= t.toList.drop(1).map(s=>{
      val content = s.substring(2, s.length-1)
      s.charAt(0).toLower match {
        case 'v' => Left(StringVariable( content.toInt ))
        case 'w' => Right(content.toList)
      }
    })
    relCons = relCons ++ List( Concatenation(StringVariable(t(0).toInt),  list) )
  }

  def addRelCons_1(str : String){
    val t = str.split(" ").filterNot(_.isEmpty)
    t(1).toLowerCase match {
      case "replace" => {
        val tran = tf.replace(t(2).charAt(0), t(3))
        relCons = relCons ::: List(TransducerConstraint(StringVariable(t(0).toInt), tran, StringVariable(t(4).toInt)))
      }
      case "replaceall" => {
        val tran = tf.replaceAll(t(2).charAt(0), t(3))
        relCons = relCons ::: List(TransducerConstraint(StringVariable(t(0).toInt), tran, StringVariable(t(4).toInt)))
      }
      case "substring" if t.length>4 => {
        val tran = tf.subString(t(2).toInt, t(3).toInt)
        relCons = relCons ::: List(TransducerConstraint(StringVariable(t(0).toInt), tran, StringVariable(t(4).toInt)))
      }
      case "substring"  => {
        val tran = tf.subString(t(2).toInt)
        relCons = relCons ::: List(TransducerConstraint(StringVariable(t(0).toInt), tran, StringVariable(t(3).toInt)))
      }
      case "before" => {
        val tran = tf.before(t(2).charAt(0))
        relCons = relCons ::: List(TransducerConstraint(StringVariable(t(0).toInt), tran, StringVariable(t(3).toInt)))
      }
      case "after" => {
        val tran = tf.after(t(2).charAt(0))
        relCons = relCons ::: List(TransducerConstraint(StringVariable(t(0).toInt), tran, StringVariable(t(3).toInt)))
      }
    }
  }

  def addRegCons(str : String) {
    val t = str.split(" ").filterNot(_.isEmpty)
    regCons = regCons + (t(0).toInt->df.getDFA(t(1)))
  }

  def addIntCons(str : String){
    //assert in z3 -smt2 from
    //assert variable is either len[num] or x[num]
    intCons = str
  }

  def toConstraints : (String, List[RelCons], Set[RegCons[Char]], Set[Char]) = {
    (intCons, relCons, regCons.map(t=> RegCons(StringVariable(t._1), t._2)).toSet, charSet)
  }

}
