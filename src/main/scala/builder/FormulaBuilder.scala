package builder

import formula.{Conjunction, Disjunction, ReturnBoolean, Negation}
import formula.atomic.{IntegerEquation, StrInRe, WordEquation}
import formula.integer._
import formula.re._
import formula.str._

case class FormulaBuilder(lines : List[String]) {

  def output : (ReturnBoolean, Boolean) = {

    def loop(tokens : List[String], p : ReturnBoolean, strV : Set[String], intV : Set[String], checkSat : Boolean, getModel : Boolean
            ) : (ReturnBoolean, Set[String], Set[String], Boolean, Boolean)={
      tokens(1) match {
        case "check-sat" => {
          val temp = parseCheckSat(tokens)
          if(temp.isEmpty)
            (p, strV, intV, true, getModel)
          else /*if(temp(1).equals("get-model"))*/
            (p, strV, intV, true, true)
        }
        case "declare-const"=>{
          val (temp, strV1, intV1) = parseDeclareConst(tokens, strV, intV)
          loop(temp, p, strV1, intV1, checkSat, getModel)
        }
        case "declare-fun"=>{
          val (temp, strV1, intV1) = parseDeclareFun(tokens, strV, intV)
          loop(temp, p, strV1, intV1, checkSat, getModel)
        }
        case "assert" =>{
          val (temp, formula) = parseAssert(tokens, strV, intV)
          if(p==null)
            loop(temp, formula, strV, intV, checkSat, getModel)
          else
            loop(temp, Conjunction(p, formula), strV, intV, checkSat, getModel)
        }
      }
    }

    val (res, _,  _, _, getModel) = loop(getTokens(lines), null, Set[String](), Set[String](), false, false)
    (res, getModel)
  }

  val diseq = Map(
    "<" ->  2,
    ">="-> -2,
    ">" ->  3,
    "<="-> -3
  )

  def getTokens(lines : List[String]) : List[String] = {
    val source = lines.filterNot(_.isEmpty).filterNot(_.startsWith(";")).filterNot(_.toList.forall(y => y.isWhitespace))
      .mkString.replaceAll("\\(", " ( ").replaceAll("\\)", " ) ").toList
    def loop(quote : Boolean, source : List[Char], temp : String, res : List[String]) : List[String] ={
      source match {
        case Nil => res:::List(temp)
        case x::xs if !quote && x=='\"' => loop(true, xs, ""+x, res)
        case x::xs if quote && x=='\"' => loop(false, xs, "", res:::List(temp+x))
        case x::xs if quote=> loop(quote, xs, temp+x, res)
        case x::xs if x.isWhitespace => loop(quote, xs, "", res:::List(temp))
        case x::xs => loop(quote, xs, temp+x, res)
      }
    }
    loop(false, source, "", List()).filterNot(_.isEmpty)
  }

  def parseDeclareFun(tokens : List[String], strV : Set[String], intV : Set[String]): (List[String], Set[String], Set[String]) ={
    tokens(5) match{
      case "Int" => (tokens.drop(7), strV, intV+tokens(2)) //( declare-fun x (  ) Int )
      case "String" => (tokens.drop(7), strV+tokens(2), intV) // ( declare-fun x (  ) String )
    }
  }

  def parseDeclareConst(tokens : List[String], strV : Set[String], intV : Set[String]): (List[String], Set[String], Set[String]) ={
    tokens(3) match{
      case "Int" => (tokens.drop(5), strV, intV+tokens(2)) //( declare-const x  Int )
      case "String" => (tokens.drop(5), strV+tokens(2), intV) // ( declare-const x  String )
    }
  }

  def parseAssert(tokens : List[String], strV : Set[String], intV : Set[String]): (List[String], ReturnBoolean) ={
    //( assert ... )
    val (temp, formula) = parseFormula(tokens.drop(2), strV, intV)
    (temp.drop(1), formula)
  }

  def parseFormula(tokens : List[String], strV : Set[String], intV : Set[String]): (List[String], ReturnBoolean) ={
    tokens(1) match {
      case "=" => { //( = ()  ()  )
        def intEq(queue : List[String], count : Int) : Boolean = {
          if(count == 0)
            false
          else{
            queue match {
              case x::xs if x.equals("(")=> intEq(xs, count+1)
              case x::xs if x.equals(")")=> intEq(xs, count-1)
              case x::_ if (x.head == '-' || x.head== '+' || x.head.isDigit) && (x.drop(1) forall Character.isDigit) => true
              case x::_ if x.equals("str.len") => true
              case x::_ if intV.contains(x) => true
              case x::_ if strV.contains(x) => false
              case x::_ if x.startsWith("str.") => false
              case _::xs => intEq(xs, count)
            }
          }
        }
        if(intEq(tokens.drop(2), 1)){ //( = 1  2  )
          val (temp1, int1) = parseInt(tokens.drop(2), strV, intV)
          val (temp2, int2) = parseInt(temp1, strV, intV)
          (temp2.drop(1), IntegerEquation(int1, int2, 1))
        }
        else{  //( =  x "aba" )
          val (temp1, str1) = parseString(tokens.drop(2), strV, intV)
          val (temp2, str2) = parseString(temp1, strV, intV)
          (str1, str2) match {
            case (x : StrV, _) =>(temp2.drop(1), WordEquation(x, str2))
            case (_ , y: StrV) =>(temp2.drop(1), WordEquation(y, str1))
          }
        }
      }
      case "<" | "<=" | ">" | ">=" =>{ //( < () () )
        val (temp1, int1) = parseInt(tokens.drop(2), strV, intV)
        val (temp2, int2) = parseInt(temp1, strV, intV)
        (temp2.drop(1), IntegerEquation(int1, int2, diseq(tokens(1))))
      }
      case "str.in.re" => { // ( str.in.re x (...) )
        val x = StrV(tokens(2))
        val (temp, re) = parseRegular(tokens.drop(3), strV, intV)
        (temp.drop(1), StrInRe(x, re, false))
      }
      case "not" => { // ( not () )
        val (temp, formula) = parseFormula(tokens.drop(2), strV, intV)
        (temp.drop(1), Negation(formula))
      }
      case "and" => {  // ( and () () )
        val (temp1, f1) = parseFormula(tokens.drop(2), strV, intV)
        val (temp2, f2) = parseFormula(temp1, strV, intV)
        (temp2.drop(1), Conjunction(f1, f2))
      }
      case "or" => { // ( or () () )
        val (temp1, f1) = parseFormula(tokens.drop(2), strV, intV)
        val (temp2, f2) = parseFormula(temp1, strV, intV)
        (temp2.drop(1), Disjunction(f1, f2))
      }
    }
  }

  def parseInt(tokens : List[String], strV : Set[String], intV : Set[String]): (List[String], ReturnInteger) ={
    //1
    if( (tokens(0).charAt(0) == '-' || tokens(0).charAt(0)=='+' || tokens(0).charAt(0).isDigit)
      && (tokens(0).drop(1) forall Character.isDigit) ) {
      (tokens.drop(1), IntC(tokens(0).toInt))
    }
    //a
    else if(!tokens(0).equals("(")){
      (tokens.drop(1), IntV(tokens(0)))
    }
    //( str.len  x )
    else if(tokens(1).equals("str.len")){
      (tokens.drop(4), StrLen(StrV(tokens(2))))
    }
    //( + 1 (str.len x ) )
    else {
      val operator = tokens(1)
      val (temp1, operand1) = parseInt(tokens.drop(2), strV, intV)
      val (temp2, operand2) = parseInt(temp1, strV, intV)
      (temp2.drop(1), Operation(operand1, operand2, operator))
    }
  }

  def parseString(tokens : List[String], strV : Set[String], intV : Set[String]): (List[String], ReturnString) ={
    if(!tokens(0).equals("(") && tokens(0).charAt(0)!='\"'){//x
      (tokens.drop(1), StrV(tokens(0)))
    }
    else if(!tokens(0).equals("(")){//"abc"
      (tokens.drop(1), StrConcat(List(Right(tokens(0).drop(1).dropRight(1)))))
    }
    else{
      tokens(1) match {
        case "str.at"=>{ // ( str.at  x  1 )
          (tokens.drop(5), StrAt(StrV(tokens(2)),tokens(3).toInt))
        }
        case "str.++"=>{ // ( str.++ x "abc" x )
          def loop(temp : List[String], res : List[Either[StrV,String]]): (List[String], List[Either[StrV,String]]) ={
            if(temp(0).equals(")"))
              (temp.drop(1),res)
            else if(temp(0).charAt(0)=='\"'){  //"abc"
              loop(temp.drop(1),res:::List(Right(temp(0).drop(1).dropRight(1))))
            }
            else{ // x
              loop(temp.drop(1),res:::List(Left(StrV(temp(0)))))
            }
          }
          val (tokens1, list) = loop(tokens.drop(2), List())
          (tokens1, StrConcat(list))
        }
        case "str.replace"=>{ //( str.replace x "pattern" "replacement" )
          (tokens.drop(6), StrReplace(StrV(tokens(2)), tokens(3).drop(1).dropRight(1), tokens(4).drop(1).dropRight(1)))
        }
        case "str.replaceall"=>{ //( str.replaceall x "pattern" "replacement" )
          (tokens.drop(6), StrReplaceAll(StrV(tokens(2)), tokens(3).drop(1).dropRight(1), tokens(4).drop(1).dropRight(1)))
        }
        case "str.reverse"=>{ //( str.reverse x )
          (tokens.drop(4), StrReverse(StrV(tokens(2))))
        }
        case "str.insert"=>{ // ( str.insert x 1 "abab" )
          (tokens.drop(6), StrInsert(StrV(tokens(2)), tokens(3).toInt, tokens(4).drop(1).dropRight(1)))
        }
        case "str.substr"=>{
          if(tokens(4).equals(")"))// ( str.substr x begin )
            (tokens.drop(5), StrSubstr(StrV(tokens(2)), tokens(3).toInt))
          else                     // ( str.substr x begin count )
            (tokens.drop(6), StrSubstrcount(StrV(tokens(2)), tokens(3).toInt, tokens(4).toInt))
        }
      }
    }
  }

  def parseRegular(tokens : List[String], strV : Set[String], intV : Set[String]): (List[String], ReturnRe) ={
    tokens(1) match {
      case "str.to.re"=> { //( str.to.re  "abc" )
        (tokens.drop(4), StrToRe(tokens(2).drop(1).dropRight(1)))
      }
      case "re.*" => { // ( re.* ( str.to.re "a" ) )
        val (temp, re) = parseRegular(tokens.drop(2), strV, intV)
        (temp.drop(1), ReStar(re))
      }
      case "re.+" =>{ // ( re.+ ( str.to.re "a" ) )
        val (temp, re) = parseRegular(tokens.drop(2), strV, intV)
        (temp.drop(1), ReConcat(ReStar(re), re))
      }
      case "re.++"=>{ // ( re.++ ( str.to.re "a" ) ( str.to.re "b" ) )
        val (temp1, re1) = parseRegular(tokens.drop(2), strV, intV)
        val (temp2, re2) = parseRegular(temp1, strV, intV)
        (temp2.drop(1), ReConcat(re1, re2))
      }
      case "re.union"=>{ // ( re.union ( str.to.re "a" ) ( str.to.re "b" ) )
        val (temp1, re1) = parseRegular(tokens.drop(2), strV, intV)
        val (temp2, re2) = parseRegular(temp1, strV, intV)
        (temp2.drop(1), ReUnion(re1, re2))
      }
      case "re.range"=>{// ( re.range "a" "b" )
        (tokens.drop(5), ReRange(tokens(2).charAt(1), tokens(3).charAt(1)))
      }
    }
  }

  def parseCheckSat(tokens : List[String]) : List[String] = tokens.drop(3) //( check-sat )

  def parseGetModel(tokens : List[String]) : List[String] = tokens.drop(3) //( get-model )

}
