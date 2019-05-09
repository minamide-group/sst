package builder

import formula.{Conjunction, Disjunction, Formula, Negation}
import formula.atomic.{IntegerEquation, StrInRe, WordEquation}
import formula.integer._
import formula.re._
import formula.str._

case class FormulaBuilder(lines : List[String]) {
  //assert valid

  def output : Formula = {

    def loop(tokens : List[String], p : Formula, strV : Set[String], intV : Set[String]) : (Formula, Set[String], Set[String])={
      if(tokens(1).equals("check-sat")){
        (p, strV, intV)
      }
      else if(tokens(1).startsWith("declare")){
        val (temp, strV1, intV1) = parseDeclare(tokens, strV, intV)
        loop(temp, p, strV1, intV1)
      }
      else /*if(tokens(1).equals("assert"))*/ {
        val (temp, formula) = parseAssert(tokens, strV, intV)
        if(p==null)
          loop(temp, formula, strV, intV)
        else
          loop(temp, Conjunction(p, formula), strV, intV)
      }
    }
    val (res, _, _) = loop(getTokens(lines), null, Set(), Set())
    res
  }

  val diseq = Map(
    "<" ->  2,
    ">="-> -2,
    ">" ->  3,
    "<="-> -3
  )

  def getTokens(lines : List[String]) : List[String] = {
    val source = lines.filterNot(_.isEmpty).filterNot(_.startsWith(";")).filterNot(_.toList.foldLeft(true) { (x, y) => x && (y.isWhitespace) })
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

  def parseDeclare(tokens : List[String], strV : Set[String], intV : Set[String]): (List[String], Set[String], Set[String]) ={

    if(tokens(1).equals("declare-fun") && tokens(5).equals("Int")){//( declare-fun x (  ) Int )
      (tokens.drop(7), strV, intV+tokens(2))
    }
    else if (tokens(1).equals("declare-fun") && tokens(5).equals("String")){
      (tokens.drop(7), strV+tokens(2), intV)
    }
    else if (tokens(1).equals("declare-const") && tokens(3).equals("Int")){//( declare-const x  Int )
      (tokens.drop(5), strV, intV+tokens(2))
    }
    else{
      (tokens.drop(5), strV+tokens(2), intV)
    }
  }

  def parseAssert(tokens : List[String], strV : Set[String], intV : Set[String]): (List[String], Formula) ={
    //( assert ... )
    val (temp, formula) = parseFormula(tokens.drop(2), strV, intV)
    (temp.drop(1), formula)
  }

  def parseFormula(tokens : List[String], strV : Set[String], intV : Set[String]): (List[String], Formula) ={

    if( tokens(1).equals("=") ) { //( = ()  ()  )
      def intEq(queue : List[String], count : Int) : Boolean = {
        if(count == 0)
          false
        else{
          queue match {
            case x::xs if x.equals("(")=> intEq(xs, count+1)
            case x::xs if x.equals(")")=> intEq(xs, count-1)
            case x::_ if x forall Character.isDigit => true
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
    else if( diseq.contains(tokens(1)) ){ //( < () () )
      val (temp1, int1) = parseInt(tokens.drop(2), strV, intV)
      val (temp2, int2) = parseInt(temp1, strV, intV)
      (temp2.drop(1), IntegerEquation(int1, int2, diseq(tokens(1))))
    }
    else if( tokens(1).equals("str.in.re") ){ // ( str.in.re x (...) )
      val x = StrV(tokens(2))
      val (temp, re) = parseRegular(tokens.drop(3), strV, intV)
      (temp.drop(1), StrInRe(x, re, false))
    }
    else if( tokens(1).equals("not") ){ // ( not () )
      val (temp, formula) = parseFormula(tokens.drop(2), strV, intV)
      (temp.drop(1), Negation(formula))
    }
    else if( tokens(1).equals("and") ){  // ( and () () )
      val (temp1, f1) = parseFormula(tokens.drop(2), strV, intV)
      val (temp2, f2) = parseFormula(temp1, strV, intV)
      (temp2.drop(1), Conjunction(f1, f2))
    }
    else /* if(tokens(1).equals("or")) */{ // ( or () () )
      val (temp1, f1) = parseFormula(tokens.drop(2), strV, intV)
      val (temp2, f2) = parseFormula(temp1, strV, intV)
      (temp2.drop(1), Disjunction(f1, f2))
    }
  }

  def parseInt(tokens : List[String], strV : Set[String], intV : Set[String]): (List[String], ReturnInteger) ={
    //1
    if(tokens(0) forall Character.isDigit) {
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
      (tokens.drop(0), StrConcat(List(Right(tokens(0).drop(1).dropRight(1)))))
    }
    else if(tokens(1).equals("str.at")){ // ( str.at  x  1 )
      (tokens.drop(5), StrAt(StrV(tokens(2)),tokens(3).toInt))
    }
    else if(tokens(1).equals("str.++")){ // ( str.++ x "abc" x )
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
    else if(tokens(1).equals("str.replace")){ //( str.replace x "pattern" "replacement" )
      (tokens.drop(6), StrReplace(StrV(tokens(2)), tokens(3).drop(1).dropRight(1), tokens(4).drop(1).dropRight(1)))
    }
    else if(tokens(1).equals("str.replaceall")){ //( str.replaceall x "pattern" "replacement" )
      (tokens.drop(6), StrReplaceAll(StrV(tokens(2)), tokens(3).drop(1).dropRight(1), tokens(4).drop(1).dropRight(1)))
    }
    else if(tokens(1).equals("str.reverse")){ //( str.reverse x )
      (tokens.drop(4), StrReverse(StrV(tokens(2))))
    }
    else if(tokens(1).equals("str.insert")){ // ( str.insert x 1 "abab" )
      (tokens.drop(6), StrInsert(StrV(tokens(2)), tokens(3).toInt, tokens(4).drop(1).dropRight(1)))
    }
    else if(tokens(1).equals("str.substr") && tokens(4).equals(")")){ // ( str.substr x begin )
      (tokens.drop(5), StrSubstr(StrV(tokens(2)), tokens(3).toInt))
    }
    else /*if(tokens(1).equals("str.substr"))*/{ // ( str.substr x begin count )
      (tokens.drop(6), StrSubstrcount(StrV(tokens(2)), tokens(3).toInt, tokens(4).toInt))
    }

  }

  def parseRegular(tokens : List[String], strV : Set[String], intV : Set[String]): (List[String], ReturnRe) ={
    if(tokens(1).equals("str.to.re")){ //( str.to.re  "abc" )
      (tokens.drop(4), StrToRe(tokens(2).drop(1).dropRight(1)))
    }
    else if(tokens(1).equals("re.*")) { // ( re.* ( str.to.re "a" ) )
      val (temp, re) = parseRegular(tokens.drop(2), strV, intV)
      (temp.drop(1), ReStar(re))
    }
    else if(tokens(1).equals("re.++")){ // ( re.++ ( str.to.re "a" ) ( str.to.re "b" ) )
      val (temp1, re1) = parseRegular(tokens.drop(2), strV, intV)
      val (temp2, re2) = parseRegular(temp1, strV, intV)
      (temp2.drop(1), ReConcat(re1, re2))
    }
    else /* if(tokens(1).equals("re.union")) */{ // ( re.union ( str.to.re "a" ) ( str.to.re "b" ) )
      val (temp1, re1) = parseRegular(tokens.drop(2), strV, intV)
      val (temp2, re2) = parseRegular(temp1, strV, intV)
      (temp2.drop(1), ReUnion(re1, re2))
    }
  }
}
