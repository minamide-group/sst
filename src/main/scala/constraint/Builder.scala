package constraint

import constraint.regular.RegCons
import constraint.relational._
import constraint.vars._
import deterministic.boundedcopy.{Composition, SST}
import nondeterministic.NFA

case class Builder[Q, Σ](alphabets : Set[Σ],
                         split : Σ
                        ) {

  private def _constraintToSST0(cons : Concatenation): SST[SST_State, Σ, Σ, SST_Var] ={

    val num = cons.left.id+1

    val varSet : Set[SST_Var] = List.range(0, num-1).map(x=> SST_Var(x, num)).toSet

    val stateSet : Set[SST_State] =  List.range(0, num).map(x=> SST_State(x, num)).toSet

    val s0 : SST_State = SST_State(0, num)

    val delta : Map[(SST_State,Σ), SST_State] = List.range(0, num-1).map(i=>
      alphabets.map(c => (SST_State(i, num), c) -> SST_State(i, num))
    ).foldLeft(List.range(0, num-1).map(i=> (SST_State(i, num), split) -> SST_State(i+1, num))){(x,y)=> x++y}.toMap

    val eta :  Map[(SST_State,Σ), Map[SST_Var, List[Either[SST_Var,Σ]]]]= List.range(0, num-1).map(i=>
      alphabets.map(c =>
        (SST_State(i, num), c)->
          List.range(0, num-1).map(j=>
            if(j==i)
              SST_Var(j, num)-> List(Left(SST_Var(j, num)), Right(c))
            else
              SST_Var(j, num)-> List(Left(SST_Var(j, num)))
          ).toMap
        )
    ).foldLeft(
      List.range(0, num-1).map(i=>
        (SST_State(i, num), split)->
          List.range(0, num-1).map(j=>
            SST_Var(j, num)-> List[Either[SST_Var, Σ]](Left(SST_Var(j,  num)))
          ).toMap
      )
    ) {(x,y) => x++y}.toMap

    val f : Map[SST_State, List[Either[SST_Var, Σ]]]= Map(
      SST_State(num-1,num)-> (
          List.range(0, num-1).map(i => SST_Var(i, num)).foldLeft(List[Either[SST_Var, Σ]]()) {(x,y) => x ++ List(Left(y), Right(split))} ++
          List(Left(SST_Var(cons.right1.id, num)), Left(SST_Var(cons.right2.id, num)), Right(split))
        )
    )

    SST(stateSet, s0, varSet, delta, eta, f)
  }

  private def _constraintToSST1(cons : TransducerConstraint[TransState, Σ]) : SST[SST_State, Σ, Σ, SST_Var] = {

    val num = cons.left.id

    val idx = cons.right2.id

    val trans = cons.right1

    val transToSST : Map[TransState, SST_State] = trans.states.map(s => s->SST_State(s.id, -idx)).toMap

    val varSet : Set[SST_Var] = List.range(0, num).map(x=> SST_Var(x, num)).toSet

    val stateSet : Set[SST_State] = List.range(0, num).filter(i=> i!=idx).map(x=> SST_State(x, num)).toSet ++ trans.states.map(s => transToSST(s))

    val s0 : SST_State = if(idx==0) transToSST(trans.initialStates) else SST_State(0, num)

    val delta0 :  Map[(SST_State,Σ), SST_State] =
      List.range(0, num-1).filter(i => i!=idx).map(i=>
        alphabets.map(c => (SST_State(i, num), c) -> SST_State(i, num))
      ).foldLeft(
        List.range(0, num-1).filter(i => i!=idx).filter(i => i!=idx-1).map(i=> (SST_State(i, num), split) -> SST_State(i+1, num) )
      ){(x,y)=>x++y}.toMap//original

    val delta1 :  Map[(SST_State,Σ), SST_State] = trans.δ.map(r=> (transToSST(r._1._1), r._1._2) -> transToSST(r._2))

    val delta2 : Map[(SST_State,Σ), SST_State] = trans.F.map(s => (transToSST(s), split) -> SST_State(idx+1, num)).toMap

    val delta3 : Map[(SST_State,Σ), SST_State] = if(idx==0) Map()
      else Map((SST_State(idx-1, num), split) -> transToSST(trans.initialStates))

    val delta : Map[(SST_State,Σ), SST_State] = delta0 ++ delta1 ++ delta2 ++ delta3

    val eta0 : Map[(SST_State,Σ), Map[SST_Var, List[Either[SST_Var,Σ]]]] =
      List.range(0, num-1).filter(i=>i!=idx).map(i=>
        alphabets.map(c =>
          (SST_State(i, num), c)->
            List.range(0, num).map(j=>
              if(j==i)
                SST_Var(j, num)-> List(Left(SST_Var(j, num)), Right(c))
              else
                SST_Var(j, num)-> List(Left(SST_Var(j, num)))
            ).toMap
        )
      ).foldLeft(
        List.range(0, num-1).filter(i=>i!=idx).filter(i=>i!=idx-1).map(i=>
          (SST_State(i, num), split)->
            List.range(0, num).map(j=>
              SST_Var(j, num)-> List[Either[SST_Var, Σ]](Left(SST_Var(j,  num)))
            ).toMap
        )
      ) {(x,y) => x++y}.toMap

    val eta1 : Map[(SST_State,Σ), Map[SST_Var, List[Either[SST_Var,Σ]]]] =
      trans.η.map(r=>
        (transToSST(r._1._1), r._1._2)->
        List.range(0, num).map(j=>
          if(j==idx)
            SST_Var(j, num)-> List(Left(SST_Var(j, num)), Right(r._1._2))
          else if(j == num-1)
            SST_Var(j, num)-> (List(Left(SST_Var(j, num))) ::: r._2.map(c=>Right(c)))
          else
            SST_Var(j, num)-> List(Left(SST_Var(j, num)))
        ).toMap
      )

    val eta2 : Map[(SST_State,Σ), Map[SST_Var, List[Either[SST_Var,Σ]]]] =
      trans.F.map(s=> (transToSST(s), split) ->
        List.range(0, num).map(i=>
          SST_Var(i, num) -> List[Either[SST_Var, Σ]](Left(SST_Var(i, num)))
        ).toMap
      ).toMap

    val eta3 : Map[(SST_State,Σ), Map[SST_Var, List[Either[SST_Var,Σ]]]] =
      if(idx==0) Map()
      else Map((SST_State(idx-1, num), split)-> List.range(0, num).map(i=> SST_Var(i, num) -> List[Either[SST_Var, Σ]](Left(SST_Var(i, num)))).toMap)

    val eta : Map[(SST_State,Σ), Map[SST_Var, List[Either[SST_Var,Σ]]]] = eta0 ++ eta1 ++ eta2 ++ eta3

    val f : Map[SST_State, List[Either[SST_Var, Σ]]]= Map(SST_State(num-1,num)->
      List.range(0, num).map(i => SST_Var(i, num)).foldLeft(List[Either[SST_Var, Σ]]()) {(x,y) => x ++ List(Left(y), Right(split))})

    SST(stateSet, s0, varSet, delta, eta, f)
  }

  def constraintToSST(cons : RelCons): SST[SST_State, Σ, Σ, SST_Var] = {
    cons match {
      case c : Concatenation=> _constraintToSST0(c)
      case t : TransducerConstraint[TransState, Σ] => _constraintToSST1(t)
    }
  }

  //num : number of String variables
  def regularToSST(num : Int, consMap : Map[StringVariable, NFA[NFAState, Σ]]) : SST[SST_State, Σ, Σ, SST_Var]={

    val states : List[SST_State]= List.range(0, num+1).map(i => SST_State(i, num+1))

    val toSSTStates : Map[NFAState, SST_State] = consMap.flatMap(r=>{
      r._2.states.map(s => s-> SST_State(s.id, -r._1.id))
    })

    val statesToInit : Map[SST_State, SST_State]= states.map(s=>
      if(consMap.contains(StringVariable(s.id0))) s-> toSSTStates(consMap(StringVariable(s.id0)).s0)
      else s->s
    ).toMap

    val statesToFinal : Map[SST_State, Set[SST_State]]= states.map(s=>
      if(consMap.contains(StringVariable(s.id0))) s-> consMap(StringVariable(s.id0)).f.map(s => toSSTStates(s))
      else s->Set(s)
    ).toMap

    val stateSet : Set[SST_State] = states.flatMap(s=>
      if(consMap.contains(StringVariable(s.id0))) consMap(StringVariable(s.id0)).states.map(q => toSSTStates(q))
      else Set(s)
    ).toSet

    val s0 : SST_State = statesToInit(states.head)

    val varSet : Set[SST_Var] = List.range(0, num).map(i => SST_Var(i, num+1)).toSet

    val f : Map[SST_State, List[Either[SST_Var, Σ]]]=Map(
      SST_State(num, num+1) -> List.range(0, num).map(i => SST_Var(i, num+1))
        .foldLeft(List[Either[SST_Var,Σ ]]()){(x,y)=> x ::: List(Left(y), Right(split))}
    )

    val delta0 : Map[(SST_State,Σ), SST_State] = List.range(0, num).map(i => SST_State(i, num+1)).flatMap(s=>
      if(consMap.contains(StringVariable(s.id0)))
        consMap(StringVariable(s.id0)).σ.flatMap(r=> r._2.map(s2=> (r._1._1, r._1._2, s2))).map(r=>
          (toSSTStates(r._1), r._2)->toSSTStates(r._3) )
      else
        alphabets.map(c=> ((s,c)->s))
    ).toMap

    val delta1 = List.range(0, num).flatMap(i =>
      statesToFinal(SST_State(i, num+1)).map(s=> (s, split)-> statesToInit(SST_State(i+1, num+1))).toMap)

    val delta : Map[(SST_State,Σ), SST_State] = delta0 ++ delta1

    val eta0 : Map[(SST_State,Σ), Map[SST_Var, List[Either[SST_Var,Σ]]]] = List.range(0, num).map(i => SST_State(i, num+1)).flatMap(s=>
      if(consMap.contains(StringVariable(s.id0)))
        consMap(StringVariable(s.id0)).σ.flatMap(r=> r._2.map(s2=> (r._1._1, r._1._2, s2))).map(r=>
          (toSSTStates(r._1), r._2)-> varSet.map(x=> if(x.id0 == s.id0) x-> List(Left(x), Right(r._2)) else x-> List(Left(x))).toMap)
      else alphabets.map(c => (s, c) -> varSet.map(x => if (x.id0 == s.id0) x->List(Left(x), Right(c)) else x->List(Left(x))).toMap)
    ).toMap

    val eta1 : Map[(SST_State,Σ), Map[SST_Var, List[Either[SST_Var,Σ]]]] = List.range(0, num).flatMap(i =>
      statesToFinal(SST_State(i, num+1)).map(s=>
        (s, split)->varSet.map(x=> x->List[Either[SST_Var, Σ]](Left(x))).toMap
      )).toMap

    val eta : Map[(SST_State,Σ), Map[SST_Var, List[Either[SST_Var,Σ]]]] = eta0 ++ eta1

    SST(stateSet, s0, varSet, delta, eta, f)
  }


  def constraintsToSST(list: List[RelCons], set : Set[RegCons[NFAState, Σ]]): Any={

    val sstList : List[SST[SST_State, Σ, Σ, SST_Var]] =
      list.map(cons => constraintToSST(cons)) ::: List(regularToSST(list.last.getLeftIdx()+1, set.map(cons=>cons.x->cons.R).toMap))

    sstList match{
      case s::Nil => s
      case s::t::Nil => Composition.compose(s,t)
      case s::t::rest => _compose(Composition.compose(s,t), rest)
    }

    def _compose[Q1, A1, B1, X1](sst : SST[Q1,A1,B1,X1], sstList : List[SST[SST_State, Σ, Σ, SST_Var]]): Any ={
      sstList match {
        case Nil => sst
        case sst2 :: rest => _compose(Composition.compose(sst,sst2), rest)
      }
    }
  }
}
