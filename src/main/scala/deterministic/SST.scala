package deterministic

case class SST[Q, Σ, Γ, X](//state, input alphabet, output alphabet, variable
                           states: Set[Q],
                           s0: Q,
                           vars: Set[X],
                           δ: Map[(Q, Σ), Q],
                           η: Map[(Q, Σ), Map[X, List[Either[X, Γ]]]],
                           f: Map[Q, List[Either[X, Γ]]]
                          ) {

  /**
    * @param input
    * @return (isAccepted, finialState, outputSequence)
    */
  def process(input: Seq[Σ]): (Boolean, Q, Seq[Γ]) = {

    def eval(list: List[Either[X, Γ]], env: Map[X, Seq[Γ]]): Seq[Γ] = {
      def _eval(list: List[Either[X, Γ]], cur: Seq[Γ]): Seq[Γ] = {
        list match {
          case next :: rest =>
            next match {
              case Left(x) => _eval(rest, cur ++ env(x))
              case Right(gamma) => _eval(rest, cur :+ gamma)
            }
          case _ => cur
        }
      }

      _eval(list, Seq())
    }

    def _process(input: Seq[Σ])(q: Q)(env: Map[X, Seq[Γ]]): (Q, Map[X, Seq[Γ]]) = {
      input match {
        case Seq(c, cs@_*) => _process(cs)(δ(q, c))(η(q, c).map(x => (x._1, eval(x._2, env))))
        case _ => (q, env)
      }
    }

    val result = _process(input)(s0)(vars.map(x => (x, Seq())).toMap)
    //isAccepted, finialState, output Sequence
    (f.contains(result._1), result._1, eval(f(result._1), result._2))
  }


  /**
    * @param input : input sequence
    * @param q     : start state
    * @return output function X=> (X∪Γ)*
    */
  def trans(input: Seq[Σ])(q: Q): (Q, Map[X, List[Either[X, Γ]]]) = {

    def composite(m1: Map[X, List[Either[X, Γ]]], m2: Map[X, List[Either[X, Γ]]]): Map[X, List[Either[X, Γ]]] = {
      def _composite(m1: Map[X, List[Either[X, Γ]]], list: List[Either[X, Γ]], ret: List[Either[X, Γ]]): List[Either[X, Γ]] = {
        list match {
          case next :: rest =>
            next match {
              case Left(x) => _composite(m1, rest, ret ::: m1(x))
              case Right(gamma) => _composite(m1, rest, ret :+ Right(gamma))
            }
          case _ => ret
        }
      }

      vars.map(x => (x, _composite(m1, m2(x), List()))).toMap
    }

    def _trans(input: Seq[Σ])(q: Q)(m: Map[X, List[Either[X, Γ]]]): (Q, Map[X, List[Either[X, Γ]]]) = {
      input match {
        case Seq(c, cs@_*) => _trans(cs)(δ(q, c))(composite(m, η(q, c)))
        case _ => (q, m)
      }
    }

    val result = _trans(input)(q)(vars.map(x => (x, List(Left(x)))).toMap) //initially m is λx.x
    //finialState, output function
    (result._1, result._2)
  }


  /**
    * @return (Q, Q_0, F, delta1, delta2)
    */
  def toIntegerTransducer : (Set[Either[(Q,Set[X]), Int]],  //Q
                             Set[Either[(Q,Set[X]), Int]],  //Q0
                             Set[Either[(Q,Set[X]), Int]],  //F
                             Set[(Either[(Q,Set[X]), Int], Σ, Int, Either[(Q,Set[X]), Int])],  //delta
      )={

    def h(alpha: Map[X, List[Either[X, Γ]]]): (Map[X, Set[X]], Map[X, Int]) =
      (alpha.map(x => x._1 -> x._2.filter(x => x.isLeft).map(x=>x.left.get).toSet ), alpha.map(x => x._1 -> x._2.filter(x => x.isRight).size))

    //the only final state
    val q_bottom : Int = 0

    //rules to final state
    val delta2 : Set[((Q, Set[X]), Σ, Int, Int)] =  δ.map(x=> (x._1._1, x._1._2, x._2, h(η(x._1)))).filter(x=> f.contains(x._3)).map(x=>{
      val q : Q = x._1
      val beta_B: Set[X] = f(x._3).filter(y=>y.isLeft).map(y=>y.left.get).flatMap(y=> x._4._1(y)).toSet
      val gamma_B : Int = f(x._3).filter(y=>y.isLeft).map(y=>y.left.get).map(y=>x._4._2(y)).reduce(_+_)
      ( (q, beta_B), x._2, gamma_B + f(x._3).filter(y=>y.isRight).size, q_bottom )
    }).toSet

    def getStatesAndDelta1(queue: List[(Q, Set[X])],
                                    res_states: Set[(Q, Set[X])],
                                    res_delta : Set[((Q, Set[X]), Σ, Int, (Q, Set[X]))],
                                    delta : Set[(Q, Σ, Q, (Map[X, Set[X]], Map[X, Int]) )]
                                   ): (Set[(Q, Set[X])], Set[((Q, Set[X]), Σ, Int, (Q, Set[X]))]) ={
      queue match {
        case s:: rest =>
          val newRules = delta.filter(r=>r._3==s._1).map(r=>{
            val s0 = (r._1, s._2.flatMap(x=>r._4._1(x)))
            ((s0, r._2, s._2.foldLeft(0){(x,y)=> x + r._4._2(y)}, s))
          })
          getStatesAndDelta1(rest ++ newRules.map(x=>x._1).diff(res_states), res_states ++ newRules.map(x=>x._1), res_delta ++ newRules, delta)
        case Nil => (res_states, res_delta)
      }
    }

    //get other rules and state
    val (states, delta1) = getStatesAndDelta1(delta2.map(x=>x._1).toList,  delta2.map(x=>x._1),  Set[((Q, Set[X]), Σ, Int, (Q, Set[X]))](),  δ.map(x=> (x._1._1, x._1._2, x._2, h(η(x._1)))).toSet)

    ( states.map(x=>Left(x) : Either[(Q,Set[X]), Int]) + Right(q_bottom), states.filter(x=>x._1==s0).map(x=>Left(x) : Either[(Q,Set[X]), Int]), Set(Right(q_bottom)),  delta1.map(r=> (Left(r._1), r._2, r._3, Left(r._4))) ++ delta2.map(r=> (Left(r._1), r._2, r._3, Right(r._4))))
  }

  def toSemiLinearSet : Set[((Int,Int), Set[(Int,Int)])]={

    def concat(m1:Set[((Int,Int), Set[(Int,Int)])], m2:Set[((Int,Int), Set[(Int,Int)])]): Set[((Int,Int), Set[(Int,Int)])] = {
      (m1,m2) match {
        case (x,y) if x.isEmpty || y.isEmpty => Set.empty
        case _ => m1.flatMap(x=> m2.map(y=>((x._1._1+y._1._1, x._1._2+y._1._2), x._2++y._2 )  ))
      }
    }

    def union(m1:Set[((Int,Int), Set[(Int,Int)])], m2:Set[((Int,Int), Set[(Int,Int)])]): Set[((Int,Int), Set[(Int,Int)])] = m1++m2

    def star(m1:Set[((Int,Int), Set[(Int,Int)])]): Set[((Int,Int), Set[(Int,Int)])] ={
      def _star(list: List[((Int,Int), Set[(Int,Int)])], set: Set[((Int,Int), Set[(Int,Int)])]): Set[((Int,Int), Set[(Int,Int)])] ={
        list match {
          case x :: rest if x._2.isEmpty=> _star(rest, set)
          case x :: rest => _star(rest, set ++ set.map(y=>((x._1._1+y._1._1, x._1._2+y._1._2), x._2 ++ y._2) ))
          case Nil => set
        }
      }

      _star(m1.toList, Set( ((0,0), m1.map(x=>x._1)) ) )
    }

    def eliminate[Q](states : List[Q],
                     rules: Set[(Q, Set[((Int,Int), Set[(Int,Int)])], Q)]
                    ) : Set[(Q, Set[((Int,Int), Set[(Int,Int)])], Q)]={
      states match {
        case s :: rest=>
          val newRules = rules.filter(x=>x._3==s).flatMap(r1=>{
            rules.filter(x=>x._1==s).map(r2=>{
              val oldRex:Set[((Int,Int), Set[(Int,Int)])] = rules.filter(x=>x._1==r1._1).filter(x=>x._3==r2._3).map(x=>x._2).foldLeft(Set.empty : Set[((Int,Int), Set[(Int,Int)])]){(x,y)=>union(x,y)}
              val newRex:Set[((Int,Int), Set[(Int,Int)])] = concat(concat(r1._2, star(rules.filter(x=>x._1==s).filter(x=>x._3==s).map(x=>x._2).foldLeft(Set.empty : Set[((Int,Int), Set[(Int,Int)])]){(x,y)=>union(x,y)})), r2._2)
              (r1._1, union(oldRex, newRex),  r2._3)
            })
          })
          eliminate(rest, newRules.union(rules.filterNot(x=>x._1==s).filterNot(x=>x._3==s).filterNot(x=>newRules.map(r=>(r._1,r._3)).contains(x._1,x._3))))
        case Nil => rules
      }
    }

    val (states, initialStates, finialStates, delta) = toIntegerTransducer

    val res = initialStates.flatMap(q0 =>
      finialStates.map(qf =>{
        val rules = eliminate(states.filterNot(x=>x==q0).filterNot(x=>x==qf).toList, delta.map(x=>(x._1, Set(((1,x._3), Set.empty[(Int,Int)])) ,x._4)))

        val A= rules.filter(x=>x._1==q0).filter(x=>x._3==q0).map(x=>x._2).foldLeft(Set.empty: Set[((Int,Int), Set[(Int,Int)])]){(x,y)=>union(x,y)}
        val B= rules.filter(x=>x._1==q0).filter(x=>x._3==qf).map(x=>x._2).foldLeft(Set.empty: Set[((Int,Int), Set[(Int,Int)])]){(x,y)=>union(x,y)}
        val C= rules.filter(x=>x._1==qf).filter(x=>x._3==q0).map(x=>x._2).foldLeft(Set.empty: Set[((Int,Int), Set[(Int,Int)])]){(x,y)=>union(x,y)}
        val D= rules.filter(x=>x._1==qf).filter(x=>x._3==qf).map(x=>x._2).foldLeft(Set.empty: Set[((Int,Int), Set[(Int,Int)])]){(x,y)=>union(x,y)}
        concat(concat(star(union(A, concat(concat(B, star(D)), C))),B), star(D))

      }).foldLeft(Set.empty: Set[((Int,Int), Set[(Int,Int)])]){(x,y)=>union(x,y)}
    )

    f.contains(s0) match {
      case false => res
      case true => res ++ Set[((Int,Int), Set[(Int,Int)])]( ((0, f(s0).filter(x=>x.isRight).size), Set.empty) )
    }
  }
}
