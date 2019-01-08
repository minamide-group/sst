package constraint.vars

case class SST_Var(id : Int, name : String){override def toString: String = "x"+id+"_"+name}