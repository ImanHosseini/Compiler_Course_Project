package lambda

/**
  * Created by ImanH on 1/8/2018.
  * Seyed Iman Hosseini Zavaraki
  * Github @ https://github.com/ImanHosseini
  * Wordpress @ https://imanhosseini.wordpress.com/
  */
object AST {
    sealed abstract trait Expr
    sealed abstract trait Cont
    case class Sequence(l: List[Expr]) extends Expr{
      override def toString=l.mkString("\n")
    }

    case class Lambda(params: List[String] ,body: Expr) extends Expr{
      override def toString="Lambda(Params("+params.mkString(". ")+"). Body("+body+"))"
    }

  case class Lambda2(params: List[String] ,body: Expr) extends Expr{
    override def toString="Lambda(Params("+params.mkString(". ")+"). Body("+body+"))"
  }


  //  def LambdaMaker(body: Expr): Lambda = {
//    var argz = List[String]()
//    for (i<-0 until env)
//    return Lambda(argz,body)
//  }
  case class DefContract(name:String) extends Expr
  case class DefInt(name:String) extends Expr
  case class END() extends Expr
  case class Give(c1: Expr) extends Expr
  case class Scale(e: Expr,c: Expr) extends Expr
  case class Truncate(e: Expr,c: Expr) extends Expr
  case class Mkdate(day:Expr,hour:Expr) extends Expr
  case class FuncSign(fname: String,num: Int) extends Expr
  case class Contract(money: Double,date: Int) extends Expr with Cont
  case class ContractVar(func: Expr,date: Int) extends Expr with Cont
  case class AddContract(c1: Expr, c2:Expr) extends Expr
  case class IfExpr(e1:Expr, e2: Expr, e3: Expr) extends Expr
  case class Assign(name:String, expr: Expr) extends Expr
  case class Equal(e1: Expr,e2: Expr) extends Expr
  case class Add(e1: Expr, e2:Expr) extends Expr
  case class ScaleX(e1: Expr,e2:Expr) extends Expr

  case class One() extends Expr
  case class And(c1:Expr,c2:Expr) extends Expr
  case class Then(c1:Expr,c2:Expr) extends Expr
  case class Sub(e1: Expr, e2:Expr) extends Expr
  case class Mul(e1:Expr,e2:Expr) extends Expr
  case class Div(e1:Expr,e2: Expr) extends Expr
  case class ContractDef(fname: String) extends Expr
  case class Application(func: Expr, args: List[Expr]) extends Expr{
    override def toString="App(Func("+func+"). Args("+args.mkString(", ")+"))"}
  case class Var(name: String) extends Expr
  case class Lit(v: Any) extends Expr{
    override def toString=if(v.isInstanceOf[String]) "\""+v+"\"" else v.toString
  }
}
