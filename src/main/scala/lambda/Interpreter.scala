package lambda

/**
  * Created by ImanH on 1/8/2018.
  * Seyed Iman Hosseini Zavaraki
  * Github @ https://github.com/ImanHosseini
  * Wordpress @ https://imanhosseini.wordpress.com/
  */

object Interpreter {
  import AST._

  var time = 0
  var hashcounter=0
  var contracts: List[String] = List[String]("c4")

  def init(time: Int,contracts: List[String]): Unit ={
    this.time=time
    this.contracts=contracts
  }
    // API
  def eval(ast: Expr): Unit={
    (new Context) eval ast
  }

    // Impl.

    // Type aliases
    type VarName = String
  type VarValue = Any
  type Environment = collection.immutable.Map[VarName,VarValue]

  // Runtime types
  sealed abstract class Func extends (List[Any]=> Any)

  case object Println extends Func{
    override def toString = "println (built-in)"
    def apply(args: List[Any]) = println(args mkString(". "))
  }


  /* TODO

  CHANGE THE LAMBDA TO ONLY STORE EXPR | THEN CHANGE TO CLOSURE TO USE Foo_sign TO MAP THE ARGS

   TODO */


  case class Closure(env: Environment, lam: Lambda) extends Func {
    override def toString = "Closure(Env("+env.mkString(".")+"). Params(" + lam.params.mkString(".")+").Body(" + lam.body+ "))"
    //println("IM HERE ")
    def apply(args: List[Any]) = {
      (new Context(env ++ lam.params.zip(args))) eval lam.body

    }
  }





  // Eval

  val initEnv: Environment=Map("println" -> Println)
  class Context( var env: Environment=initEnv) {
    override def toString = "Environment: "+env.mkString(", ")
    def eval(e: Expr): Any = e match{
      case Sequence(exprs) => exprs foreach eval
      case lam: Lambda => Closure (env,lam)
      // case lam2: Lambda2 => Closure2 (env,lam2)
      case IfExpr(e1,e2,e3) => eval(e1) match{
        case b:Boolean => eval(if(b) e2 else e3)
        case _=> println("error")
      }
      case Assign(id,expr) =>
        {
        if (env.contains(id+"_sign")){

          // Closure2(env,id)
          // var argnames = List[String]()
          val strn = id+"_sign"
         // println(env.get(strn).get)
          val cnt = env.get(strn).get.toString.toInt
          val argnames = List.tabulate(cnt)(i => ("arg"+(i+1).toString))

         // println(argnames)
          val lam = Lambda(argnames,expr)
          eval(lam)
         // println(lam)

          env += (id -> Closure(env,lam))
         //Closure(env,lam)
        }else{
        //  println(id +" "+expr)
          env += (id -> eval(expr))
        //  println(env)
        }

      }
      case FuncSign(id,x) => env += (id+"_sign" -> x)

    //  case Contract(name,money,date) => env += (name->("*"+money+"*"+date))

       // println(env+" FROM")

      case DefContract(name) => env+=(name->"Contract")
      case DefInt(name) => env+=(name->"Int")
      case Equal(e1,e2) => eval(e1) == eval(e2)
      case Add(e1,e2) => (eval(e1),eval(e2)) match{
        case (v1:String, v2) => v1+v2.toString
        case (v1,v2:String) => v1.toString +v2
        case (i1: Double, i2: Double) => i1+i2
        case (i1:Int,i2:Int) => i1+i2
        case _ => println("error Add "+eval(e1)+"**"+eval(e2))
      }
      case Sub(e1,e2) => (eval(e1),eval(e2)) match {
        case (i1:Double,i2:Double) => i1-i2
        case (i1:Int,i2:Int) => i1-i2
        case _ => println("error Sub "+e1+"***"+e2)
      }
      case Mul(e1,e2) => (eval(e1),eval(e2)) match{
        case (i1:Double,i2:Double) => i1*i2
        case (i1:Int,i2:Int) => i1*i2
        case _ => println("error Mul "+e1+"***"+e2)
      }
      case Div(e1,e2) => (eval(e1),eval(e2)) match{
        case (i1:Double,i2:Double) => i1/i2
          case(i1:Int,i2:Int) => i1/i2
        case _ => println("error Div "+e1+"***"+e2)
      }
      case Give(e1) => eval(e1) match{
        case e1: Contract => Contract(-e1.money,e1.date)
        case e1:ContractVar => {
          val cnt = 1
          val argnames = List.tabulate(cnt)(i => ("arg"+(i+1).toString))
          var newbody = e1.func
          newbody = Mul(newbody,Lit(-1.0))

          ContractVar(newbody,e1.date)
        }
        case x => println("ERROR IN GIVE "+x)
        }

      case Scale(ex,c) => (eval(ex),eval(c)) match {
        case (exx,c:Contract) => Contract(c.money*exx.toString.toDouble,c.date)
        case (exx,c:ContractVar) => {
          val cnt = 1
          val argnames = List.tabulate(cnt)(i => ("arg"+(i+1).toString))
          var newbody = c.func
          newbody = Mul(newbody,ex)

          ContractVar(newbody,c.date)
        }
      }
      case ScaleX(ex,c) => (eval(ex),eval(c)) match{
        case (f: Closure,c:Contract) =>{
          val cnt = 1
          val argnames = List.tabulate(cnt)(i => ("arg"+(i+1).toString))
          var newbody = f.lam.body
          // println("***"+newbody+"***")
          newbody = Mul(newbody,Lit(c.money))
         // println(argnames)
          //val lam = Lambda(argnames,newbody)
          //eval(lam)
          //println(lam)
          //val nname = "_"+hashcounter.toString
          //hashcounter=hashcounter+1
         // val newcloz = Closure(env,lam)
          //env += (nname -> newcloz
          ContractVar(newbody,c.date)
        }
        case (f: Closure,c:ContractVar) =>{
          val cnt = 1
          val argnames = List.tabulate(cnt)(i => ("arg"+(i+1).toString))
          var newbody = f.lam.body
          newbody = Mul(newbody,c.func)

          ContractVar(newbody,c.date)
        }
      }
        // TODO fix for when the money is time funcky!
      case And(c1,c2) => (eval(c1),eval(c2)) match {
        case (c1:Contract,c2:Contract) => Contract(c1.money+c2.money,math.max(c1.date,c2.date))
        case (c1:Contract,c2:ContractVar) =>{
          val cnt = 1
          val argnames = List.tabulate(cnt)(i => ("arg"+(i+1).toString))
          var newbody = c2.func
          newbody = Mul(newbody,Lit(c1.money))

          ContractVar(newbody,math.max(c1.date,c2.date))
        }
        case (c1:ContractVar,c2:Contract) =>{
          val cnt = 1
          val argnames = List.tabulate(cnt)(i => ("arg"+(i+1).toString))
          var newbody = c1.func
          newbody = Mul(newbody,Lit(c2.money))

          ContractVar(newbody,math.max(c1.date,c2.date))
        }
        case (c1:ContractVar,c2:ContractVar) =>{
          val cnt = 1
          val argnames = List.tabulate(cnt)(i => ("arg"+(i+1).toString))
          var newbody = c2.func
          newbody = Mul(newbody,c1.func)

          ContractVar(newbody,math.max(c1.date,c2.date))
        }
      }

      case Then(c1,c2) => (eval(c1),eval(c2)) match {
        case (c1:Contract,c2:Contract) => {
          if(time>c1.date) Contract(c1.money,c1.date)
          else Contract(c2.money,c2.date)
        }
          case(c1:ContractVar,c2:ContractVar) => {
            if(time>c1.date) ContractVar(c1.func,c1.date)
            else ContractVar(c2.func,c2.date)
      }
        case(c1:Contract,c2:ContractVar) => {
          if(time>c1.date) Contract(c1.money,c1.date)
          else ContractVar(c2.func,c2.date)
        }
      case(c1:ContractVar,c2:Contract) => {
        if(time>c1.date) ContractVar(c1.func,c1.date)
        else Contract(c2.money,c2.date)
      }
    }
      case Truncate(ex,c) => (eval(ex),eval(c)) match {
        case (exx:Int,c:Contract) => Contract(c.money,exx)
          case(exx:Int,c:ContractVar) => ContractVar(c.func,exx)
      }
      case Application(expr,args) => eval(expr) match {
        case f: Func => f(args map eval)
        case x => {
          println(x)
          println(env)
          println("error App "+ x)
        }
      }

      case Mkdate(e1,e2) => (eval(e1),eval(e2)) match {
        case (e1:Int,e2:Int) => e1*24+e2
      }

      case One() => Contract(1.0,8760)

      case Var(id) => env getOrElse(id,println("error VAR "+id))
      case Lit(v) => v
      case ContractDef(f) => env+=(f->"Contract")

      case END() =>{
        var sum = 0.0
        for (i<-0 until contracts.length){
          var c = env.get(contracts(i)).get match {
            case (c:Contract) =>
              if(c.date>time && c.money>0) {
                sum+=c.money
              //  println(sum)
              }
          case (c:ContractVar) =>
              if(c.date>time){
                var max=0.0
                val cnt = 1
                val argnames = List.tabulate(cnt)(i => ("arg"+(i+1).toString))
              //  println("ARG "+argnames)
                val lam = Lambda(argnames,c.func)
                val cloz = Closure(env,lam)
                // println(eval(Application(lam,List(Lit(1.0)))))




               // cloz(List(1))
                for (i<-time until c.date){
                val mon = eval(Application(lam,List(Lit(i.toDouble)))).toString.toDouble
                //   println("***"+mon)
                 if(mon > max) max= mon
                }
                sum+=max
              }
          }

        }
        println(s"#include<iostream> \n using namespace std; \n int main() {\ncout << $sum;\nreturn 0;}")
      }
    }
  }

}
