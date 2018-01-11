/**
  * Created by ImanH on 1/8/2018.
  * Seyed Iman Hosseini Zavaraki
  * Github @ https://github.com/ImanHosseini
  * Wordpress @ https://imanhosseini.wordpress.com/
  */
package lambda
import util.parsing.combinator.syntactical.StandardTokenParsers
import util.parsing.combinator.ImplicitConversions


object Parser extends StandardTokenParsers  with ImplicitConversions {
    import AST._


    // API
    def parse(input:String): Either[String, Expr]=
        phrase(program) (new lexical.Scanner(input)) match{
            case Success(ast,_) => Right(ast)
            case e: NoSuccess => Left("parse error: "+e.msg)
        }

    // Impl.
    lexical.reserved ++= ("if then else Int Double Date Contract one give add scale truncate mkdate and scaleX TimeFunc END" split ' ')
  lexical.delimiters ++= ("\\ => + - * / ( ) () , . == = :: -> ;" split ' ')
  def TYPE = ("Int" | "Double" | "Date" | "Contract")

  // def oneFunc = (ident <~ ("=" ~ "one" )) ^^ { e => Contract(e,1.0,-1)}
  def scale = (("scale" ~ "(") ~> expr) ~ (","~>expr <~ ")") ^^ Scale
  def scalex = (("scaleX" ~ "(") ~> expr) ~ (","~>expr <~ ")") ^^ ScaleX
  def truncate = (("truncate" ~ "(") ~> expr) ~ (","~>expr <~ ")") ^^ Truncate
    type P[+T] = Parser[T]  // alias for brevity

  def tfdef = (ident <~ "::") <~ ("TimeFunc" ~ "(" ~ TYPE ~")") ~ ("->" ~ TYPE) ^^ {case a  => FuncSign(a,1)}
    def fdef = (ident <~ "::") ~  ("(" ~> repsep(TYPE,",") <~ ")") <~ ("->" ~ TYPE) ^^ {case a ~ b => FuncSign(a,b.length)}

    def program   =rep1sep(expr, ";") <~ opt(";")   ^^ Sequence
  // def program   =rep1(expr)  ^^ Sequence
    def expr: P[Expr] =     end | defDate|defDouble|defInt | defContract | cthen | and |  mkdate | truncate| scale |scalex| one | give  | tfdef | fdef   | assign | operations
  def one = "one" ~ "()" ^^ {case _ => One()}

 //   def lambda2 = ident ("=" ~> expr) ^^ Lambda
  //  def oneFunc = (ident <~ ( "=" ~ "one" ~ "()" )) ^^ Onefunc
  def give = "give" ~ "("~> expr <~")" ^^ Give
    def end = "END" ^^ {case _ => END()}

    def assign = ident ~ ("="~> expr) ^^ Assign
    def operations = infixOps
  def defContract = ident <~ "::" ~ "Contract"  ^^ DefContract
  def defInt = ident <~ "::" ~ "Int" ^^ DefInt
  def defDate = ident <~ "::" ~ "Date" ^^ DefInt
  def defDouble = ident <~ "::" ~ "Double" ^^ DefInt


  def and = (("and" ~ "(") ~> expr) ~ (","~>expr <~ ")") ^^ And
  def cthen = (("then" ~ "(") ~> expr) ~ (","~>expr <~ ")") ^^ Then
    def mkdate = ("mkdate"~"("~>expr) ~ (","~>expr<~")") ^^ Mkdate
    def infixOps= equality
    def equality = sum * ("==" ^^^ Equal)
    def sum = product * ("+" ^^^ Add | "-" ^^^ Sub)


  def thenCont = ("then" ~ "(" ~ expr~","~expr ~ ")")

    def product = postfixOps * ("*" ^^^ Mul | "/" ^^^ Div)
    def postfixOps= application
    def application = simpleExpr ~ rep(arglist) ^^ {case e~args => (e /: args)(Application)}
    def arglist = "(" ~> repsep(expr,",") <~ ")" | simpleExpr ^^ {List(_)}
  def doubleLit = (numericLit <~ ".") ~ opt(numericLit) ^^ {case a~b => a+"."+b.get.toString}
    def simpleExpr = doubleLit ^^ {x=>Lit(x.toDouble)} | (ident ^^ Var | numericLit ^^ {x=> Lit(x.toDouble)}) | stringLit ^^ Lit | "(" ~> expr <~ ")" | failure ("Expression Expected! ")

  def main(args: Array[String]): Unit = {
    println(parse("coo = one() ; "))
  }
}
