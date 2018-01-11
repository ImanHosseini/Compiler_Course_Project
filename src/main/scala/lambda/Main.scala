package lambda
import scala.util.control._

/**
  * Created by ImanH on 1/8/2018.
  * Seyed Iman Hosseini Zavaraki
  * Github @ https://github.com/ImanHosseini
  * Wordpress @ https://imanhosseini.wordpress.com/
  */
object Main extends App{
  val loop = new Breaks
    var ok = true
    var lines = Array[String]()
    while (ok) {
      val ln = scala.io.StdIn.readLine()
      ok = ln != null
      if (ok) {
        lines:+=ln
      }
    }
    var contracts= List[String]()
    var time=0
    val builder = StringBuilder.newBuilder
  var lasti = 0
    loop.breakable{
      for (i <- 0 until lines.length) {
        builder.append(lines(i))
        builder.append(";")
             if(lines(i)=="END") {
               lasti=i
               loop.break
             }
      }
    }
    val input = builder.toString;
    val n = lines(lasti+1).split(" ")(0).toInt
  val t = lines(lasti+1).split(" ")(1).toInt
    time=t
  for(i<-0 until n.toInt){
    contracts :+= lines(lasti+2+i)
  }
  val interp = Interpreter
  interp.init(time,contracts)
  val imanParser = Parser
  val astR = imanParser.parse(input)
    astR match {
      case Right(ast) => {
        (interp eval ast)

      }
    }


//  val imanParser = Parser
//  val astR = imanParser.parse(" c4 :: Contract; f:: TimeFunc(Date) ->Double;  f=arg1*0.1; println(f(2)); c4=scaleX(f ,one());  END ")
//  val interp = Interpreter
//  astR match {
//    case Right(ast) => {
//      println(interp eval ast)
//
//    }
//  }


  def process(input:String): Unit ={
    println("\nInput:\n" + input)
    (Parser parse input) match{
      case Right(ast) =>
        println("\nTREE:\n"+ast)
        println("\nEVAL:"); Interpreter eval ast
      case Left(errMsg)=>
        println(errMsg)
    }
    println("=========")
  }

}
