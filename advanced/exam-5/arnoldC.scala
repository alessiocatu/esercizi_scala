import scala.util.parsing.combinator.JavaTokenParsers
import scala.io.Source
import scala.collection.mutable.Map
import scala.collection.mutable.Stack

class arnoldCParser extends JavaTokenParsers{
  var map:Map[String, Int] = Map()
  var stack:Stack[Int] = Stack()

  def program = "IT'S SHOWTIME" ~> exec <~ "YOU HAVE BEEN TERMINATED" ^^ {_ => (map,stack)}
  def exec : Parser[Any] = rep(statement)
  def statement = print | declare | assign | condition | loop
  def print = "TALK TO THE HAND" ~> (number | variable) ^^ {println(_)}
  def declare = "HEY CHRISTMAS TREE" ~> ident ~ ("YOU SET US UP" ~> number) ^^
   {case name ~ value => map(name) = value}
  def assign = "GET TO THE CHOPPER" ~> ident <~ ("HERE IS MY INVITATION" ~> init) <~ (operations <~ "ENOUGH TALK") ^^
   {case name => map += (name -> stack.pop())}
  def loop = "STICK AROUND" ~> ident ~ block <~ "CHILL" ^^
   {case cond ~ operations => while (map(cond) != 0) parseAll(exec, operations)}
  def condition = "BECAUSE I'M GOING TO SAY PLEASE" ~> init ~ block ~ ("BULLSHIT" ~> block <~ "YOU HAVE NO RESPECT FOR LOGIC") ^^
   {case cond ~ block1 ~ block2 => if (cond != 0) parseAll(exec, block1) else parseAll(exec, block2)}

  def operations = op ~ rep(op)
  def op = (add | min | mult | div | eq | mg | or2 | and2) ^^ {case f => stack.push(f(stack.pop()))}
  def add = "GET UP" ~> (number | variable) ^^ {case n => (a:Int) => a+n}
  def min = "GET DOWN" ~> (number | variable) ^^ {case n => (a:Int) => a-n}
  def mult = "YOU'RE FIRED" ~> (number | variable) ^^ {case n => (a:Int) => a*n}
  def div = "HE HAD TO SPLIT" ~> (number | variable) ^^ {case n => (a:Int) => a/n}
  def eq = "ARE NOT YOU YOU ARE ME" ~> (number | variable) ^^ {case n => (a:Int) => if (a==n) 1 else 0}
  def mg = "LET OFF SOME STEAM BENNET" ~> (number | variable) ^^ {case n => (a:Int) => if (a>n) 1 else 0}
  def or2 = "CONSIDER THAT A DIVORCE" ~> (number | variable) ^^ {case n => (a:Int) => if(a==1 && n==1) 1 else a+n}
  def and2 = "KNOCK KNOCK" ~> (number | variable) ^^ {case n => (a:Int) => a*n}

  def block = """(?s)\[.*?\]""".r ^^ {s => s.substring(1,s.length-1)}
  def init = (number | variable) ^^ {case n => stack.push(n.toInt)}
  def number : Parser[Int] = wholeNumber ^^ {_.toInt}
  def variable : Parser[Int] = ident ^^ {case a => map(a)}
}

object arnoldCmd {
  def main(args:Array[String]) = {
    val src = Source.fromFile("exec1.arnoldC")
    val lines = src.mkString
    val p = new arnoldCParser
    p.parseAll(p.program, lines) match {
      case p.Success(r,_) => println(r)
      case other => println(other)
    }
  }
}
