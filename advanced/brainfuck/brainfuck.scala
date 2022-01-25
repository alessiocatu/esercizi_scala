import scala.util.parsing.combinator.JavaTokenParsers

class BrainfuckParser extends JavaTokenParsers {
  def program = statementList
  def statementList = rep(statemant)
  def statemant = ">" | "<" | "+" | "-" | "." | "," | "[" | "]" | spurious
  def spurious = stringLiteral
}

object Brainfuck {
  def main(args:Array[String]) = {
    val b = new BrainfuckParser

    b.parseAll(b.program, args(0)) match {
      case  p.Success(r,_) => println(r)
      case _ => println("other")
    }
  }
}
