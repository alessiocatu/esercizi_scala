import scala.util.parsing.combinator.JavaTokenParsers
import scala.io.Source
import scala.collection.mutable.Map
import scala.collection.mutable.Stack

class WTFParser extends JavaTokenParsers {
  var head:Map[Char, (Int, String)] = Map()
  var stack:Stack[Int] = Stack()
  var args:List[Int] = List()

  def program = statements ^^ {case _ => ("Fine")}
  def statements = rep(statement)
  def statement : Parser[Any] = funs | eval
  def funs = fun ~ rep(fun)
  def fun = "def" ~> """[A-Z]""".r ~ number ~ ("=" ~> """.*\n""".r) ^^
    {case name ~ nargs ~ body => head += (name.charAt(0)) -> (nargs,body.dropRight(1))}

  def eval = call ~ rep(call)
  def call = operator | print | funcall | getValue | ternario
  def operator = "0" ~> (min | add)
  def add = rep("+") ^^ {case n => stack.push(n.size)}
  def min = rep1("-") ^^ {case n => stack.push(0-n.size)}
  def print = "!" ^^ {case _ => println(stack.pop())}
  def funcall = """[A-Z]""".r ^^
  {case f =>
    if (head.contains(f.charAt(0))) {
      var fun = head(f.charAt(0))
      for (i <- 1 to fun._1) {
        args = args :+ stack.pop()
      }
      parseAll(statements,fun._2)
      args = List()
    }
  }
  def ternario = "?" ~> block ~ (":" ~> block) ^^
    {case block1 ~ block2 => if (stack.pop()==0) parseAll(statement, block1) else parseAll(statement, block2)}
  def getValue = "$" ~> number ^^ {case n => stack.push(args(n-1))}
  def block = """(?s)\[.*?\]""".r ^^ {case b => b.substring(1,b.length-1)}
  def number = wholeNumber ^^ {_.toInt}
}

object WTFexe {
  def main(args:Array[String]) = {
    val src = Source.fromFile("1.wtf")
    val lines = src.mkString
    val p = new WTFParser
    p.parseAll(p.program, lines) match {
      case p.Success(r,_) => println(r)
      case other => println(other)
    }

    src.close
  }
}
