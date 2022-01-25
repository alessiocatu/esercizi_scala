import scala.util.parsing.combinator.JavaTokenParsers
import scala.io.Source

class arithParser extends JavaTokenParsers {
  var maxsize = 0
  def calc : Parser[Int] = num ~ rep( add | del | end ) ^^ {
    case a ~ commands => commands.reverse.foldRight(a) {
      case (("+", b),y) => y+b
      case (("-", b),y) => y-b
      case (("=", b),y) => if (y==b) y else 0
      case (_,y) => println("error"); y
    }
  }
  def add : Parser[(String,Int)] = "+" ~> num ^^ {case num => ("+", num)}
  def del : Parser[(String,Int)] = "-" ~> num ^^ {case num => ("-", num)}
  def end : Parser[(String,Int)] = "=" ~> line ~> num ^^ {case num => ("=", num)}
  def line: Parser[Any] = repN(maxsize, "-")
  def num : Parser[Int] = wholeNumber ^^ {
    case n => n.toInt
  }
}

object airth {
  def main(args:Array[String]) = {
    List("1.txt","2.txt","3.txt").foreach { file =>
      val src = Source.fromFile(file)
      val lines = src.mkString

      val p = new arithParser
      p.maxsize = lines.split("\n").head.length
      p.parseAll(p.calc, lines) match {
        case p.Success(result, _) => println(result)
        case other => println(other)
      }

      src.close()
    }
  }
}
