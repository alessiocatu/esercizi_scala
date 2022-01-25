import scala.util.parsing.combinator.JavaTokenParsers
import scala.io.Source

class CSVParser extends JavaTokenParsers {
  var size:List[Int] = List()

  def table = header ~ lines ^^ {case header ~ lines => (formatH(header),lines)}
  def header = """.*\n""".r ^^ {case hd => hd.dropRight(1).split(";").toList}
  def lines = rep(""".*\n""".r) ^^ {case lines => evaluatecl(lines); format(lines)}

  def evaluatecl(lines:List[String]) :Unit = {
    lines.foreach { case line =>
      var line_split = line.dropRight(1).split(";").toList
      if(size.length == 0){size = line_split.map(_.length)}
      else {
        size = line_split.map(_.length)
        .zipWithIndex.map {case (value,i) => if(value > size(i)) value else size(i)}
      }
    }
  }
  def format(lines:List[String]) :List[String] = {
    (lines.map { case line =>
      var line_split = line.dropRight(1).split(";").toList
      line_split.zipWithIndex.map {case (s,i) => f"| ${s+List.fill(size(i)-s.length)(" ").mkString}%s "}.mkString + "|"
    }) :+ (List.fill((size.foldRight(0)(_+_))+size.length*2+size.length+1)("-")).mkString
  }

  def formatH(lines:List[String]) :List[String] = {
    (for(i <- 0 to 2) yield i match {
      case 0 => List.fill((size.foldRight(0)(_+_))+size.length*2+size.length+1)("-").mkString
      case 1 => lines.zipWithIndex.map {case (s, i) => f"| ${s+List.fill(size(i)-s.length)(" ").mkString}%s "}.mkString + "|"
      case 2 => List.fill((size.foldRight(0)(_+_))+size.length*2+size.length+1)("-").mkString
    }).toList
  }
}

object CSVParserCLI {
  def main(args:Array[String]) = {
    //args.toList.foreach {
    List("book.csv", "languages.csv").foreach { case file =>
      val src = Source.fromFile(file)
      val lines = src.mkString

      val p = new CSVParser
      p.parseAll(p.table, lines) match {
        case p.Success(r,_) => r._1.foreach {println(_)}; r._2.foreach {println(_)}
        case other => println(other)
      }
      src.close
    }
  }
}
