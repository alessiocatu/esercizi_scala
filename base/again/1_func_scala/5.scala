import scala.io.Source
import scala.collection.immutable.StringOps

object fifth {
  def main(args:Array[String]) = {
    if(args.length > 0) kwic(args(0))
  }

  def kwic (file: String, res:List[Any]=Nil) : Unit = {
    Source.fromFile(file).getLines()
    .zipWithIndex
    .map({case (line, i) => generate_lines(i, line) })
    .flatten
    .toList
    .sortWith((t1, t2) => t1.substring(39) < t2.substring(39))
    .foreach(println(_))
  }

  def generate_lines(i: Int, line: String) = {
    for (elem <- line.split(' ') if elem(0).isUpper)
      yield f"${i+1}%4d ${line.substring(Math.max(0, line.indexOf(elem)-33), line.indexOf(elem))}%33s ${line.substring(line.indexOf(elem), Math.min(line.length, line.indexOf(elem) + 40))}%-40s"
  }
}
