trait debug {

  val print = (method:String, line:String, pos:Int) =>
    if(line.length > 0) {
      println(f"${method}%-4s:- ${line.substring(0,pos)}[${line.charAt(pos)}]${line.substring(pos+1)}")
    } else
      println("<- EMPTY ->")
}

trait UndoRedo {
  var cmd:List[(String,Any)] = Nil

  def x = {cmd = cmd :+ ("x",0)}
  def dw = {cmd = cmd :+ ("dw",0)}
  def i (c:Char) = {cmd = cmd :+ ("i",c)}
  def iw (w:String) = {cmd = cmd :+ ("iw",w)}
  def l (i:Int=1) = {cmd = cmd :+ ("l",i)}
  def h (i:Int=1) = {cmd = cmd :+ ("h",i)}

  def u = {cmd = cmd :+ ("u",0)}
  def ctrlr {cmd = cmd :+ ("ctrlr",0)}

  def getLast = {
    var (s,i) = cmd.last
    cmd = cmd.dropRight(1)
    (s,i)
  }
}

object editor {
  def main (args:Array[String]) = {
    println()
    println()
    var e = new editor("Tells whether or not this string matches the given regular expression.")
    exe(scala.io.StdIn.readLine(":"),e)

    println()
    println()
  }

  def exe (in:String, e:editor) : Unit = {
    in match {
      case s if s.startsWith("x") => e.x; exe (scala.io.StdIn.readLine(":"), e)
      case s if s.startsWith("dw") => e.dw; exe (scala.io.StdIn.readLine(":"), e)
      case s if s.startsWith("i") && s.length > 1 => e.i(in.charAt(2)); exe (scala.io.StdIn.readLine(":"), e)
      case s if s.startsWith("iw") && s.length > 3 => e.iw(in.substring(3)); exe (scala.io.StdIn.readLine(":"), e)
      case s if s.startsWith("l") => if(in.length > 1) e.l(in.substring(2).toInt) else e.l(); exe (scala.io.StdIn.readLine(":"), e)
      case s if s.startsWith("h") => if(in.length > 1) e.h(in.substring(2).toInt) else e.h(); exe (scala.io.StdIn.readLine(":"), e)
      case s if s.startsWith("u") => e.u; exe (scala.io.StdIn.readLine(":"), e)
      case s if s.startsWith("u") => e.ctrlr; exe (scala.io.StdIn.readLine(":"), e)
      case s if s.startsWith("e") => example(e)
      case _ => exe (scala.io.StdIn.readLine(":"), e)
    }
  }

  def example(e:editor) : Unit = {
    e.x
    e.l()
    e.l(5)
    e.h()
    e.l(10)
    e.h(3)
    e.i('X')
    e.l(2)
    e.dw
    e.l(9)
    e.iw("ciccione")
    }
}

class editor (input_line:String, input_pos:Int=0) extends debug with UndoRedo {
  var line = input_line;
  var pos = input_pos;
  print("beg", line, pos)

  override def x = {
    var (x,y) = line.splitAt(pos)
    line = x + y.substring(1)
    pos = Math.min(pos,line.length-1)
    super.x
    print("x", line, pos)
  }

  override def dw = {
    var (x,y) = line.splitAt(pos)
    line = x + y.substring(if (y.indexOf(' ') < 0) y.length else y.indexOf(' '))
    pos = Math.min(pos+1,line.length-1)
    super.dw
    print("dw", line, pos)
  }

  override def i (c:Char) = {
    var (x,y) = line.splitAt(pos+1)
    line = x + c + y
    pos = Math.min(pos+1,line.length-1)
    super.i(c)
    print("i", line, pos)
  }

  override def iw (w:String) = {
    var (x,y) = line.splitAt(pos)
    line = x + w + " " + y
    pos = pos+w.length+1
    super.iw(w)
    print("iw", line, pos)
  }

  override def l (i:Int=1) = {
    pos = Math.min(pos + i,line.length-1)
    super.l(i)
    print("l", line, pos)
  }

  override def h (i:Int=1) = {
    pos = Math.max(pos - i, 0)
    super.h(i)
    print("h", line, pos)
  }

  override def u = {
    super.getLast match {
      case ("x",0) => x
      case ("dw",0) => dw
      case ("i",c) => l();x
      case ("iw",w) => l(w.asInstanceOf[String].length);dw
      case ("l",i) => h(i.asInstanceOf[Int])
      case ("h",i) => l(i.asInstanceOf[Int])

      case ("u",0) => ctrlr
      case ("ctrlr",0) => u
    }
  }

  override def ctrlr = {

  }
}
