import scala.actors._

object thrird {
  def main(args: Array[String]) = {
    var m = new Master
    m.start()
    m.long_reverse_string("TestTestTestTest")
  }
}

class Slave extends Actor {
  def act() {
    receive {
      case (n:Pos, s: String) => sender ! (n, s.reverse()); act()
      case _ =>
    }
  }
}

object Master extends Actor {
  def act() {
    receive {
      //Manca la gestione del pezzo finale e del risultato post reverse
      case s: String if s.length < 10 =>
      case s: String =>
        var slave = new Slave;
        slave.start();
        var s_cutted = if (s.length >= 10) s.substrings(0,10) else s
        slave ! (1, s_cutted);
        Actor.self ! s_cutted;
        act()
      case _ =>
    }
  }

  def long_reverse_string(s:String) = {
    //!? invio sincrono
    Actor.self !? s
  }
}
