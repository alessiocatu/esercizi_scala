import scala.language.postfixOps

case class Time(hours: Int = 0, minutes: Int = 0) {
  require(hours >= 0 && hours <= 23, "hours must be within 0 and 23 - " + hours)
  require(minutes >= 0 && minutes <= 59, "minutes must be within 0 and 59")

  def pm = {new Time(hours + 12, minutes)}
}

object TimeDSL {
  implicit class IntExtd(value:Int) {
    def am = {new Time(value,0)}
    def ::(hours:Int) = {new Time(hours, value)}
  }

  // Exercise 1:
  // Make the following syntaxes work.
  // Hint: All we need are operator notation and implicit conversions
  val t1: Time = 11 am
  val t2: Time = 11 :: 30
  val t3: Time = 11 :: 30 pm
  val t4: Time = 11 :: 12

  def main(args:Array[String]) = {
    println(t1)
    println(t2)
    println(t3)
    println(t4)
  }
}
