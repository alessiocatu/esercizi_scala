//package dsl.internal
//import scala.language.postfixOps

case class City(name: String)
case class Train(name: String, schedule: List[(Time, City)]) {
  //assert(schedule.length > 1, "Schedule must contain at least two elements")

  def at(time:Time) = new Train(name, schedule :+ (time, new City("")))
  def at(time:String) = {
    val timef = new Time(time.substring(0,2).toInt, time.substring(time.length-2, time.length).toInt)
    new Train(name, schedule :+ (timef, new City("")))
  }
  def from(city:String) = new Train(name, schedule.dropRight(1) :+ (schedule.last._1, new City(city)))
}

object TrainDSL {
  import TimeDSL._

  implicit class TrainExtd(name:String) {
    def at(time:Time) = new Train(name, List((time, new City(""))))
  }

  // Exercise2: Make the following two syntaxes work and return a Train
  val train1: Train = "International" at
     8 :: 50 from "Rotterdam" at
  "11 :: 20" from "Paris"

  val train2: Train = "Intercity" at
    8 :: 50 from "Rotterdam" at
    "10:00" from "Amersfoort" at
    12 :: 10 from "Groningen"

  def main(args:Array[String]) = List(train1, train2).foreach {println(_)}
}
