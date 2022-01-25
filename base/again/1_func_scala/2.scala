object second {
  def main(args:Array[String]) = {
    val second = new second()
    println("squared_numbers -> "
      + second.squared_numbers(1 :: "hello" :: 100 :: 3.14 :: ('a'::10::Nil) :: 'c' :: (5,7,'a') :: Nil))
    println("intersect -> " + second.intersect(List(1,2,3,4,5), List(4,5,6,7,8)))
    println("symmetric_difference -> " + second.symmetric_difference(List(1,2,3,4,5), List(4,5,6,7,8)))
  }
}

class second {
  def squared_numbers(lists:List[Any]): List[Any] = {
    (for (elem <- lists) yield elem match {
      case i:Int => i*i
      case i:Float => i*i
      case i:Double => i*i
      case l:List[Any] => squared_numbers(l)
      case t:Product => toTuple(squared_numbers(t.productIterator.toList.filter(
        m => m match {
          case m:Int => true
          case m:Float => true
          case m:Double => true
          case m:List[Any] => true
          case m:Product => true
          case _ => false
        }
      )))
      case _ =>
    }).filter(_ != ())
  }

  def toTuple(as:List[Any]):Product = {
    val tupleClass = Class.forName("scala.Tuple" + as.size)
    tupleClass.getConstructors.apply(0).newInstance(as:_*).asInstanceOf[Product]
  }

  val intersect = (a:List[Any], b:List[Any]) => a intersect b

  val symmetric_difference = (a:List[Any], b:List[Any]) => (a.filter(!b.contains(_)) concat b.filter(!a.contains(_)))
}
