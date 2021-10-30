object second {
  def main(args: Array[String]) = {
    println("#Start")
    println(squared_numbers(1 :: "hello" :: 100 :: 3.14 :: ('a'::10::Nil) :: 'c' :: (5,7,'a', (3,4) ) :: Nil))
    println(intersect(List(1,2,3,4,5), List(4,5,6,7,8)))
    println(symmetric_difference(List(1,2,3,4,5), List(4,5,6,7,8)))
    println("#End")
  }

  def squared_numbers (list:List[Any], res:List[Any]=Nil): List[Any] = {
    (for (elem <- list) yield elem match {
      case i:Int => i*i
      case i:Float => i*i
      case i:Double => i*i
      case hd::tl => squared_numbers(hd::tl)
      case tuple:Product => toTuple (squared_numbers (tuple.productIterator.toList.filter( m =>
          m match {
            case _:Int => true
            case _:Float => true
            case _:Double => true
            case _:List[Any] => true
            case _:Product => true
            case _ => false
          })))
      case _ =>
    }).filter(_ != ())
  }

  def toTuple(as:List[Any]):Product = {
    val tupleClass = Class.forName("scala.Tuple" + as.size)
    tupleClass.getConstructors.apply(0).newInstance(as:_*).asInstanceOf[Product]
  }

  def intersect(a:List[Any], b:List[Any]) = {
    a intersect b
  }

  def symmetric_difference(a:List[Any], b:List[Any]) = {
    //a.union(b).toSet.toList.diff(a intersect b)
    a.diff(a intersect b).concat(b.diff(a intersect b))
  }
}
