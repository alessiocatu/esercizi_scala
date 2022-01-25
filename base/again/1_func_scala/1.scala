object First {
  def main(args: Array[String]) = {
    val first = new First()
    println("test is_palindrome = teet -> " + first.is_palindrome("teet"))
    println("test is_palindrome = test -> " + first.is_palindrome("test"))
    println("test is_an_anagram = aaa, [abc,abb,acc,aba,aaa] -> " + first.is_an_anagram("aaa", List("abc","abb","acc","aba","aaa")))
    println("test is_an_anagram = aaa, [abc,abb,acc,aba,aac] -> " + first.is_an_anagram("aaa", List("abc","abb","acc","aba","aac")))
    println("test factors = 15 -> " + first.factors(15))
    println("test factors = 7  -> " + first.factors(7))
    println("test factors = 13  -> " + first.factors(13))
    println("test factors = 28  -> " + first.factors(28))
    println("test is_proper = 6  -> " + first.is_proper(6))
    println("test is_proper = 7  -> " + first.is_proper(7))
  }
}

class First {
  val illegalCharacters = List('.', '?', '^', ' ', ',')

  def is_palindrome(s:String) = {
    var cleared = s.toLowerCase().filter(!illegalCharacters.contains(_))
    cleared.equals(cleared.reverse)
  }

  def is_an_anagram(s:String, dict:List[String]) = {
    dict.exists((_).toSeq.sorted.equals(s.toSeq.sorted))
  }

  def factors(i:Int, start:Int = 2, list:List[Int] = Nil): List[Int] = {
    LazyList
      .iterate(start)(_+1)
      .takeWhile((_) <= i)
      .find(i%_ == 0)
      .map(n => factors(i/n, n, list:+ n))
      .getOrElse(list)
  }

  def is_proper(i:Int) = {
    LazyList
      .iterate(1)(_+1)
      .takeWhile(_ < i)
      .filter(i%_ == 0)
      .toList
      .foldLeft(0)((a,b) => a+b) == i
  }
}
