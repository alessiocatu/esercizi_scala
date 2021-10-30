/*
Define the following functions in Scala:

is_palindrome: string → bool that checks if the string given as input is palindrome,
a string is palindrome when the represented sentence can be read the same way in either
directions in spite of spaces, punctual and letter cases, e.g., detartrated, "Do geese see God?",
"Rise to vote, sir.", ...;

is_an_anagram : string → string list → boolean that given a dictionary of strings,
checks if the input string is an anagram of one or more of the strings in the dictionary;

factors: int → int list that given a number calculates all its prime factors;

is_proper: int → boolean that given a number calculates if it is a perfect number or not,
where a perfect number is a positive integer equal to the sum of its proper positive divisors
(excluding itself), e.g., 6 is a perfect number since 1, 2 and 3 are the proper divisors of 6 and 6 is equal to 1+2+3;


*/

object first {
  val illegalCharacters = List('?', '.', ' ', ',')

  def main (args: Array[String]) = {
    println("#Start")
    println(is_palindrome("Do geese see God?"))
    println(is_an_anagram("abc", List("aaa", "abb", "bca")))
    println(factors(204))
    println(is_proper(28))
    println("#End")
  }

  def is_palindrome(s: String) = {
    var clear = s
    .toLowerCase()
    .filter(!illegalCharacters.contains(_))

    clear.equals(clear.reverse)
  }

  val is_an_anagram = (s: String, dict: List[String]) =>
    dict.exists(_.toSeq.sorted.equals(s.toSeq.sorted))

  def factors(i:BigInt, start:BigInt=2, list:List[BigInt]=Nil): List[BigInt] = {
    LazyList
      .iterate(start)(_+1)
      .takeWhile(_<= i)
      .find(i%_ == 0)
      .map(n => factors(i/n , n, list:+n ))
      .getOrElse(list)
    }

  def is_proper(i:BigInt, start:BigInt=1) = {
    LazyList
      .iterate(start)(_+1)
      .takeWhile(_<i)
      .filter(i%_ == 0)
      .toList
      .foldLeft(BigInt(0))((a,b) => a+b) == (i)
    }
}
