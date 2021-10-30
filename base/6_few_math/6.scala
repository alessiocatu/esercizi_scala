object sixth {
  def main (args:Array[String]) = {
    val six = new sixth()
    println(six.goldbach(10))
    println(six.goldbach_list(4,20))
  }
}

class sixth {
  def goldbach (i:BigInt, start:BigInt=2) = {
      var primes = getPrimes(i, start)

      (for (n <- primes if primes.exists(y => n+y==i))
        yield (n,primes.find(y => n+y==i).getOrElse(0))
      ).head
    }

  def goldbach_list (n:BigInt, m:BigInt, start:BigInt=2) = {
      LazyList
      .iterate(n)(_+2)
      .takeWhile(_ <= m)
      .map(n => (n, goldbach(n)))
      .toList
    }

  def getPrimes(i:BigInt, start:BigInt=2) = {
    LazyList
    .iterate(start)(_+1)
    .takeWhile(_<i)
    .filter(x => LazyList.iterate(start)(_+1).takeWhile(_<x).filter(x%_ == 0).length == 0)
    .toList
  }
}
