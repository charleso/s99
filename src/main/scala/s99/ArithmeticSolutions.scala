package s99

import Solutions.???

trait ArithmeticSolutions {

  // add new functions to integers
  implicit def extendInt(n: Int): ExtendedInt = ExtendedInt(n)

  case class ExtendedInt(n: Int) {

    def isPrime: Boolean = ???
    def isCoprimeTo(n: Int): Boolean = ???
    def totient: Int = ???
    def improvedTotient: Int = ???
    def primeFactors: List[Int] = ???
    def primeFactorMultiplicity: List[(Int, Int)] = ???
    def primeFactorMultiplicityMap: Map[Int, Int] = ???
    def goldbach: (Int, Int) = ???

  }

  def primes: Stream[Int] = ???
  def gcd(m: Int, n: Int): Int = ???
  def listPrimesinRange(r: Range): List[Int] = ???
  def printGoldbachList(r: Range): List[String] = ???
  def printGoldbachListLimited(r: Range, limit: Int): List[String] = ???

}
