package s99

import Solutions.???
import annotation.tailrec

trait MyArithmeticSolutions extends ArithmeticSolutions {

  // add new functions to integers
  implicit override def extendInt(n: Int): ExtendedInt = ExtendedInt2(n)

  case class ExtendedInt2(n2: Int) extends ExtendedInt(n2) {

    def subPrimes = primes.takeWhile(_ <= n)

    override def isPrime: Boolean = subPrimes.find(_ == n).isDefined

    override def isCoprimeTo(m: Int): Boolean = gcd(m, n) == 1

    override def totient: Int = (1 to n) filter (isCoprimeTo(_)) length

    override def improvedTotient: Int = primeFactorMultiplicity.foldLeft(1) {
      case (t, (p, m)) => t * (p - 1) * math.pow(p, m - 1).toInt
    }

    override def primeFactors: List[Int] = {
      subPrimes.find(n % _ == 0).map {
        i => i :: (n / i).primeFactors
      }.toList.flatten
    }

    override def primeFactorMultiplicity: List[(Int, Int)] = primeFactorMultiplicityMap.toList.sortBy(_._1)

    override def primeFactorMultiplicityMap: Map[Int, Int] = primeFactors.groupBy(identity).mapValues(_.size)

    override def goldbach: (Int, Int) = subPrimes.map(i => (i, n - i)).filter(_._2.isPrime).head

  }

  override val primes: Stream[Int] = 2 #:: (Stream.from(3) filter {
    i => val sqrt = math.sqrt(i).ceil.toInt; (sqrt #:: primes.takeWhile(_ < sqrt)).find(i % _ == 0).isEmpty
  })

  @tailrec
  final override def gcd(m: Int, n: Int): Int = if (n == 0) m else gcd(n, m % n)

  override def listPrimesinRange(r: Range): List[Int] = primes.dropWhile(_ < r.start).takeWhile(_ <= r.end).toList

  override def printGoldbachList(r: Range): List[String] = printGoldbachListLimited(r, 0)

  override def printGoldbachListLimited(r: Range, limit: Int): List[String] = r
    .filter(_ % 2 == 0)
    .filter(_ > 2)
    .map(i => (i, i.goldbach))
    .filter {
    case (a, (b, c)) => b > limit && c > limit
  }.map {
    case (a, (b, c)) => "%d = %d + %d" format(a, b, c)
  }.toList

}
