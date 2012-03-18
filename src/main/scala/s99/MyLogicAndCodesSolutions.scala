package s99

import Solutions.???

trait MyLogicAndCodesSolutions extends LogicAndCodesSolutions {

  override def and(a: Boolean, b: => Boolean) = (a, b) match {
    case (true, true) => true
    case _ => false
  }

  override def or(a: Boolean, b: => Boolean) = (a, b) match {
    case (true, _) => true
    case (_, true) => true
    case _ => false
  }

  override def nand(a: Boolean, b: => Boolean) = not(and(a, b))

  override def nor(a: Boolean, b: => Boolean) = not(or(a, b))

  override def xor(a: Boolean, b: => Boolean) = (a, b) match {
    case (true, true) => false
    case (false, false) => false
    case _ => true
  }

  override def impl(a: Boolean, b: => Boolean) = (a, b) match {
    case (true, false) => false
    case _ => true
  }

  override def equ(a: Boolean, b: => Boolean) = not(xor(a, b))

  override def not(a: Boolean) = a match {
    case true => false
    case false => true
  }

  override def table2(f: (Boolean, Boolean) => Boolean) = {
    List("A", "B", "result") :: {
      List((true, true), (true, false), (false, true), (false, false)).map {
        case (a, b) => List(a, b, f(a, b))
      }.toList
    }
  }.map(_.map("%-5s" format _))
    .map(_.mkString(" ").trim)
    .mkString("\n", "\n", "")

  override def gray(n: Int) = gray2(n)

  def gray1(n: Int): List[String] = {
    if (n == 1) List("0", "1")
    else gray1(n - 1).map("0" + _) ::: gray1(n - 1).reverse.map("1" + _)
  }

  // Silly really - better to use Scalaz for this
  val gray2: Stream[List[String]] = Nil #:: List("0", "1") #:: Stream.from(2).map {
    n => gray2(n - 1).map("0" + _) ::: gray2(n - 1).reverse.map("1" + _)
  }

  /*
   First of all, consult a good book on discrete mathematics or algorithms for a detailed description of Huffman
  codes! We suppose a set of symbols with their frequencies, given as a list of (S, F) Tuples.
  E.g. (("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)). Our objective is to construct a
  list of (S, C) Tuples, where C is the Huffman code word for the symbol S""" >>
   */
  override def huffman(list: List[(String, Int)]): List[(String, Int)] = list.map {
    case (h, t) => (h, t)
  }

}
