package s99

import collection.immutable.SortedSet

trait MiscellaneousSolutions {

  def queens = queensN(8)

  def queensN(n: Int): Iterator[Seq[Int]] = {
    def isValid(l: Seq[Int]) = {
      def check(f: (Int, Int) => Int) = l.zipWithIndex.map(f.tupled).distinct.size == l.length
      check(_ - _) && check(_ + _)
    }
    ((0 until n) permutations) filter (isValid)
  }

  /* Knights */

  type Pt = (Int, Int)

  class Board(val n: Int, val s: Set[Pt] = Set(), val l: List[Pt] = List()) {

    def +(pt: Pt) = {
      val s2 = s + pt
      val l2 = pt :: l
      new Board(n, s2, l2)
    }

    val max = n * n
    def isFull = s.size == max

    def contains(p: Pt) = s.contains(p)

    def validMoves = {
      val p = l.head
      def isValid(i: Int) = i >= 0 && i < n
      Board.moves.map {
        case (x, y) => (x + p._1, y + p._2)
      }.filter {
        newP => isValid(newP._1) && isValid(newP._2)
      }
    }

    override def equals(n:Any) = n.asInstanceOf[List[_]] == l
    override def toString = l.toString
  }

  object Board {
    val moves = Stream((1, 2)).flatMap {
      case (x, y) => Stream((x, y), (-x, y), (x, -y), (-x, -y))
    }.flatMap(t => Stream(t, t.swap))

    def apply(n: Int, p: Pt) = new Board(n) + p
  }

  def knight = knightN(8)

  def knightClosedN(n: Int) = knightN(n).filter(_.validMoves.contains((0, 0)))

  def knightN(n: Int) = {
    def jump(b: Board): Stream[Board] = {
      if (b.isFull) Stream(b)
      else b.validMoves.filter(!b.contains(_)).flatMap(next => jump(b + next))
    }

    jump(Board(n, (0, 0)))
  }

}
