package s99

import annotation.tailrec

trait MyListsSolutions extends ListsSolutions {

  def all[T](expected: T, all: T*): T = {
    for (e <- all) {
      assert(e == expected, {
        expected + " != " + e
      })
    }
    expected
  }

  override def last[T](list: List[T]) = {
    @tailrec
    def lastRecursive[T](list: List[T]): T = list match {
      case Nil => throw new NoSuchElementException
      case x :: Nil => x
      case _ :: xs => lastRecursive(xs)
    }

    def lastLoop[T](list: List[T]): T = {
      var l = list
      while (!l.tail.isEmpty) {
        l = l.tail
      }
      l.head
    }

    all(lastLoop(list), lastRecursive(list));
  }

  @tailrec
  override final def penultimate[T](list: List[T]): T = list match {
    case Nil => throw new NoSuchElementException
    case x :: _ :: Nil => x
    case _ :: xs => penultimate(xs)
  }

  override def nth[T](n: Int, list: List[T]): T = {
    @tailrec
    def nthRecursive[T](n: Int, list: List[T]): T = (n, list) match {
      case (_, Nil) => throw new NoSuchElementException
      case (0, x) => x.head
      case (_, xs) => nthRecursive(n - 1, xs.tail)
    }
    all(nthRecursive(n, list))
  }

  override final def length[T](list: List[T]): Int = {
    def lengthFold = list.foldLeft(0)((x, _) => x + 1)
    @tailrec
    def lengthRec(i: Int, list: List[T]): Int = list match {
      case _ :: tail => lengthRec(i + 1, tail)
      case Nil => i
    }
    def lengthLoop = {
      var count = 0;
      for (_ <- list) {
        count = count + 1
      }
      count
    }
    all(lengthLoop, lengthFold, lengthRec(0, list))
  }

  override def reverse[T](list: List[T]): List[T] = {
    def reverseFold = list.foldLeft(Nil: List[T])((xs, x) => x :: xs)
    // This is bad - list is a linked list!
    def reverseLoop = {
      var newList = collection.mutable.ListBuffer[T]()
      for (i <- (list.length - 1) to(0, -1)) {
        newList += list(i)
      }
      newList.toList
    }
    all(reverseFold, reverseLoop)
  }

  override def isPalindrome[T](list: List[T]): Boolean = {
    def palidromeEasy = reverse(list) == list
    def palidromeLoop = {
      val length = list.length - 1
      var is = false;
      for (i <- 0 to length) {
        is = list(i) == list(length - i)
      }
      is
    }
    all(palidromeLoop, palidromeEasy)
  }

  override def flatten(list: List[Any]): List[Any] = {
    def flattenPattern(l: List[Any]): List[Any] = l match {
      case Nil => Nil
      case (x :: ys) :: xs => x :: flattenPattern(ys) ::: flattenPattern(xs)
      case x :: xs => x :: flattenPattern(xs)
    }
    all(flattenPattern(list))
  }

  override def compress[T](list: List[T]): List[T] = {
    def compressFold = list.foldRight(Nil: List[T]) {
      case (x, y :: xs) => if (x == y) x :: xs else x :: y :: xs
      case (x, Nil) => x :: Nil
    }
    def compressLoop = {
      val newList = collection.mutable.ListBuffer[T]()
      val len = length(list) - 1
      for (i <- 0 to len) {
        if (i == len || list(i) != list(i + 1)) {
          newList += list(i)
        }
      }
      newList.toList
    }
    all(compressFold, compressLoop)
  }

  override def pack[T](list: List[T]): List[List[T]] = {
    @tailrec
    def packSpan(newList: List[List[T]], list: List[T]): List[List[T]] = list match {
      case Nil => newList
      case l =>
        val (a, b) = list.span(_ == list.head)
        packSpan(a :: newList, b)
    }
    def packLoop = {
      val newList = collection.mutable.ListBuffer[List[T]]()
      val tempList = collection.mutable.ListBuffer[T]()
      if (!list.isEmpty) {
        var l: List[T] = null.asInstanceOf[T] :: list
        def update = {
          l = l.tail
          tempList += l.head
        }
        update
        while (!l.tail.isEmpty) {
          if (l.head != l.tail.head) {
            newList += tempList.toList
            tempList.clear
          }
          update
        }
        newList += tempList.toList
      }
      newList.toList
    }
    all(packSpan(Nil, list).reverse, packLoop)
  }

  override def encode[T](list: List[T]) = pack(list).map(l => (l.length, l.head))

  override def encodeModified[T](list: List[T]) = encode(list).map {
    case (1, h) => h
    case t => t
  }

  override def decode[T](list: List[(Int, T)]) = list.flatMap {
    case (i, e) => List.fill(i)(e)
  }

  override def encodeDirect[T](list: List[T]): List[(Int, T)] = {
    list.foldRight(Nil: List[(Int, T)]) {
      case (t, (a, b) :: xs) if t == b => (a + 1, t) :: xs
      case (t, xs) => (1, t) :: xs
    }
  }

  override def duplicate[T](list: List[T]): List[T] = {
    def dupFlatMap = list.flatMap(x => List(x, x))
    def dupFold = list.foldRight(Nil: List[T])((x, xs) => x :: x :: xs)
    def dupManual = {
      val l = collection.mutable.ListBuffer[T]()
      for (x <- list) {
        l += x
        l += x
      }
      l.toList
    }
    all(dupFlatMap, dupManual, dupFold)
  }

  override def duplicateN[T](n: Int, list: List[T]): List[T] = {
    def dupFlatMap = list.flatMap(List.fill(n)(_))
    def dupFold = list.foldRight(Nil: List[T])((x, xs) => List.fill(n)(x) ::: xs)
    def dupManual = {
      val l = collection.mutable.ListBuffer[T]()
      for (x <- list) {
        for (i <- 0 until n) {
          l += x
        }
      }
      l.toList
    }
    all(dupFlatMap, dupManual, dupFold)
  }

  override def drop[T](n: Int, list: List[T]): List[T] = {
    @tailrec
    def dropRec(i: Int, list: List[T], newlist: List[T]): List[T] = if (list.isEmpty) {
      newlist
    } else if (i == n) {
      dropRec(1, list.tail, newlist)
    } else {
      dropRec(i + 1, list.tail, list.head :: newlist)
    }

    def dropZip = list.zipWithIndex.filter(_._2 % n != n - 1).map(_._1)

    def dropManual = {
      val l = collection.mutable.ListBuffer[T]()
      for ((x, i) <- list.zipWithIndex) {
        if (i % n != n - 1) {
          l += x
        }
      }
      l.toList
    }
    all(dropRec(1, list, Nil).reverse, dropManual, dropZip)
  }
}
