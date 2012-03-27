#Jump Start

https://github.com/etorreborre/s99

## Hello World

    object Hello {
        def main(args:Array[String]) {
            println("Hello world")
        }
    }

## Val vs Var

    val a = "a string"
    var b = "bad string"
    // a = "can't do this"
    b = "mutability ftw"

## Syntax

    "a string".toUpperCase();
    "a string".toUpperCase()
    "a string".toUpperCase
    "a string" toUpperCase
    1 + 2
    1.+(2)

## Objects

    class Something(val a: String)
    object Something {
        def b = "Global"
    }
    val a = new Something("foo")
    println(a.a)
    println(Something.b)

## Everything is a statement

    // String a = something ? "abc" : "xyz"
    val a : String = if (something) {
        "abc"
    } else {
        "xyz"
    }
    val a : String = if (something) "abc" else "xyz"

## Methods

    def normalFunction(a: String): String = {
        a + "!" // No return statement
    }
    def noBrace(a:String) = a + "!"
    def notTheSameThing(a: String) {
        a + "!"
    }
    def notTheSameThing(a: String): Unit = {
        a + "!"
    }

## Lambdas

    def foo(f : (Int, Int) => Int, i : Int) = f(i, i)
    foo((a, b) => a + b, 1)
    foo((a, b) => a * b, 1)

## Default params

    def method(a: String, b: String = "b", c: String = "c")
    method("a")
    method("a", "b")
    method("a", c = "c")

## Type inference

    val s: String = "string"
    val s2 = "string"
    def something(a:String) {
        // Always need types on parameters :(
    }

## Pattern matching

    val string = "a string"
    val result = string match {
        case "another string" => "foo"
        case "a string" => "bar"
        case _ => "miss!"
    }
    val tup =  ("a", 1)
    tup match {
        case ("x", 3) => ...
        case (_, 2) => ...
        case ("a", 1) => ...
        case _ => ...
    }

## Lists

    val list1 = List("a", "b", "c")
    val list2 = Nil.::("c").::("b").::("a") // Same thing - yuck
    val list3 = "a" :: "b" :: "c" :: Nil // Same thing
    list1 match {
        case head :: tail => head
        case Nil =>
    }

## Loops

    var i = 0
    while (i < 10) {
        i++
    }
    for (i <- 0 until 10) {
        // ...
    }
    for ( (a, i) <- List("x", "y", "z").zipWithIndex) {
        // ...
    }

## Options

    val mayBeNull : Option = ...
    if (mayBeNull.isDefined) {
        val value = mayBeNull.get
    }
    for (i <- mayBeNull) {
        // No NPE!
    }
    val doubleO: Option[Option[String]] = ...
    for (anotherOption <- doubleO; string <-anotherOption) {
        // No NPE!
    }

## Everything is a statement (part 2)

    val a : List[String] = for (i <- List(1, 2, 3)) {
        yield i.toString
    }
    val b : Option[String] = for (i <- Some(1)) {
        yield i.toString
    }

## Case class

    case class Something(a : String)
    val s1 = new Something("a")
    s1.a
    val s2 = Something("b")

    case class Something2(private var _a: String) {
      def a = _a
      def a_=(v: String) { _a = v }
    }

## Traits

    trait Ordered[A] {
        def compare(that: A): Int

        def <  (that: A): Boolean = (this compare that) <  0
        def >  (that: A): Boolean = (this compare that) >  0
        def <= (that: A): Boolean = (this compare that) <= 0
        def >= (that: A): Boolean = (this compare that) >= 0
    }

    class AnOrdered(val s:String) extends Ordered[String] {
        override compare(that:String) = s compare that
    }


## First example

    "Find the last element of a list" >> { last(List(1, 1, 2, 3, 5, 8)) === 8 }

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

## Recursion

    def packRec(list: List[T]): List[List[T]] = list match {
      case Nil => Nil
      case l =>
        val (a, b) = list.span(_ == list.head)
        a :: packRec(b)
    }

## Tail Recursion

    @tailrec
    def packSpan(newList: List[List[T]], list: List[T]): List[List[T]] = list match {
      case Nil => newList
      case l =>
        val (a, b) = list.span(_ == list.head)
        packSpan(a :: newList, b)
    }

## Some functional exercises

* http://blog.tmorris.net/further-understanding-scalaoption/
* http://blog.tmorris.net/monad-exercises-in-scala/

## TicTacToe

http://blog.tmorris.net/scala-exercise-with-types-and-abstraction/

Write an API for the tic-tac-toe game. Do not use variables -- they are not permitted. This includes libraries that expose in-line updates.
No exceptions (or non-termination) in exposed functions -- all functions return a consistent value for every element of their domain.
The follow API methods should exist:

* move: takes a tic-tac-toe board and position and moves to that position (if not occupied) returning a new board.
  This function can only be called on a board that is in-play. Calling move on a game board that is finished is a *compile-time type error*.
* whoWon: takes a tic-tac-toe board and returns the player that won the game (or a draw if neither).
  This function can only be called on a board that is finished. Calling move on a game board that is in-play is a *compile-time type error*.
* takeBack: takes either a finished board or a board in-play that has had at least one move and returns a board in-play.
  It is a compile-time type error to call this function on an empty board.
* playerAt: takes a tic-tac-toe board and position and returns the (possible) player at a given position.
  This function works on any type of board.
* Other API functions that you may see fit. These can be determined by also writing an interactive console application
  that uses the API -- other useful functions are likely to arise.

You should encode this property in an automated specification test. For Scala, use ScalaCheck. For Haskell, QuickCheck. For Java, consider Functional Java. For other languages such as C# or F#, you may need to search around.

https://github.com/charleso/haskell-course/blob/904567fd7f245f02290f1e2e17e5b28192ba5ae9/TicTacToe/src/main/scala/TicTacToe.scala

## Game of life

* https://extranet.atlassian.com/display/~jed/2011/09/04/Functional+Conway
* https://github.com/charleso/haskell-course/blob/master/GameOfLife/src/main/scala/org/charleso/GameOfLife.scala

## Resources

* http://www.amazon.com/Programming-Scala-Comprehensive-Step-Step/dp/0981531644
* http://blog.charleso.org/scala-talk/#slide1
