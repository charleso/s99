package s99

import org.specs2.mutable.Specification

class MiscellaneousSpec extends Specification with MiscellaneousSolutions {

  """This is a classical problem in computer science. The objective is to place eight queens on a chessboard so that no
  two queens are attacking each other; i.e., no two queens are in the same row, the same column, or on the same diagonal.

  Hint: Represent the positions of the queens as a list of numbers 1..N. Example: List(4, 2, 7, 3, 6, 8, 5, 1) means
  that the queen in the first column is in row 4, the queen in the second column is in row 2, etc. Use the
  generate-and-test paradigm.""" >> {
    queens.contains(List(0, 4, 7, 5, 2, 6, 1, 3))
  }

  """Another famous problem is this one: How can a knight jump on an N—N chessboard in such a way that it visits every square exactly once?
  Hints: Represent the squares by pairs of their coordinates of the form (X, Y), where both X and Y are integers between 1 and N.
  (Alternately, define a Point class for the same purpose.) Write a function jumps(N, (X, Y))
  to list the squares that a knight can jump to from (X, Y) on a N—N chessboard.
  And finally, represent the solution of our problem as a list of knight positions (the knight's tour).

  It might be nice to find more than one tour, but a computer will take a long time trying to find them all at once.
  Can you make a lazy list that only calculates the tours as needed?

  Can you find only "closed tours", where the knight can jump from its final position back to its starting position?""" >> {
    val k5 = knightN(5)
    k5.contains(List((0,4), (2,3), (4,4), (3,2), (4,0), (2,1), (0,2), (1,4), (3,3), (4,1), (2,0), (0,1), (1,3), (3,4), (4,2), (3,0), (1,1), (0,3), (2,2), (1,0), (3,1), (4,3), (2,4), (1,2), (0,0)))
    k5.length === 304
    val kc6 = knightClosedN(6)
    kc6.size === 1
  }
}
