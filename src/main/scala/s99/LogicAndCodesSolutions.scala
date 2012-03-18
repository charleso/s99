package s99

import Solutions.???

trait LogicAndCodesSolutions { outer =>

  def and(a: Boolean,  b: =>Boolean): Boolean = ???
  def or(a: Boolean,   b: =>Boolean): Boolean = ???
  def nand(a: Boolean,  b: =>Boolean): Boolean = ???
  def nor(a: Boolean,  b: =>Boolean): Boolean = ???
  def xor(a: Boolean,  b: =>Boolean): Boolean = ???
  def impl(a: Boolean,  b: =>Boolean): Boolean = ???
  def equ(a: Boolean,  b: =>Boolean): Boolean = ???
  def not(a: Boolean) = ???
  
  def table2(f: (Boolean, Boolean) => Boolean): String = ???

  def gray(n: Int): List[String] = ???
  def huffman(list: List[(String,  Int)]): List[(String, String)] = ???

}
