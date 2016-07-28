package com.serchinastico.fpscala.chapter2

object Chapter2 {
  // Exercise 2.1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: (Int, Int)): Int =
      if (n == 0) acc._1
      else if (n == 1) acc._2
      else go(n - 1, (acc._2, acc._1 + acc._2))

    go(n, (0, 1))
  }

  // Exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(as: Array[A], lastItem: A): Boolean =
      if (as.isEmpty) true
      else if (ordered(lastItem, as.head)) go(as.tail, as.head)
      else false

    if (as.isEmpty) true
    else if (as.length == 1) true
    else go(as.tail, as.head)
  }
}
