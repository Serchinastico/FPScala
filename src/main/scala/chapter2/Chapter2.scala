package chapter2

object Chapter2 {
  // Exercise 2.1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: (Int, Int)): Int = {
      if (n == 0) {
        acc._1
      } else if (n == 1) {
        acc._2
      } else {
        go(n - 1, (acc._2, acc._1 + acc._2))
      }
    }
    go(n, (0, 1))
  }
}
