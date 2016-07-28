package com.serchinastico.fpscala.chapter2

import org.scalatest.{FlatSpec, Matchers}

class Chapter2Spec extends FlatSpec with Matchers {
  "fibonacci of 0" should "be equal to 0" in {
    Chapter2.fib(0) should be (0)
  }

  "fibonacci of 1" should "be equal to 1" in {
    Chapter2.fib(1) should be (1)
  }

  "fibonacci of 2" should "be equal to 1" in {
    Chapter2.fib(2) should be (1)
  }

  "fibonacci of 3" should "be equal to 2" in {
    Chapter2.fib(3) should be (2)
  }

  "fibonacci of 4" should "be equal to 3" in {
    Chapter2.fib(4) should be (3)
  }

  "fibonacci of 5" should "be equal to 5" in {
    Chapter2.fib(5) should be (5)
  }

  "fibonacci of 32" should "be equal to 2178309" in {
    Chapter2.fib(32) should be (2178309)
  }
}

class Chapter2Exercise2Spec extends FlatSpec with Matchers {
  "isSorted of an empty array" should "return true" in {
    Chapter2.isSorted(Array.empty, (a: Int, b: Int) => false) should be (true)
  }

  "isSorted of an unitary array" should "return true" in {
    Chapter2.isSorted(Array[Int](0), (a: Int, b: Int) => false) should be (true)
  }

  "isSorted of an sorted array" should "return true" in {
    Chapter2.isSorted(Array[Int](0, 1, 2, 3, 4), (a: Int, b: Int) => a < b) should be (true)
  }

  "isSorted of an unsorted array" should "return false" in {
    Chapter2.isSorted(Array[Int](0, 1, 2, 3, 2), (a: Int, b: Int) => a < b) should be (false)
  }

}