package com.serchinastico.fpscala.chapter3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object Chapter3 {

  object List {
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    // Exercise 3.1
    // The execution of the match statement returns 3.
    // The input list matches with the expression Cons(x, Cons(y, Cons(3, Cons(4, _))))

    // Exercise 3.2
    def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => throw new IllegalArgumentException("Cannot tail of empty list")
      case Cons(x, xs) => xs
    }

    // Exercise 3.3
    def setHead[A](head: A, l: List[A]): List[A] =
    l match {
      case Nil => throw new IllegalArgumentException("Cannot setHead of empty list")
      case Cons(x, xs) => Cons(head, xs)
    }

    // Exercise 3.4
    def drop[A](l: List[A], n: Int): List[A] =
    n match {
      case 0 => l
      case _ => drop(tail(l), n - 1)
    }

    // Exercise 3.5
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(x, xs) if f(x) => dropWhile(xs, f)
      case _ => l
    }

    // Exercise 3.6
    def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => init(xs)
    }

    // Exercise 3.7
    // It's not possible to stop execution during foldRight.
    // There are several possibilities to implement this, for now, we can
    // add a parameter with type "(a: A, z: B) => Boolean" to know if we should
    // stop running the foldRight function

    // Exercise 3.8
    // The list is reversed

    // Exercise 3.9
    def length[A](as: List[A]): Int = foldRight(as, 0)((a: A, b: Int) => b + 1)

    // Exercise 3.10
    @annotation.tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
      as match {
        case Nil => z
        case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
      }
    }

    // Exercise 3.11
    def sum(ints: List[Int]): Int =
      foldLeft(ints, 0)(_ + _)

    def product(ints: List[Int]): Int =
      foldLeft(ints, 1)(_ * _)

    def length2[A](l: List[A]): Int =
      foldLeft(l, 0)((b: Int, a: A) => b + 1)

    // Exercise 3.12
    def reverse[A](l: List[A]): List[A] =
      foldLeft(l, Nil: List[A])((b: List[A], a: A) => Cons(a, b))

    // Exercise 3.13
    def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      foldLeft(reverse(as), z)((b: B, a: A) => f(a, b))

    // Exercise 3.14
    def append[A](l1: List[A], l2: List[A]): List[A] =
      foldRight(l2, l1)(Cons(_, _))

    // Exercise 3.15
    def concat[A](l: List[List[A]]): List[A] =
      foldLeft(l, Nil:List[A])(append)
  }
}