package rainysjurgis

import scala.annotation.tailrec

object chapter2 {
  //exercise 2.1
  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, olderNum: Int, newerNum: Int): Int =
      if (n <= 0) newerNum
      else go(n - 1, newerNum, olderNum + newerNum)

    if (n <= 0) 0
    else go(n - 2, 0, 1)
  }

  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "the %s of %d is %d"
    msg.format(name, n, f(n))
  }

  private def formatFib(x: Int) = {
    if (x >= 0) {
      s"fibonnaci ${x}nth number is ${fib(x)}"
    }
    else {
      s"lowest fibonacci num is 0; you entered $x"
    }
  }

  private def formatAbs(n: Int) = {
    val keyword = "absolute value"
    formatResult(keyword, n, abs)
  }

  private def formatFactorial(n: Int) = {
    val keyword = "factorial"
    formatResult(keyword, n, factorial)
  }

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

    loop(0)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int, wasSorted: Boolean): Boolean =
      if (n >= as.length - 1) wasSorted
      else if (ordered(as(n), as(n + 1))) loop(n + 1, wasSorted = true)
      else false

    loop(0, wasSorted = true)
  }

  def areOrdered(a: Int, b: Int): Boolean = {
    a <= b
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

  def double(a: Int) =
    a * 2

  def add1(a: Int) =
    a + 1

  def main(): Unit = {
    //    println(formatAbs(-42))
    //    println(formatFactorial(4))
    //    println(formatFib(4))
    val arr = Array(1, 2, 3, 5)
    println(isSorted(arr, areOrdered))
    val composed = compose(double, add1)
    println(composed(1))
  }
}