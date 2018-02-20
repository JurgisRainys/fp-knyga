package rainysjurgis

object chapter3 {
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }
    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def tail[A](list: List[A]): List[A] = list match {
      case Nil => List[A]()
      case Cons(_, t) => t
    }

    def setHead[A](list: List[A], newVal: A): List[A] = list match {
      case Nil => List[A]()
      case Cons(_, t) => Cons(newVal, t)
    }

    def drop[A](list: List[A], n: Int): List[A] = {
      @annotation.tailrec
      def loop(n: Int, list: List[A]): List[A] =
        if (n <= 0) list
        else loop(n - 1, tail(list))

      loop(n, list)
    }

//    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
//      @annotation.tailrec
//      def loop[A](l: List[A], resultList: List[A]) = l match {
//        case Nil => resultList
//        case Cons(h, t) => {
//          if ()
//        }
//      }
//
//    }
  }

  def test() = {
    val ls =List(1, 2, 3, 4)
//    println(List.tail(ls))
//    println(List.setHead(ls, 4))
//    println(List.drop(ls, 2))

  }
}
