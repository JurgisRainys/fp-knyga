package rainysjurgis
import scala.annotation.tailrec

object chapter3 {
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B) : B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
      @tailrec
      def loop(list: List[A], acc: B): B = list match {
        case Nil => acc
        case Cons(h, t) => loop(t, f(acc, h))
      }

      loop(as, z)
    }

    def sum(ns: List[Int]) =
      foldRight(ns, 0)((x, y) => x + y)

    def sumFoldLeft(ns: List[Int]) =
      foldLeft(ns, 0)((x, y) => x + y)

    def Ex38() =
      foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))

    def product(ns: List[Double]) =
      foldRight(ns, 1.0)(_ * _)

    def productFoldLeft(ns: List[Double]) =
      foldLeft(ns, 1.0)(_ * _)

    def tail[A](list: List[A]): List[A] = list match {
      case Nil => List[A]()
      case Cons(_, t) => t
    }

    def setHead[A](list: List[A], newVal: A): List[A] = list match {
      case Nil => List[A]()
      case Cons(_, t) => Cons(newVal, t)
    }

    def drop[A](list: List[A], n: Int): List[A] = {
      @tailrec
      def loop(n: Int, list: List[A]): List[A] =
        if (n <= 0) list
        else loop(n - 1, tail(list))

      loop(n, list)
    }

    //grazina lista is priesingos puses
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
      @tailrec
      def loop(l: List[A], resultList: List[A]): List[A] = l match {
        case Nil => resultList
        case Cons(h, t) => {
          if (f(h)) loop(t, resultList)
          else loop(t, Cons(h, resultList))
        }
      }

      loop(l, List[A]())
    }

    //grazina is geros puses, bet ne tailrec
    def dropWhile2[A](l: List[A])( f: A => Boolean): List[A] = {
      def loop(l: List[A], resultList: List[A]): List[A] = l match {
        case Nil => List[A]()
        case Cons(h, t) => {
          val prevRes = loop(t, resultList)
          if (f(h)) prevRes
          else Cons(h, prevRes)
        }
      }

      loop(l, List[A]())
    }

    def init[A](l: List[A]): List[A] = {
      @tailrec
      def loop(l: List[A], resultList: List[A]): List[A] = l match {
        case Nil => Nil
        case Cons(h, t) =>
          t match {
            case Nil => resultList
            case _ => loop(t, Cons(h, resultList))
          }
      }

      loop(l, List[A]())
    }

    def length[A](as: List[A]): Int = {
      foldRight(as, 0)((_, acc) => acc + 1)
    }

    def lengthFoldLeft[A](as: List[A]): Int = {
      foldLeft(as, 0)((acc, _) => acc + 1)
    }

    def reverse[A](list: List[A]): List[A] = {
      foldLeft(list, List[A]())((t, h) => (Cons(h, t)))
    }

    def append[A](list1: List[A], list2: List[A]) = {
      foldRight(list1, list2)((h, list) => Cons(h, list))
    }

    def mergeLists[A](list: List[List[A]]): List[A] = {
      foldLeft(list, List[A]())((h, acc) => append(h, acc))
    }

    def add1ToEach(list: List[Int]): List[Int] = {
      foldRight(list, List[Int]())((num, acc) => Cons(num + 1, acc))
    }

    def DoubleToStringList(list: List[Double]): List[String] = {
      foldRight(list, List[String]())((num, acc) => Cons(num.toString()+ "xd", acc))
    }

    def map[A, B](as: List[A])(f: A => B): List[B] = {
      foldRight(as, List[B]())((h, acc) => Cons(f(h), acc))
    }

    def filter[A](as: List[A])(f : A => Boolean): List[A] = {
      foldRight(as, List[A]())((h, acc) => if (f(h)) Cons(h, acc) else acc)
    }

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
      foldRight(as, List[B]())((h, acc) => foldRight(f(h), acc)(Cons(_, _)))
    }

    def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
      flatMap(as)(h => if (f(h)) List[A](h) else List[A]())
    }

    def zipWith[A](as1: List[A], as2: List[A])( f: (A, A) => A ): List[A] = {
      @tailrec
      def loop(as1: List[A], as2: List[A], acc: List[A]): List[A] = as1 match {
          case Nil => acc
          case Cons(h1, t1) =>
            as2 match {
              case Nil => acc
              case Cons(h2, t2) => {
                loop(t1, t2, Cons(f(h1, h2), acc))
              }
            }
        }

      loop(as1, as2, List[A]())
    }

    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
      def loop(sup: List[A], sub2: List[A], result: Boolean): Boolean = sup match {
        case Nil =>
          if (length(sub2) > 0) false
          else true
        case Cons(h, t) =>
          sub2 match {
            case Nil => result
            case _ =>
              if (length(filter(sub2)(_ == h)) > 0) loop(t, tail(sub2), true)
              else
                if (result) loop(sup, sub, result = false)
                else loop(t, sub, result = false)
          }

      }

      if (length(sup) >= length(sub))
        loop(sup, sub, result = false)
      else false
    }
  }

  def lowerThan5(num: Int): Boolean =
    num < 5

  def test() = {
    val ls = List(1, 7, 2, 3, 4, 6, 8)
    val ls2 = List(22, 22)
    val ls3 = List(33, 33)
    val ls4 = List(44, 44)
    val ls6 = List(44, 23, 11, 44, 44, 11)
    val ls5 = List(ls2, ls3, ls4)
    val lsd: List[Double] = List(1, 7, 2, 3, 4, 6, 8)
//    println(List.tail(ls))
//    println(List.setHead(ls, 4))
//    println(List.drop(ls, 2))
//    println(List.dropWhile(ls, lowerThan5))
//    println(List.dropWhile2(ls)(lowerThan5))
//    println(List.Ex38())
//    println(List.length(ls))
//    println(List.sum(ls))
//    println(List.sumFoldLeft(ls))
//    println(List.product(lsd))
//    println(List.productFoldLeft(lsd))
//    println(List.length(ls))
//    println(List.lengthFoldLeft(ls))
//    println(List.append(ls, ls2))
//    println(List.mergeLists(ls5))
//    println(List.add1ToEach(ls))
//    println(List.DoubleToStringList(lsd))
//    println(List.map(lsd)(_.toString() + "xd"))
//    println(List.DoubleToStringList(lsd))
//    println(List.filter(ls)(lowerThan5(_)))
//    println(List.filter2(ls)(lowerThan5(_)))
//    println(List.flatMap(List(1,2,3))(i => List(i,i)))
//    println(List.zipWith(ls3, ls4)(_ + _))
    println(List.hasSubsequence(ls6, ls4))
  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  
  object Tree {
    def size[A](tree: Tree[A]): Int = {

    }
  }
}
