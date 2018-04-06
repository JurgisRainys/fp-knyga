package rainysjurgis
import scala.annotation.tailrec

object chapter3 {
  sealed trait List[+A] {
    def isEmpty: Boolean = this match {
      case Nil => true
      case _ => false
    }

    def head: Option[A] = this match {
      case Nil => None
      case Cons(head, _) => Some(head)
    }

    def tail: List[A] = this match {
      case Nil => Nil
      case Cons(_, t) => t
    }

    def isLongerThan[B](other: List[B]): Boolean = {
      @tailrec
      def loop(thisList: List[A], otherList: List[B]): Boolean = (thisList, otherList) match {
        case (Nil, _) => false
        case (_, Nil) => true
        case _ => loop(thisList.tail, otherList.tail)
      }

      loop(this, other)
    }

    def print: String = {
      List.foldLeft(this, "")(_ + " " + _)
    }
  }
  case object Nil extends List[Nothing]
  case class Cons[+A](head_ : A, tail_ : List[A]) extends List[A]

  object List {
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def empty[A]: List[A] = Nil

    def listFromSeq[A](s: Seq[A]): List[A] = {
      @tailrec
      def loop[B](s: Seq[B], acc: List[B]): List[B] = {
        if (s.isEmpty) acc
        else loop(s.tail, Cons(s.head, acc))
      }

      loop(s, List.empty[A])
    }

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

    def sum(ns: List[Int]): Int =
      foldRight(ns, 0)((x, y) => x + y)

    def sumFoldLeft(ns: List[Int]): Int =
      foldLeft(ns, 0)((x, y) => x + y)

    def Ex38(): List[Int] =
      foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))

    def product(ns: List[Double]) =
      foldRight(ns, 1.0)(_ * _)

    def productFoldLeft(ns: List[Double]) =
      foldLeft(ns, 1.0)(_ * _)

    def setHead[A](list: List[A], newVal: A): List[A] = list match {
      case Nil => empty
      case Cons(_, t) => Cons(newVal, t)
    }

    def drop[A](list: List[A], n: Int): List[A] = {
      @tailrec
      def loop(n: Int, list: List[A]): List[A] =
        if (n <= 0) list
        else loop(n - 1, list.tail)

      loop(n, list)
    }

    // TODO FIXME
    def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
      @tailrec def loop(l: List[A]): List[A] = l match {
        case Cons(h, t) if f(h) => loop(t)
        case _ => l
      }

      loop(l)
    }

    def init[A](l: List[A]): List[A] = {
      @tailrec
      def loop(l: List[A], resultList: List[A]): List[A] = l match {
        case Nil => Nil
        case Cons(_, Nil) => resultList
        case Cons(h, t) => loop(t, Cons(h, resultList))
      }

      loop(l, empty)
    }

    def length[A](as: List[A]): Int = {
      foldRight(as, 0)((_, acc) => acc + 1)
    }

    def lengthFoldLeft[A](as: List[A]): Int = {
      foldLeft(as, 0)((acc, _) => acc + 1)
    }

    def reverse[A](list: List[A]): List[A] = {
      foldLeft(list, List[A]())((t, h) => Cons(h, t))
    }

    def append[A](list1: List[A], list2: List[A]): List[A] = {
      foldRight(list1, list2)(Cons.apply)
    }

    def mergeLists[A](list: List[List[A]]): List[A] = {
      foldLeft(list, empty[A])(append)
    }

    def add1ToEach(list: List[Int]): List[Int] = {
      foldRight(list, List[Int]())((num, acc) => Cons(num + 1, acc))
    }

    def DoubleToStringList(list: List[Double]): List[String] = {
      foldRight(list, List[String]())((num, acc) => Cons(num.toString, acc))
    }

    def map[A, B](as: List[A])(f: A => B): List[B] = {
      foldRight(as, List[B]())((h, acc) => Cons(f(h), acc))
    }

    def filter[A](as: List[A])(f : A => Boolean): List[A] = {
      foldRight(as, List[A]())((h, acc) => if (f(h)) Cons(h, acc) else acc)
    }

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
      foldRight(as, List[B]())((h, acc) => foldRight(f(h), acc)(Cons.apply))
    }

    def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
      flatMap(as)(h => if (f(h)) List[A](h) else List[A]())
    }


    def zipWith[A](as1: List[A], as2: List[A])( f: (A, A) => A ): List[A] = {
      @tailrec
      def loop(as1: List[A], as2: List[A], acc: List[A]): List[A] =
        (as1, as2) match {
          case (Nil, _) | (_, Nil) => acc
          case (Cons(h1, t1), Cons(h2, t2)) => loop(t1, t2, Cons(f(h1, h2), acc))
        }

      loop(as1, as2, List[A]())
    }

    // TODO FIXME
    def hasSubsequence2[A](sup: List[A], sub: List[A]): Boolean = {
      def loop(sup: List[A], sub2: List[A], result: Boolean): Boolean = sup match {
        case Nil => sub2.isEmpty
        case Cons(h, t) => sub2 match {
            case Nil => result
            case _ =>
              if (!filter(sub2)(_ == h).isEmpty) loop(t, sub2.tail, true)
              else if (result) loop(sup, sub, result = false)
              else loop(t, sub, result = false)
          }
      }

      if (sup.isLongerThan((sub)))
        loop(sup, sub, result = false)
      else false
    }

    def hasSubsequence3[A](list: List[A], sublist: List[A]): Boolean = {
      def loop(list: List[A], needsToMatch: List[A], toCheckUponFailure: List[A]): Boolean =
        (list, needsToMatch) match {
          case (_, Nil) => true
          case (Nil, _) => false
          case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => loop(t1, t2, toCheckUponFailure)
          case _ => loop(toCheckUponFailure, sublist, toCheckUponFailure.tail)
        }

      if (list.isLongerThan(sublist))
        loop(list, sublist, list.tail)
      else
        false
    }
  }

  def lowerThan5(num: Int): Boolean =
    num < 5

  def test() = {
    val ls = List(1, 2, 2, 3, 4, 6, 8)
    val ls2 = List(22, 22)
    val ls3 = List(33, 33)
    val ls4 = List(44, 44)
    val ls6 = List(44, 23, 11, 44, 44, 11, 44)
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
//    println(List.hasSubsequence3(ls6, ls4))
//    println(List.hasSubsequence2(ls6, ls4))
      println(ls.print)
      println(List.dropWhile(ls)(lowerThan5).print)
  }

  sealed trait Tree[+A] {
//    def printTree(): String = {
//      Tree.iterateTree(this)(_.toString)(_ + " :: " + _)
//    }
  }
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
//  case object EmptyTree extends Tree[Nothing]

//  object Tree {
//    @tailrec
//    def generateTree[A](treeList: List[Tree[A]]): Tree[A] = {
//      treeList match {
//        case Cons(_, Cons(_, _)) => generateTree(makeBranches(treeList, List.empty[Tree[A]]))
//        case Cons(h, _) => h
//        case Nil => EmptyTree
//      }
//    }
//
//    @tailrec
//    def makeBranches[A](treeList: List[Tree[A]], resultBranches: List[Tree[A]]): List[Tree[A]] = {
//      treeList match {
//        case Cons(a1, Cons(a2, tail)) => makeBranches(tail, Cons(Branch(a1, a2), resultBranches))
//        case Cons(a1, Nil) => Cons(Branch(a1, EmptyTree), resultBranches)
//        case Nil => resultBranches
//      }
//    }
//
//    def apply[A](as: A*): Tree[A] = {
//      if (as.isEmpty) EmptyTree
//      else {
//        val leaves = List.map(List.listFromSeq(as))(Leaf(_))
//        generateTree(leaves)
//      }
//    }
//
//    def iterateTree[A, B](tree: Tree[A])(mapper: A => B)(joiner: (B, B) => B): B = {
//      def loop(tree: Tree[A]): B = tree match {
//        case EmptyTree => ???
//        case Leaf(x) => mapper(x)
//        case Branch(EmptyTree, right) => loop(right)
//        case Branch(left, EmptyTree) => loop(left)
//        case Branch(left, right) => joiner(loop(left), loop(right))
//      }
//
//      loop(tree)
//    }
//
//    def size[A](tree: Tree[A]): Int = {
//      iterateTree(tree)(_ => 1)((x, y) => x + y)
//    }
//
//    def maximum(tree: Tree[Int]): Int = {
//      iterateTree(tree)(identity)((x, y) => if (x > y) x else y)
//    }
//
//    def depth[A](tree: Tree[A]): Int = {
//      iterateTree(tree)(_ => 0)((x, y) => (if (x > y) x else y) + 1)
//    }
//
//    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
//      iterateTree(tree)(x => Tree(f(x)))(Branch.apply)
//    }
//
//    def test2() = {
//      val t0 = Tree.apply(1, 2, 3)
//      val t1 = Tree.apply(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
//      val t2 = Tree.apply(1, 12, 3, 4, 5, 6, 7, 8, 9, 10, 2, 6, 4, 6, 11, 2, 6)
////      println(Tree.maximum(t2))
////      println(Tree.size(t1))
////      println(Tree.size2(t1))
//      //println(Tree.depth(t2))
////      println(t0.printTree())
////      println(Tree.map(t0)(_ + 10).printTree())
//    }
//  }
}
