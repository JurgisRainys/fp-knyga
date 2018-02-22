package rainysjurgis
import scala.annotation.tailrec

object chapter3 {
  sealed trait List[+A] {
    def isEmpty: Boolean = this match {
      case Nil => true
      case _ => false
    }

    def isLongerThan[B](other: List[B]): Boolean = {
      @tailrec
      def loop(thisList: List[A], otherList: List[B]): Boolean = {
        val h1 = List.head(thisList)
        val h2 = List.head(otherList)
        if (h1.isEmpty || h2.isEmpty) !h1.isEmpty
        else loop(List.tail(thisList), List.tail(otherList))
      }

      loop(this, other)
    }

  }
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def listFromSeq[A](s: Seq[A]): List[A] = {
      @tailrec
      def loop[B](s: Seq[B], acc: List[B]): List[B] = {
        if (s.length <= 0) acc
        else loop(s.tail, Cons(s.head, acc))
      }

      loop(s, List[A]())
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

    def head[A](list: List[A]): Option[A] = list match {
      case Nil => None
      case Cons(h, _) => Some(h)
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
        case Nil => sub2.isEmpty
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

    def hasSubsequence2[A](sup: List[A], sub: List[A]): Boolean = {
      def loop(sup: List[A], sub2: List[A], result: Boolean): Boolean = sup match {
        case Nil => sub2.isEmpty
        case Cons(h, t) => sub2 match {
            case Nil => result
            case _ =>
              if (!(filter(sub2)(_ == h)).isEmpty) loop(t, tail(sub2), true)
              else
              if (result) loop(sup, sub, result = false)
              else loop(t, sub, result = false)
          }
      }

      if (sup.isLongerThan((sub)))
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
    val ls4 = List(44, 44, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
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
    println(List.hasSubsequence(ls6, ls4))
    println(List.hasSubsequence2(ls6, ls4))
  }

  sealed trait Tree[+A] {
    def isEmptyTree(): Boolean = this match {
      case EmptyTree => true
      case _ => false
    }

    def printTree(): String = {
      Tree.iterateTree(this)(x => x.toString())(_ + " :: " + _)
    }
  }
  case object EmptyTree extends Tree[Nothing]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    def generateTree[A](treeList: List[Tree[A]]): Tree[A] = {
      if (List.length(treeList) > 1) generateTree(makeBranches(treeList, List[Tree[A]]()))
      else treeList match {
        case Cons(h, t) => h
        case Nil => EmptyTree
      }
    }

    @tailrec
    def makeBranches[A](treeList: List[Tree[A]], resultBranches: List[Tree[A]]): List[Tree[A]] = {
      val left = List.head(treeList)
      left match {
        case None => resultBranches
        case Some(leftBranch) =>
          val right = List.head(List.tail(treeList))
          right match {
            case None => Cons(Branch(leftBranch, EmptyTree), resultBranches)
            case Some(rightBranch) =>
              makeBranches(List.tail(List.tail(treeList)), Cons(Branch(leftBranch, rightBranch), resultBranches))
          }
      }
    }

    def apply[A](as: A*): Tree[A] = {
      if (as.isEmpty) EmptyTree
      else {
        val leaves = List.map(List.listFromSeq(as))(Leaf(_))
        generateTree(leaves)
      }
    }

    def iterateTree[A, B](tree: Tree[A])(f1: A => B)(f2: (B, B)=> B): B = {
      def loop(tree: Tree[A]): B = tree match {
        case Leaf(x) => f1(x)
        case Branch(left, right) =>
          if (left.isEmptyTree()) loop(right)
          else if (right.isEmptyTree()) loop(left)
          else f2(loop(left),loop(right))
      }

      loop(tree)
    }

    def size[A](tree: Tree[A]): Int = {
      def loop(tree: Tree[A]): Int = tree match {
        case Leaf(_) => 1
        case Branch(left, right) =>
          if (left.isEmptyTree()) loop(right)
          else if (right.isEmptyTree()) loop(left)
          else loop(left) + loop(right)
        }

      loop(tree)
    }

    def size2[A](tree: Tree[A]): Int = {
      iterateTree(tree)(_ => 1)((x, y) => x + y)
    }

    def maximum(tree: Tree[Int]): Int = {
      iterateTree(tree)(num => num)((x, y) => if (x > y) x else y)
    }

    def depth[A](tree: Tree[A]): Int = {
      iterateTree(tree)(_ => 0)((x, y) => if (x > y) (x + 1) else (y + 1))
    }

    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
      iterateTree(tree)(x => Tree(f(x)))((x, y) => Branch(x, y))
    }

    def test2() = {
      val t0 = Tree.apply(1, 2, 3)
      val t1 = Tree.apply(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      val t2 = Tree.apply(1, 12, 3, 4, 5, 6, 7, 8, 9, 10, 2, 6, 4, 6, 11, 2, 6)
//      println(Tree.maximum(t2))
//      println(Tree.size(t1))
//      println(Tree.size2(t1))
      //println(Tree.depth(t2))
//      println(t0.printTree())
//      println(Tree.map(t0)(_ + 10).printTree())

    }
  }
}
