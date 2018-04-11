package rainysjurgis

import java.util.concurrent.Executors

import rainysjurgis.chapter7.Par
import sun.reflect.generics.tree.Tree

object chapter10 {
  sealed trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
  }

  case class OrderableSeq[A](seq: IndexedSeq[A], isOrdered: Boolean)
  object OrderableSeq {
    def emptyTrue[A]: OrderableSeq[A] = OrderableSeq(IndexedSeq.empty, true)
    def emptyFalse[A]: OrderableSeq[A] = OrderableSeq(IndexedSeq.empty, false)
    def unit[A](a: A): OrderableSeq[A] = OrderableSeq(IndexedSeq(a), false)
  }

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  val stringMonoid: Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2
    val zero: String = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2
    val zero: List[A] = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2
    val zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2
    val zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    val zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    val zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    val zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a => {
      a1(a2(a))
    }
    override def zero: A => A = identity
  }

  def wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Stub(c), Stub(d)) => Stub(c + d)
      case (Stub(c), Part(l, w, r)) => Part(c + l, w, r)
      case (Part(l, w, r), Stub(c)) => Part(l, w, r + c)
      case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
        Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
    }

    override def zero: WC = Stub("")
  }

  def orderedInts: Monoid[OrderableSeq[Int]] = new Monoid[OrderableSeq[Int]] {
    override def op(a1: OrderableSeq[Int], a2: OrderableSeq[Int]): OrderableSeq[Int] = {
      if (a2.isOrdered) OrderableSeq(a1.seq(0) +: a2.seq, a1.seq(0) >= { if (a2.seq.isEmpty) Int.MinValue else a2.seq.max })
      else OrderableSeq.emptyFalse
    }
    override def zero: OrderableSeq[Int] = OrderableSeq.emptyTrue
  }

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new Monoid[Par[A]] {
    override def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)
    override def zero: Par[A] = Par.unit(m.zero)
  }

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    foldMapV(v, par(m))(a => Par.unit(f(a)))
  }

  import chapter8.{ Gen, Prop }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    Prop.forAll(gen.unsized)(a => m.op(a, m.zero) == m.op(m.zero, a))
  }

  def concatenate[A](list: List[A], m: Monoid[A]): A = {
    list.foldLeft(m.zero)(m.op)
  }

  def foldMap[A, B](list: List[A], m: Monoid[B])(f: A => B): B = {
    list.foldLeft(m.zero)((acc, h) => m.op(f(h), acc))
  }

  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = v.length match {
    case 0 => m.zero
    case 1 => f(v(0))
    case _ => {
      val (seq1, seq2) = v.splitAt(v.length / 2)
      m.op(foldMapV(seq1, m)(f), foldMapV(seq2, m)(f))
    }
  }

  def foldRight[A, Z](z: Z)(list: List[A])(f: (A, Z) => Z): Z = {
    foldMap(list, endoMonoid: Monoid[Z => Z])(listItem => zz => f(listItem, zz))(z)
  }

  def foldLeft[A, Z](z: Z)(list: List[A])(f: (A, Z) => Z): Z = list match {
    case Nil => foldMap(list, endoMonoid: Monoid[Z => Z])(listItem => zz => f(listItem, zz))(z)
    case h :: tail => f(h, foldLeft(z)(tail)(f))
  }

  def seqIsOrdered(seq: IndexedSeq[Int]) = {
    foldMap(seq.toList, orderedInts)(OrderableSeq.unit).isOrdered
  }

  def wordCount(str: String): Int = {
//    foldMapV(str.toIndexedSeq, wcMonoid)(a => Stub(a.toString))
    def wc(c: Char): WC =
      if (c.isWhitespace) Part("", 0, "")
      else Stub(c.toString)

    def unstub(s: String) = s.length.min(1)

    foldMapV(str.toIndexedSeq, wcMonoid)(wc) match {
      case Stub(s) => unstub(s)
      case Part(l, w, r) => unstub(l) + w + unstub(r)
    }
  }

  trait Foldable[F[_]] {
    def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B
    def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B
    def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B
    def concatenate[A](as: F[A])(m: Monoid[A]): A =
      foldLeft(as)(m.zero)(m.op)
  }

  case class FoldableList[A](list: List[A]) extends Foldable[List[A]] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
    override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = chapter10.foldMap(as, mb)(f)
  }

  case class FoldableSeq[A](seq: IndexedSeq[A]) extends Foldable[IndexedSeq[A]] {
    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B =  as.foldLeft(z)(f)
    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = chapter10.foldMap(as.toList, mb)(f)
  }

  case class FoldableStream[A](stream: Stream[A]) extends Foldable[Stream[A]] {
    override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)
    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)
    override def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B = as match {
      case h #:: tail => foldRight(tail)(f(h))((a, acc) => mb.op(f(a), acc))
    }
  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  case class FoldableTree[A](stream: Tree[A]) extends Foldable[Tree[A]] {
    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = 
    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = ???
    override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = ???
  }

  def test: Unit = {
    val gen = Gen.choose(0, 1000)
    val gen2 = Gen.choose(0, 1000)
    val tests =
      monoidLaws(intMultiplication, gen) ||
      monoidLaws(intAddition, gen) ||
      monoidLaws(optionMonoid: Monoid[Option[Int]], gen.map2(gen2)((a, a2) => if (a > a2) Some(a) else None)) ||
      monoidLaws(endoMonoid: Monoid[Int => Int], Gen.genFn(gen)(_ + _))


    Prop.run(tests)

    val es = Executors.newCachedThreadPool
    val list = List(1, 2, 3, 4, 5, 6)
    val t6_1 = foldRight(List.empty[Int])(list)((a, z) => a + 1 :: z)
    val t6_2 = foldLeft(List.empty[Int])(list)((a, z) => a + 1 :: z)
    val t7 = foldMapV(list.toIndexedSeq, endoMonoid: Monoid[List[Int] => List[Int]])(a => ls => a :: ls)(List.empty)
    val t8 = parFoldMap(list.toIndexedSeq, endoMonoid: Monoid[List[Int] => List[Int]])(a => ls => a :: ls)(es).get()(List.empty)

    val t9 = seqIsOrdered(list.toIndexedSeq)
    val t11 = wordCount("lorem ipsum dolor sit amet, 555555 11")

    println(t11)

  }
}
