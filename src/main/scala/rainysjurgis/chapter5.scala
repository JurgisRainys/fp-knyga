package rainysjurgis

object chapter5 {
  sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, _) => Some(h())
    }


    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def headOption2: Option[A] = {
      foldRight(Option.empty[A])((a, _) => Some(a))
    }

    def foldLeft[B](z: => B)(f: (B, => A) => B): B = this match {
      case Empty => z
      case Cons(h, t) =>
        val b1 = f(z, h())
        t().foldLeft(b1)(f)
    }

    def toList: List[A] = foldRight(List.empty[A])(_ :: _)

    def take(n: Int): Stream[A] = {
      def loop(stream: Stream[A], n: Int, acc: Stream[A]): Stream[A] = stream match {
        case Cons(h, t) if n > 0 => loop(t(), n - 1, Stream.cons(h(), acc))
        case _ => acc
      }

      loop(this, n, Empty)
    }

    def take2(n: Int): Stream[A] = {
      foldLeft((Empty: Stream[A], n)) {
        case ((stream, 0), _) => (stream, 0)
        case ((stream, _n), x) => (Stream.cons(x, stream), _n - 1)
      }._1
    }

    def takeWithUnfold(n: Int): Stream[A] = {
      Stream.unfold((this, n)){
        case (Cons(h, t), _n) if _n > 0 => Some(h(), (t(), _n - 1))
        case _ => None
      }
    }

    def drop(n: Int): Stream[A] = {
      def loop(stream: Stream[A], n: Int, acc: Stream[A]): Stream[A] = stream match {
        case Cons(_, t) if n > 0 => loop(t(), n - 1, acc)
        case Cons(h, t) => loop(t(), n, Stream.cons(h(), acc))
        case _ => acc
      }

      loop(this, n, Empty)
    }

    def drop2(n: Int): Stream[A] = {
      foldLeft((Empty: Stream[A], n))((acc, h) =>
        if (acc._2 > 0) (acc._1, acc._2 - 1) else (Stream.cons(h, acc._1), acc._2 - 1))._1
    }

    def takeWhile(p: A => Boolean): Stream[A] = {
      def loop(stream: Stream[A], acc: Stream[A]): Stream[A] = stream match {
        case Cons(h, t) if p(h()) => loop(t(), Stream.cons(h(), acc))
        case _ => acc
      }

      loop(this, Empty)
    }

    def takeWhileFoldLeft(p: A => Boolean): Stream[A] = {
      foldLeft(Empty: Stream[A], true) { (acc, h) =>
        if (acc._2) if (p(h)) (Stream.cons(h, acc._1), true) else (acc._1, false) else acc
      }._1
    }

    def takeWhileFoldRight(p: A => Boolean): Stream[A] = {
      foldRight(Empty: Stream[A])((h, acc) =>
        if (p(h)) Stream.cons(h, acc) else Empty)
    }

    def takeWhileWithUnfold(p: A => Boolean): Stream[A] = {
      Stream.unfold(this){
        case Cons(h, t) if p(h()) => Some(h(), t())
        case _ => None
      }
    }

    def exists(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

    def exists2(p: A => Boolean): Boolean = {
      foldRight(false)((a, b) => p(a) || b)
    }

    def forAll(p: A => Boolean): Boolean = this match {
      case Cons(h, t) if p(h()) => t().forAll(p)
      case Empty => true
      case _ => false
    }

    def forAll2(p: A => Boolean): Boolean = {
      foldRight(true)((a, acc) => if (p(a)) acc else false)
    }

    def map[B](f: A => B): Stream[B] = {
      foldRight(Empty: Stream[B])((h, acc) => Stream.cons(f(h), acc))
    }

    def mapWithUnfold[B](f: A => B): Stream[B] = {
      Stream.unfold(this) {
        case Empty => None
        case (Cons(h, t)) => Some(f(h()), t())
      }
    }

    def flatMap[B](f: A => Stream[B]): Stream[B] = {
        foldRight(Empty: Stream[B])((h, acc) =>
          f(h).foldRight(acc)((h2, acc) => Stream.cons(h2, acc)))
    }

    def filter(f: A => Boolean): Stream[A] = {
      foldRight(Empty: Stream[A])((h, acc) => if (f(h)) Stream.cons(h, acc) else acc)
    }

    def append[AA >: A](otherList: => Stream[AA]): Stream[AA] = {
      foldRight(otherList)((h, acc) => Stream.cons(h, acc))
    }

    def zipAll[B](otherStream: Stream[B]): Stream[(Option[A],Option[B])] = {
      Stream.unfold(this, otherStream) {
        case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Empty))
        case (Empty, Cons(h, t)) => Some((None, Some(h())), (Empty, t()))
        case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
        case _ => None
      }
    }

    def tails: Stream[Stream[A]] = {
      Stream.unfold(this){
        case Cons(_, t1) => Some(Stream.unfold(t1()){
          case Cons(h2, t2) => Some(h2(), t2())
          case _ => None
        }, t1())
        case _ => None
      }
    }

    def startsWith[AA >: A](otherStream: Stream[AA]): Boolean = {
      !Stream.unfold(this, otherStream){
        case (Cons(h1, t1), Cons(h2, t2)) if h1() == h2() => Some(true, (t1(), t2()))
        case (Empty, Empty) => None
        case (_, Empty) => Some(true, (Empty, Empty))
        case _ => Some(false, (Empty, Empty))
      }.exists2(x => x.equals(false))
    }

    def hasSubsequence[AA >: A](subStream: Stream[AA]): Boolean =
      (this startsWith subStream) || tails.exists2(_ startsWith subStream)

    def isEmpty: Boolean = this match {
      case Cons(_, _) => false
      case _ => true
    }
  }

  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    def infiniteStream[A](a: A)(f: A => A): Stream[A] = {
      Stream.cons(a, infiniteStream(f(a))(f))
    }

    def constant[A](a: A): Stream[A] = {
//      Stream.cons(a, constant(a))
      infiniteStream(a)(identity)
    }

    def from(n: Int): Stream[Int] = {
//      Stream.cons(n, from(n + 1))
      infiniteStream(n)(_ + 1)
    }

    def fibs: Stream[Int] = {
      def generate(olderNum: Int, newerNum: Int): Stream[Int] =
        Stream.cons(olderNum + newerNum, generate(newerNum, olderNum + newerNum))

      Stream.cons(0, Stream.cons(1, Empty)).append(generate(0, 1))
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
      f(z) match {
        case Some((a, newState)) => Stream.cons(a, unfold(newState)(f))
        case None => Empty
      }
    }

    def constant3[A](a: A): Stream[A] =
      unfold(a)(state => Some(a, state))

    def from3(n: Int): Stream[Int] =
      Stream(n).append(unfold(n)(state => Some(state + 1, state + 1)))

    def fib2: Stream[Int] =
      Stream(0, 1).append(
        unfold((0, 1)){
          case (olderNum, newerNum) =>
          Some(newerNum + olderNum, (newerNum, newerNum + olderNum))
        }
      )
  }

  def test(): Unit = {
    val s1 = Stream(1, 2, 3, 4, 5)
    val s2 = Stream(6, 7)
//    println(s1.drop(3).toList)
//    println(s1.drop2(3).toList)
//    println(s1.takeWhile(_ < 2).toList)
//    println(s1.takeWhileWithUnfold(_ < 3).toList)
//    println(s1.takeWhile3(_ > 2).toList)
//    println(s1.forAll(_ <= 5))
//    println(s1.forAll2(_ <= 5))
//    println(s1.headOption)
//    println(s1.headOption2)
//    println(s1.map(_ + 10).toList)
//    println(s1.flatMap(x => Stream.apply(-x, 10 + x, x)).toList)
//    println(s1.filter(_ > 2).toList)
//    println(s1.append(s2).toList)
//    println(Stream.constant("xd").take(5).toList)
//    println(Stream.from2(2).take(5).toList)
//    println(Stream.from3(2).take(5).toList)
////    println(Stream.fibs.take(5).toList)
//    println(Stream.constant3("xd").take(5).toList)
//    println(Stream.fib2.takeWithUnfold(7).toList)
//    println(Stream.constant3("xd").take(7).mapWithUnfold(_ + "lol").toList)
//    println(s1.zipAll(s2).toList)
//    println(s1.hasSubsequence(Stream(1)))
    println(s1.hasSubsequence(Stream(1, 2, 3, 4)))
    println(s1.tails.map(_.toList).toList)
  }
}
