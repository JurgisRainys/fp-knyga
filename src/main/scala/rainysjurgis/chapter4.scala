package rainysjurgis

object chapter4 {
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(a) => Some(f(a))
    }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(a) => f(a)
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(res) => res
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
      case Some(_) => this
      case None => ob
    }

    def filter(f: A => Boolean): Option[A] = this match {
      case Some(a) if f(a) => this
      case _ => None
    }
  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  object Option {
    def map2[A, B, C](aOpt: Option[A], bOpt: Option[B])(f: (A, B) => C): Option[C] =
      (aOpt, bOpt) match {
        case (Some(a), Some(b)) => Some(f(a, b))
        case _ => None
      }

    def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

    def sequence[A](aOptList: List[Option[A]]): Option[List[A]] = {
      aOptList.foldRight(Some(List.empty[A]): Option[List[A]]) { (aOpt, listOpt) =>
        for {
          list <- listOpt
          a <- aOpt
        } yield a :: list
      }
    }

    def traverse[A, B](aList: List[A])(f: A => Option[B]): Option[List[B]] = {
      //sequence(a map (f(_)))
      def loop(list: List[A], acc: List[B]): Option[List[B]] = list match {
        case Nil => Some(acc)
        case head :: tail => f(head) match {
          case None => None
          case Some(a) => loop(tail, a :: acc)
        }
      }

      loop(aList, List.empty)
    }

    def sequence2[A](a: List[Option[A]]): Option[List[A]] = {
      traverse(a)(_.map(identity))
    }

    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    def variance(xs: Seq[Double]): Option[Double] = {
      if (xs.isEmpty) None
      else {
        val m = mean(xs)
        mean(xs.map(x => m.flatMap(mm => Some(math.pow(x - mm, 2))).getOrElse(0.0)))
      }
    }

    def Try[A](a: => A): Option[A] =
      try Some(a)
      catch { case _: Exception => None }
  }

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case Right(_) => this
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
      case (err @ Left(_), _) => err
      case (_, err @ Left(_)) => err
      case (Right(r1), Right(r2)) => Right(f(r1, r2))
    }

    //    def mapp[B, EE >: E, C](f: A => C)(f2: C => Either[EE, B]): Either[EE, B] = this match {
    //      case Left(e) => Left(e)
    //      case Right(a) => f2(f(a))
    //    }
    //
    //    def map[B](f: A => B): Either[E, B] = {
    //      mapp(f)(Right(_))
    //    }
    //
    //    def flatMap[EE >: E,B](f: A => Either[EE, B]): Either[EE, B] =
    //      mapp(f)(x => x)
  }

  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  object Either {
    def mean(xs: IndexedSeq[Double]): Either[String, Double] =
      if (xs.isEmpty) Left("Mean of empty list!")
      else Right(xs.sum / xs.length)

    def safeDiv(x: Int, y: Int): Either[Exception, Int] =
      try Right(x / y)
      catch { case e: Exception => Left(e) }

    def Try[A](a: => A): Either[Exception, A] =
      try Right(a)
      catch { case e: Exception => Left(e)}

    def traverse[E, A, B](aList: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
      def loop(list: List[A], acc: List[B]): Either[E, List[B]] = list match {
        case Nil => Right(acc)
        case head :: tail => f(head) match {
          case Left(err) => Left(err)
          case Right(a) => loop(tail, a :: acc)
        }
      }

      loop(aList, List.empty)
    }

    def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] = {
      traverse(a)(_.map(identity))
    }
  }

  case class Person(name: Name, age: Age) {
    def mkName(name: String): Either[String, Name] =
      if (name == "" || name == null) Left("Name is empty.")
      else Right(new Name(name))

    def mkAge(age: Int): Either[String, Age] =
      if (age < 0) Left("Age is out of range.")
      else Right(new Age(age))

    def mkPerson(name: String, age: Int): Either[String, Person] =
      mkName(name).map2(mkAge(age))(Person)
  }
  sealed class Name(val value: String)
  sealed class Age(val value: Int)

  def test: Unit =  {
    //println(variance(List(1, 2, 3)).getOrElse(0))
    println(Either.sequence(List(Right(1), Right(2), Left("xd"), Right(3))))
//    println(Either.sequence(List(Some(1), Some(2), None, Some(3))))
//    println(Either.sequence2(List(Some(1), Some(2), None, Some(3))))

  }
}
