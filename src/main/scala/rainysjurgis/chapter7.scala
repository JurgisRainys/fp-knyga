package rainysjurgis

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference

import scala.concurrent.duration.TimeUnit

object chapter7 {

  sealed trait Future[A] {
    private[rainysjurgis] def apply(k: A => Unit): Unit
  }

  type Par[A] = ExecutorService => Future[A]

  object Par {
    def unit[A](a: A): Par[A] =
      es => new Future[A] {
        def apply(cb: A => Unit): Unit =
          cb(a)
      }

    def fork[A](a: => Par[A]): Par[A] = es =>
      new Future[A] {
        def apply(cb: A => Unit): Unit =
          eval(es)(a(es)(cb))
      }

    def eval(es: ExecutorService)(r: => Unit): Unit =
      es.submit(new Callable[Unit] { def call = r })

    def run[A](es: ExecutorService)(p: Par[A]): A= {
      val ref = new AtomicReference[A]
      val latch = new CountDownLatch(1)
      p(es) { a => ref.set(a); latch.countDown }
      latch.await
      ref.get
    }

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def map[A, B](pa: Par[A])(f: A => B): Par[B] = {
      map2(pa, unit(()))((a, _) => f(a))
    }

    def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] = {
      es: ExecutorService => new Future[C] {
        def apply(cb: C => Unit): Unit = {
          var ar: Option[A] = None
          var br: Option[B] = None
          val combiner = Actor[Either[A,B]](es) {
            case Left(a) => br match {
              case None => ar = Some(a)
              case Some(b) => eval(es)(cb(f(a, b)))
            }
            case Right(b) => ar match {
              case None => br = Some(b)
              case Some(a) => eval(es)(cb(f(a, b)))
            }
          }
          p(es)(a => combiner ! Left(a))
          p2(es)(b => combiner ! Right(b))
        }
      }
    }

    def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = {
      map2(a, map2(b, c)((_, _))) { case (a, (b, c)) => f(a, b, c) }
    }

    def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] = {
      map2(map2(a, b)((_, _)), map2(c, d)((_, _))) { case ((a, b), (c, d)) => f(a, b, c, d) }
    }

    def map5[A, B, C, D, E, F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A, B, C, D, E) => F): Par[F] = {
      map2(
        map2(a, b)((_, _)),
        map2(
          c,
          map2(d, e)((_, _)))
        ((_, _))
      ) {
        case ((a, b), (c, (d, e))) => f(a, b, c, d, e)
      }
    }

    def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

    def asyncF[A, B](f: A => B): A => Par[B] = {
      a => lazyUnit(f(a))
    }

    def sortPar(parList: Par[List[Int]]): Par[List[Int]] = {
      map(parList)(_.sorted)
    }

    def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
      ps.foldRight(unit(List.empty[A]))((h, acc) =>
        map2(acc, h)((list, head) => head :: list)
      )
    }

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
      fork(sequence(ps.map(asyncF(f))))
    }

    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
      fork(sequence(as.filter(a => f(a)).map(asyncF(identity))))
    }
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum2(l)), Par.fork(sum2(r)))(_ + _)
    }

  def sum2(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else
      Par.map(Par.unit(ints.toList))(_.sum)

  def wordCount(paragraphs: List[String]): Par[Int] = {
    val wordCounts = Par.parMap(paragraphs)(_.split(" |\\n").length)
    Par.map(wordCounts)(_.sum)
  }

  def sleep: Par[Int] = {
    Thread.sleep(900)
    Par.unit(1)
  }

  def test: Unit = {
    val executor = Executors.newFixedThreadPool(10)
    val list = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val list2 = List("lol xd\nxd", "xd xd xd", "xd")
    val result = Par.map2(sleep, Par.fork(Par.fork(sleep)))((num1, num2) => num1 + num2)
    println(result(executor))

    val a = Par.lazyUnit(42 + 1)
    val S = Executors.newFixedThreadPool(1)
    println(result(S).apply(a => println(a)))
  }
}
