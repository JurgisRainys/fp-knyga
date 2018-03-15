package rainysjurgis

import java.util.concurrent._

import scala.concurrent.duration.TimeUnit

object chapter7 {

  type Par[A] = ExecutorService => Future[A]

  object Par {
    def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

    def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
      Par.map2(p,p2)(_ == _)

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true

      def get(timeout: Long, units: TimeUnit): A = get

      def isCancelled = false

      def cancel(eventIfRunning: Boolean): Boolean = false
    }

    case class Map2Future[A,B,C](
      a: Future[A], b: Future[B], f: (A,B) => C
    ) extends Future[C] {
      @volatile var cache: Option[C] = None
      def isDone: Boolean = cache.isDefined
      def isCancelled: Boolean = a.isCancelled || b.isCancelled
      def cancel(evenIfRunning: Boolean): Boolean =
        a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
      def get: C = compute(Long.MaxValue)
      def get(timeout: Long, units: TimeUnit): C =
        compute(TimeUnit.MILLISECONDS.convert(timeout, units))

      private def compute(timeoutMs: Long): C = cache match {
        case Some(c) => c
        case None =>
          val start = System.currentTimeMillis
          val ar = a.get(timeoutMs, TimeUnit.MILLISECONDS)
          val stop = System.currentTimeMillis; val at = stop-start
          val br = b.get(timeoutMs - at, TimeUnit.MILLISECONDS)
          val ret = f(ar, br)
          cache = Some(ret)
          ret
      }
    }

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def map[A, B](pa: Par[A])(f: A => B): Par[B] = {
      map2(pa, unit(()))((a, _) => f(a))
    }

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
      es: ExecutorService => {
        val af = a(es)
        val bf = b(es)
        UnitFuture(f(af.get, bf.get))
      }
    }

    def map2WithFlatMap[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = {
      flatMap(pa)(a => flatMap(pb)(b => unit(f(a, b))))
    }

    def map2WithTimeouts[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
      es: ExecutorService => {
        val (af, bf) = (a(es), b(es))
        Map2Future(af, bf, f)
      }
    }

    def map3[A, B, C, D](aP: Par[A], bP: Par[B], cP: Par[C])(f: (A, B, C) => D): Par[D] = {
      map2(aP, map2(bP, cP)((_, _))) { case (a, (b, c)) => f(a, b, c) }
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

    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

    def fork[A](a: => Par[A]): Par[A] = es => {
      es.submit(new Callable[A] {
        def call = a(es).get
      })
    }

    def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

    // choices //

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      choiceN(map(cond)(x => if (x) 0 else 1))(List(t, f))

    def choiceChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      flatMap(cond)(if (_) t else f)

    def choiceMapChooser[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
      flatMap(key)(choices(_))

    def choiceMapChooserSafe[K, V](keyPar: Par[K])(choices: Map[K, Par[V]]): Par[Option[V]] =
      flatMap(keyPar) { key =>
        choices.get(key) match {
          case Some(par) => map(par)(Some.apply)
          case None => unit(None)
        }
      }

    // def choiceN[A, N <: Nat](n: Par[0..N])(choices: List[Par[A], N]): Par[A] = {
    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
      es => {
        val ind = run(es)(n).get
        choices(ind)(es)
      }
    }

    def choiceNChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
      flatMapUsingJoin(n)(choices(_))
    }

    // choices end //

    def flatMap[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = {
      es => {
        val a = run(es)(pa).get
        choices(a)(es)
      }
    }

    def flatMapUsingJoin[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = {
      joinUsingFlatMap(map(pa)(a => choices(a)))
    }

    def join[A](a: Par[Par[A]]): Par[A] = {
      es => {
        val parA = run(es)(a).get
        parA(es)
      }
    }

    def joinUsingFlatMap[A](a: Par[Par[A]]): Par[A] = {
      flatMap(a)(identity)
    }

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

    def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
      p(e).get == p2(e).get
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
    val result = Par.choiceNChooser(Par.unit(1))(List(Par.unit(1), Par.unit(2), Par.unit(3)))
    println(result(executor))
  }
}
