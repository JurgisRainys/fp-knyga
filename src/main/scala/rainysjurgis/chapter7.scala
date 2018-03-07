package rainysjurgis

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference

import scala.concurrent.duration.TimeUnit

object chapter7 {

  type Par[A] = ExecutorService => Future[A]

  object Par {
    def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true

      def get(timeout: Long, units: TimeUnit): A = get

      def isCancelled = false

      def cancel(eventIfRunning: Boolean): Boolean = false
    }

    case class Map2Future[A,B,C](a: Future[A], b: Future[B],
                                 f: (A,B) => C) extends Future[C] {
      @volatile var cache: Option[C] = None
      val isDone: Boolean = cache.isDefined
      val isCancelled: Boolean = a.isCancelled || b.isCancelled
      def cancel(evenIfRunning: Boolean): Boolean =
        a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
      val get: C = compute(Long.MaxValue)
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

    def map2WithTimeouts[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
      es: ExecutorService => {
        val (af, bf) = (a(es), b(es))
        Map2Future(af, bf, f)
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

    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = {
      def run[A](es: ExecutorService)(p: Par[A]):A={
        val ref = new AtomicReference[A]
        val latch = new CountDownLatch(1)
        p(es) { a => ref.set(a); latch.countDown }
        latch.await
        ref.get
      }
    }

    def fork[A](a: => Par[A]): Par[A] = es => {
      es.submit(new Callable[A] {
        def call = a(es).get
      })
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

    def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
      p(e).get == p2(e).get
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
    val result = Par.map2WithTimeouts(sleep, sleep)((num1, num2) => num1 + num2)
    println(result(executor))

    val a = Par.lazyUnit(42 + 1)
    val S = Executors.newFixedThreadPool(1)
    println(Par.equal(S)(a, Par.fork(a)))
  }
}
