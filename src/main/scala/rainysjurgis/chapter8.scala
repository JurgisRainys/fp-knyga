package rainysjurgis

import java.util.concurrent.{ExecutorService, Executors, ThreadFactory}

import rainysjurgis.chapter6_newPC._
import rainysjurgis.chapter7._
import rainysjurgis.chapter8.Prop.{FailedCase, MaxSize, SuccessCount, TestCases, forAll}
import rainysjurgis.chapter8.Result.{Falsified, Passed, Proved}

import scala.util.Try

object chapter8 {
  // 8.2
  // if (list.filter(x => x > maximum(list)) == 0) && (list.filter(x => x == maximum(list)) != 0)

  // GEN START //

  object ** {
    def unapply[A, B](p: (A, B)): Option[(A, B)] = Some(p)
  }

  object IntE {
    def unapply(s: String) = Try(s.toInt).toOption
  }

  "123" match {
    case IntE(int) =>
    case other =>
  }

  case class Gen[A](sample: State[RNG, A]) {
    def flatMap[B](f: A => Gen[B]): Gen[B] = {
      Gen(this.sample.flatMap(a => f(a).sample))
    }

    def map[B](f: A => B): Gen[B] = {
      this.flatMap(a => Gen.unit(f(a)))
    }

    def map2[B, C](gb: Gen[B])(f: (A, B) => C): Gen[C] = {
//      ga.flatMap(a => gb.flatMap(b => Gen.unit(f(a, b))))
      for {
        a <- this
        b <- gb
      } yield f(a, b)
    }

    def option: Gen[Option[A]] = {
      map(Some.apply)
    }

    def listOfN(n: Int): Gen[List[A]] = {
      Gen(State.sequence(List.fill(n)(this.sample)))
    }

    def union(other: Gen[A]): Gen[A] = {
      Gen.boolean.flatMap(if (_) this else other)
    }

    def unsized: SGen[A] = SGen(_ => this)

    def **[B](g: Gen[B]): Gen[(A,B)] =
      this.map2(g)((_,_))
  }

  case class SGen[A](forSize: Int => Gen[A]) {
    def map[B](f: A => B): SGen[B] = {
      this.flatMap(a => SGen(_ => Gen.unit(f(a))))
    }

    def flatMap[B](f: A => SGen[B]): SGen[B] = {
      SGen { num => this.forSize(num).flatMap(a => f(a).forSize(num)) }
    }

    def **[B](genB: SGen[B]): SGen[(A, B)] =
      this.flatMap(a => genB.map(b => (a, b)))
  }

  object Gen {
    def choose(start: Int, stopExclusive: Int): Gen[Int] =
      Gen(SimpleRNG.numberInRange(start, stopExclusive))

    def chooseVector[A](v: Vector[A]) =
      choose(0, v.length).map(v.apply)

    def nestedPar(start: Int, stopExclusive: Int): Gen[Par[Int]] = {
      val (minDepth, maxDepth) = (1, 6)
      def nest(par: Par[Int], timesToNest: Int): Par[Int] =
        if (timesToNest > 0) nest(Par.fork(par), timesToNest - 1) else par

      choose(minDepth, maxDepth).flatMap(depth => {
        choose(start, stopExclusive).map(num => nest(Par.unit(num), depth))
      })
    }

    def fxInt[A](start: Int = 0, stopExclusive: Int = 100): Gen[A => Int] = {
      unit((arg: A) => {
        val hash = arg.hashCode
        val difference = stopExclusive - start
        (hash % difference) + start
      })
    }

    def sign[A](implicit a: Ordering[A]): Gen[(A, A) => Boolean] = {
      import scala.math.Ordering.Implicits._

      val functions = Vector[(A, A) => Boolean](
        (a1, a2) => infixOrderingOps(a1).<(a2),
        _ <= _,
        _ == _,
        _ != _,
        _ >= _,
        _ > _
      )

      Gen.chooseVector(functions)
    }

//    def fxIntBool[A](start: Int = 0, stopExclusive: Int = 100): Gen[Int => Boolean] = {
//      fxInt(start, stopExclusive).map(fxOuter => num => )
//    }
    def genFn[A, B](g: Gen[A])(f: (A, A) => B): Gen[A => B] =
      g.map(arg => genFunc => f(arg, genFunc))

    def unit[A](a: => A): Gen[A] = Gen(State.setArgument(a))

    def boolean: Gen[Boolean] = Gen.choose(0, 2).map(_ > 0)

    def sequence[A](fs: List[Gen[A]]): Gen[List[A]] = {
      fs.foldRight(unit(List.empty[A]))((genA, acc) => {
        genA.flatMap(a => acc.flatMap(list => unit(a :: list)))
      })
    }

    def listOf[A](sizeUpTo: Int, gen: Gen[A]): Gen[List[A]] =
      choose(0, sizeUpTo + 1).flatMap(size => sequence(List.fill(size)(gen)))

    val alphabeticChar: Gen[Char] = {
      val lowercaseStart = 65 // a - z : 65 - 90
      val uppercaseStart = 97 // A - Z : 97 - 122
      val letterCountInAlphabet = 26 // mazu && dideliu == 26; viso = 52

      choose(0, letterCountInAlphabet * 2).map { idx =>
        val asciiCode = idx + (
          if (idx < letterCountInAlphabet) lowercaseStart
          else uppercaseStart - letterCountInAlphabet
        )
        asciiCode.toChar
      }
    }

    def alphabeticString(length: Int): Gen[String] = {
//      listOf(alphabeticChar).map(asString)
      sequence(List.fill(length)(alphabeticChar)).map(asString)
    }

    def asString(l: TraversableOnce[Char]): String = l.foldLeft("")(_ + _)

    def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
      /* The probability we should pull from `g1`. */
      val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
      Gen(SimpleRNG.double).flatMap(d => Gen {
        if (d < g1Threshold) g1._1.sample else g2._1.sample
      })
    }
  }

  object SGen {
    def unit[A](a: A): SGen[A] = SGen {
      _ => Gen.unit(a)
    }

    def listOf[A](g: Gen[A]): SGen[List[A]] = {
      SGen { size => g.listOfN(size) }
    }

    def listOf1[A](g: Gen[A]): SGen[List[A]] = {
      SGen { size => g.listOfN(if (size < 1) 1 else size) }
    }
  }

  // GEN END //
  // PROP START //

  sealed trait Result {
    def isFalsified: Boolean
  }

  object Result {
    case object Passed extends Result {
      def isFalsified = false
    }

    case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
      def isFalsified = true
    }

    case object Proved extends Result {
      def isFalsified = false
    }
  }

  case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
    def changePassedToProved = Prop {
      this.run(_, _, _) match {
        case fail @ Falsified(_, _) => fail
        case _ => Proved
      }
    }

    def &&(p: Prop): Prop = Prop {
      (maxSize, testCases, rng) =>
        this.run(maxSize, testCases, rng) match {
          case Passed | Proved => p.run(maxSize, testCases, rng)
          case falsified @ _ => falsified
        }
    }

    def ||(p: Prop): Prop = Prop {
      (maxSize, testCases, rng) => this.run(maxSize, testCases, rng) match {
        case pass @ (Passed | Proved) => pass
        case Falsified(msg, _) => p.tag(msg).run(maxSize, testCases, rng)
      }
    }

    def tag(msg: String) = Prop {
      (max, n, rng) => run(max, n, rng) match {
        case Falsified(e, c) => Falsified(msg + "\n" + e, c)
        case x => x
      }
    }
  }

  object Prop {
    type MaxSize = Int
    type TestCases = Int
    type SuccessCount = Int
    type FailedCase = String

    def run(p: Prop,
            maxSize: Int = 100,
            testCases: Int = 100,
            rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit =
      p.run(maxSize, testCases, rng) match {
        case Falsified(msg, n) =>
          println(s"! Falsified after $n passed tests:\n $msg")
        case Passed =>
          println(s"+ OK, passed $testCases tests.")
        case Proved =>
          println(s"+ Ok, property proved.")
      }

    def check(p: => Boolean): Prop = Prop { (_, _, _) =>
      if (p) Proved else Falsified("()", 0)
    }

    def checkParBad(p: => Par[Boolean]): Prop = {
      forAllPar(S)(_ => p)
    }

    def checkParBetter(p: => Par[Boolean]): Prop =
      forAllMoreInner(S)(es => p(es).get).changePassedToProved

    def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAllInner(g.forSize)(f)

    def forAllInner[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
      (maxSize, testCases, rng) => {
        val casesPerSize = testCases
        //      val casesPerSize = (testCases + (maxSize - 1)) / maxSize

        val props: Stream[Prop] =
          Stream.from(0).take(maxSize).map(size => forAllMoreInner(g(size))(f))
//          Stream.from(0).take(testCases.min(maxSize)).map(i => forAllMoreInner(g(i))(f))

        val prop: Prop =
          props.map(p => Prop { (max, _, rng) =>
            p.run(max, casesPerSize, rng)
          }).toList.reduce(_ && _)

        prop.run(maxSize, testCases, rng)
      }
    }

    def forAllMoreInner[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
      (_, testCases,rng) =>
        randomStream(as)(rng)
          .zip(Stream.from(0))
          .take(testCases)
          .map { case (a, i) => getResult(a, i)(f) }
          .find(_.isFalsified)
          .getOrElse(Passed)
    }

    def forAllMoreInner2[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
      (_, testCases, rng) =>
        randomStream(as)(rng)
          .zip(Stream.from(0))
          .take(testCases)
          .map { case (a, i) => getResult(a, i)(f) }
          .find(_.isFalsified)
          .getOrElse(Passed)
    }

    def getResult[A](a: A, caseNum: Int)(f: A => Boolean): Result = {
      try {
        if (f(a)) Passed
        else Falsified(a.toString, caseNum)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), caseNum)
      }
    }

    def asProp[A](result: Boolean): Prop = Prop {
      (_, _, _) =>
        getResult(() , 0)(_ => result)
    }

    def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = {
      def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
        f(z) match {
          case Some((a, newState)) => Stream.cons(a, unfold(newState)(f))
          case None => Stream.empty
        }
      }

      unfold(rng)(rng => Some(g.sample.run(rng)))
    }

    def buildMsg[A](s: A, e: Exception): String = {
      s"test case: $s\n" +
        s"generated an exception: ${e.getMessage}\n" +
        s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
    }

    val S = Gen.unit(Executors.newCachedThreadPool)

    def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
      forAll(SGen{ _ => S ** g }) {
        case es ** a => f(a)(es).get
//        case **((es, a)) => f(a)(es).get
      }
  }

  // PROP END //

  def filterList(l: List[Int])(f: Int => Boolean): List[Int] =
    l.filter(i => i == 10 || f(i))


  def test: Unit = {
    import Ordering.Int

    val sgenList: SGen[List[Int]] = SGen.listOf(Gen.choose(0, 100))
    val genList: Gen[List[Int]] = Gen.listOf(10, Gen.choose(0, 10))

    Prop.run {
      Prop.forAll(sgenList ** Gen.sign.flatMap(f => Gen.genFn(Gen.choose(1, 100))(f)).unsized) {
        //          case (list, f) => list.takeWhile(f).forall(f)
        case (list, f) => filterList(list)(f).lengthCompare(list.count(f)) == 0
      }
    }

//    Prop.run(Prop.forAll(Gen.choose(0, 100).unsized)(_ < 95))

    //kodel neveikia ** objektas su sgenais
  }
}
