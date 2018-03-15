package rainysjurgis

import java.util.concurrent.{ExecutorService, Executors, ThreadFactory}

import rainysjurgis.chapter6_newPC._
import rainysjurgis.chapter7._
import rainysjurgis.chapter8.Prop.{FailedCase, MaxSize, SuccessCount, TestCases, forAll}

object chapter8 {
  // 8.2
  // if (list.filter(x => x > maximum(list)) == 0) && (list.filter(x => x == maximum(list)) != 0)

  // GEN START //
  object ** {
    def unapply[A,B](p: (A,B)) = Some(p)
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
        c <- Gen.unit(f(a, b))
      } yield c
    }

    def option: Gen[Option[A]] = {
      this.flatMap(a => Gen.unit(Some(a)))
    }

    def listOfN(n: Int): Gen[List[A]] = {
//      Gen(this.sample.flatMap(a => State.setArgument(List.fill(n)(a))))
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
      this.flatMap(a => SGen( _ => Gen.unit(f(a))))
    }

    def flatMap[B](f: A => SGen[B]): SGen[B] = {
      SGen { this.forSize(_).flatMap(a => f(a).forSize(0)) }
    }
  }

  object Gen {
    def choose(start: Int, stopExclusive: Int): Gen[Int] = {
      Gen(SimpleRNG.numberInRange(start, stopExclusive))
    }

    def nestedPar(start: Int, stopExclusive: Int): Gen[Par[Int]] = {
      val (minDepth, maxDepth) = (1, 6)
      def nest(par: Par[Int], timesToNest: Int): Par[Int] =
        if (timesToNest > 0) nest(Par.fork(par), timesToNest - 1) else par

      choose(minDepth, maxDepth).flatMap(depth => {
        choose(start, stopExclusive).map(num => nest(Par.unit(num), depth))
      })
    }

    def unit[A](a: => A): Gen[A] = Gen(State.setArgument(a))

    def boolean: Gen[Boolean] = Gen.choose(0, 2).map(_ > 0)

    def sequence[A](fs: List[Gen[A]]): Gen[List[A]] = {
      fs.foldRight(unit(List.empty[A]))((genA, acc) => {
        genA.flatMap(a => acc.flatMap(list => unit(a :: list)))
      })
    }

    def listOfInts(length: Int, start: Int, stopExclusive: Int): Gen[List[Int]] = {
      sequence(List.fill(length)(Gen.choose(start, stopExclusive)))
    }

    def listOfStrings(listLength: Int, stringMaxLength: Int): Gen[List[String]] = {
      listOfInts(listLength, 0, stringMaxLength).flatMap(stringLengths =>
        sequence(stringLengths.map(alphabeticString))
      )
    }

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

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    def isFalsified = true
  }

  case object Proved extends Result {
    def isFalsified = false
  }

  case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
    def changePassedToProved = Prop {
      this.run(_, _, _) match {
        case fail @ Falsified(_, _) => fail
        case _ => Proved
      }
    }
//      this.run(_, _, _) match {
//        case fail @ Falsified(_, _) => fail
//        case _ => Proved
//      }

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
      (max,n,rng) => run(max,n,rng) match {
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
          Stream.from(0).take(maxSize).map(i => forAllMoreInner(g(i))(f))
        //        Stream.from(0).take(testCases.min(maxSize)).map(i => forAllMoreInner(g(i))(f))

        val prop: Prop =
          props.map(p => Prop { (max, _, rng) =>
            p.run(max, casesPerSize, rng)
          }).toList.reduce(_ && _)

        prop.run(maxSize, testCases, rng)
      }
    }

    def forAllMoreInner[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
      (_, testCases,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(testCases).map {
        case (a, i) => try {
          if (f(a)) Passed
          else Falsified(a.toString, i)
        } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
      }.find(_.isFalsified).getOrElse(Passed)
  }

    val S = Gen.unit(Executors.newCachedThreadPool)
//      Gen.weighted(
//        Gen.choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
//        Gen.unit(Executors.newCachedThreadPool) -> .25
//      )

    def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
      forAll(SGen{ _ => S ** g }) { case (es ** a) => f(a)(es).get }
//      forAll(SGen{ _ => S.map2(g)((_,_)) }) { case (s,a) => f(a)(s).get }
  }

  // PROP END //

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

  def test: Unit = {
    val rng = SimpleRNG(System.currentTimeMillis)
    val smallInt = Gen.choose(-10, 10)
//
//    //test List.sorted
//    val sortedProp = forAll(SGen.listOf(smallInt)) { intList =>
//      val (_, isSorted) = intList.sorted.foldLeft((Int.MinValue, true)) {
//        case ((prevNum, wasSorted), currentNum) if wasSorted => (currentNum, currentNum >= prevNum)
//        case _ => (0, false)
//      }
//      isSorted
//    }
//    Prop.run(sortedProp, 15, 20)
//
//    //test Par.map
//    val ES: ExecutorService = Executors.newCachedThreadPool
//
//    val p1 = forAll(SGen { _ => Gen.unit(Par.unit(1)) })(i =>
//      Par.map(i)(_ + 1)(ES).get == Par.unit(2)(ES).get)
//    Prop.run(p1)
//
//    val p2 = Prop.checkPar{
//      Par.equal(
//        Par.map(Par.unit(1))(_ + 1),
//        Par.unit(2)
//      )
//    }
//    Prop.run(p2)
//
//    val p3 = Prop.check {
//      Par.equal(
//        Par.map(Par.unit(1))(_ + 1),
//        Par.unit(2)
//      )(ES).get
//    }
//    Prop.run(p3)
//
//    val pint = Gen.choose(0,10).map(Par.unit)
//    val pint = Gen.nestedPar(0, 10)
//    val p4 =
//      Prop.forAllPar(pint)(n => Par.equal(Par.map(n)(y => y), n))
//    Prop.run(p4)

//    val p = Prop.checkParBetter {
//      val x = Par.unit(7 + 11)
//      val x2 = Par.unit(8 + 11)
//      val forkX = Par.fork(x)
//      val forkX2 = Par.fork(x2)
//      Par.equal(x, forkX)
//    }
//    Prop.run(p)


    // 8.19
    def getInt[A](a: A, start: Int, stopExclusive: Int): Gen[A => Int] = {
      val hash = a.hashCode
      val difference = stopExclusive - start
      Gen.choose(0, difference).map(num => _ => (hash % num) + start)
    }

    val fx = (i: Int) => i < 3
    val list = List(1, 2).takeWhile(num => getInt(num, -20, 30))
    val test = list.lengthCompare(list.dropWhile(!fx(_)).size) == 0

  }
}
