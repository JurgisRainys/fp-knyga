package rainysjurgis

import rainysjurgis.chapter6_newPC._
import rainysjurgis.chapter8.Prop.{ MaxSize, TestCases, FailedCase, SuccessCount }

object chapter8 {
  // 8.2
  // if (list.filter(x => x > maximum(list)) == 0) && (list.filter(x => x == maximum(list)) != 0)

  // GEN START //

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

    // kaip apsirasyt klasej, o ne objekte?
    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
      ???
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

  case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
    def &&(p: Prop): Prop = Prop {
      (maxSize, testCases, rng) =>
        this.run(maxSize, testCases, rng) match {
          case Passed => p.run(maxSize, testCases, rng)
          case falsified @ _ => falsified
        }
    }

    def ||(p: Prop): Prop = Prop {
      (maxSize, testCases, rng) => this.run(maxSize, testCases, rng) match {
        case pass @ Passed => pass
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
      }
  }

  // PROP END //

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAllInner(g.forSize)(f)

  def forAllInner[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (maxSize, testCases, rng) => {
      val casesPerSize = (testCases + (maxSize - 1)) / maxSize

      val props: Stream[Prop] =
        Stream.from(0).take(testCases.min(maxSize) ).map(i => forAllMoreInner(g(i))(f))

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
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
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

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"


  def test: Unit = {
    val rng = SimpleRNG(System.currentTimeMillis)

    val smallInt = Gen.choose(-10, 10)
    val sortedProp = forAll(SGen.listOf1(smallInt)) { intList =>
      val (_, isSorted) = intList.sorted.foldLeft((Int.MinValue, true)) {
        case ((prevNum, wasSorted), currentNum) if wasSorted => (currentNum, currentNum >= prevNum)
        case _ => (0, false)
      }
      isSorted
    }
    Prop.run(sortedProp)
  }
}
