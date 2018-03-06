package rainysjurgis

import rainysjurgis.chapter6_newPC.State

object chapter6 {
  trait RNG {
    def nextInt: (Int, RNG)
  }
  type State[S, +A] = S => (A, S)
  type Rand[A] = State[RNG, A]

//  case class State[S, +A](run: S => (A, S))
  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  object SimpleRNG {
    def randomPair(rng: RNG): ((Int, Int), RNG) = {
      val (i1, rng2) = rng.nextInt
      val (i2, rng3) = rng2.nextInt
      ((i1, i2), rng3)
    }

    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      rng.nextInt match {
        case (Int.MinValue, nextRng) => (0, nextRng)
        case (num, nextRng) => (if (num < 0) -num else num, nextRng)
      }
    }

    def double(rng: RNG): (Double, RNG) = {
      nonNegativeInt(rng) match {
        case (Int.MinValue, nextRng) => (0, nextRng)
        case (num, nextRng) => ((num / Int.MaxValue).toDouble, nextRng)
      }
    }

    def double2[S]: Rand[Double] =
      map(nonNegativeInt)(num => if (num == Int.MinValue) 0 else (num / Int.MaxValue).toDouble)

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      rng.nextInt match {
        case (intNum, nextRng) => double(rng) match {
          case (doubleNum, _) => ((intNum, doubleNum), nextRng)
        }
      }
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      double(rng) match {
        case (doubleNum, nextRng) => rng.nextInt match {
          case (intNum, _) => ((doubleNum, intNum), nextRng)
        }
      }
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      double(rng) match {
        case (d1, rng2) => double(rng2) match {
          case (d2, rng3) => double(rng3) match {
            case (d3, rng4) => ((d1, d2, d3), rng4)
          }
        }
      }
    }

    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
      def loop(_count: Int, _rng: RNG, acc: List[Int]): (List[Int], RNG) = {
        (_rng.nextInt, _count) match {
          case ((num, nextRng), _count) => if (_count == 0) (acc, nextRng) else loop(_count - 1, nextRng, num :: acc)
        }
      }

      loop(count, rng, List.empty)
    }

    def unit[A](a: A): Rand[A] =
      rng => (a, rng)

    def unitGeneralized[S, A](a: A): State[S, A] =
      s => (a, s)

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
      val (a, nextRng) = s(rng)
      (f(a), nextRng)
    }

    def mapUsingFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = {
      flatMap(s)(a => nextRng => (f(a), nextRng))
    }

    def mapGeneralized[S, A, B](func: S => (A, S))(f: A => B): State[S, B] = s => {
      func(s) match {
        case (a, state) => (f(a), state)
      }
    }

    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = initialRng => {
      ra(initialRng) match {
        case (a, tempRng) => rb(tempRng) match {
          case (b, finalRng) => (f(a, b), finalRng)
        }
      }
    }

    def map2Generalized[A, B, C, S](ra: State[S, A], rb: State[S, B])(f: (A, B) => C): State[S, C] = initialState => {
      ra(initialState) match {
        case (a, tempState) => rb(tempState) match {
          case (b, finalState) => (f(a, b), finalState)
        }
      }
    }

    def map2UsingFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
      rng =>
        ra(rng) match {
          case (a, tempRng) => rb(tempRng) match {
            case (b, finalRng) => (f(a, b), finalRng)
          }
        }
    }

    def flatMap[A, B](s: Rand[A])(f: A => Rand[B]): Rand[B] =
      rng => {
        val (a, nextRng) = s(rng)
        f(a)(nextRng)
      }

    def flatMapGeneralized[A, B, S](func: State[S, A])(f: A => State[S, B]): State[S, B] =
      rng => {
        val (a, nextRng) = func(rng)
        f(a)(nextRng)
      }


    def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
      map2(ra, rb)((_, _))

    val randIntDouble: Rand[(Int, Double)] =
      both(rng => rng.nextInt, double)

    val randDoubleInt: Rand[(Double, Int)] =
      both(double, rng => rng.nextInt)

    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
      fs.foldRight(unit(List.empty[A])) { (rng, current) =>
        flatMap(current) { list =>
          flatMap(rng) { a => unit(a :: list) }
        }
      }
    }

    def ints2(count: Int): Rand[List[Int]] = rng => {
      sequence(List.fill(count)(rng => rng.nextInt match {
        case (num, nextRng) => (num, nextRng)
      }): List[Rand[Int]])(rng)
    }

    //keistai generuoja, daznai sugeneruoja 1, 7, 10
    def nonNegativeLessThan(n: Int): Rand[Int] = {
      flatMap(nonNegativeInt)(num => {
        val remainder = num % n
        if (num + (n-1) - remainder >= 0) nextRng => (remainder, nextRng) else nonNegativeLessThan(n)
//        nextRng => if (((Int.MaxValue - remainder) % n) == 0) (remainder, nextRng) else nonNegativeLessThan(n)(nextRng)
      })
    }

    val nonNegativeEven: Rand[Int] = {
      map(nonNegativeInt)(i => i - i % 2)
    }

    val nonNegativeEven6: Rand[Int] = {
      mapUsingFlatMap(nonNegativeInt)(i => i - i % 2)
    }

    def nonNegativeEven2: Rand[Int] = {
      map(nonNegativeInt)(i => i - i % 2)(_)
    }

    val nonNegativeEven3: Rand[Int] = {
      val x = map(nonNegativeInt)(i => i - i % 2)
      rng => x(rng)
    }

    val nonNegativeEven4: Rand[Int] = { rng => {
      println("call nonNegativeEven4")
      val x = map(nonNegativeInt)(i => i - i % 2)
      x(rng)
    }}
  }

  def test: Unit = {
    val rng = SimpleRNG(13)

//    println(SimpleRNG.ints(5)(rng))
//    println(SimpleRNG.ints2(5)(rng))
//    println(SimpleRNG.double2(rng))
    //    println(SimpleRNG.randomPair(rng)._1._2)
    println(SimpleRNG.randIntDouble(rng))
  }
}
