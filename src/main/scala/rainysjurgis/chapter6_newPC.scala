package rainysjurgis

object chapter6_newPC {
  trait RNG {
    def nextInt: (Int, RNG)
  }
  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  type Rand[A] = State[RNG, A]

  case class State[S,+A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] =
      State { state =>
        val (a, newState) = run(state)
        (f(a), newState)
      }

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State { state =>
        val (a, newState) = run(state)
        f(a).run(newState)
      }

    def map2[B, C](otherState: State[S, B])(f: (A, B) => C): State[S, C] = {
      State { state =>
        val (a, tempState) = this.run(state)
        val (b, finalState) = otherState.run(tempState)
        (f(a, b), finalState)
      }
    }
  }

  object SimpleRNG {
    def nonNegativeEven: State[RNG, Int] = {
      nonNegativeInt.map(num => num - num % 2)
    }

    def nonNegativeInt: State[RNG, Int] = {
      State(rng => {
        rng.nextInt match {
          case (Int.MinValue, nextRng) => (0, nextRng)
          case (num, nextRng) => (if (num < 0) -num else num, nextRng)
        }
      })
    }

    def nonNegativeLessThan(n: Int): State[RNG, Int] = {
      nonNegativeInt.flatMap(num => {
        val remainder = num % n
        if (num + (n-1) - remainder >= 0) State.setArgument(remainder) else nonNegativeLessThan(n)
      })
    }

    def numberInRange(start: Int, stopExclusive: Int): State[RNG, Int] = {
      val difference = stopExclusive - start
      nonNegativeLessThan(difference).map(_ + start)
    }

    def double: State[RNG, Double] = {
      nonNegativeInt.map {
        case Int.MinValue => 0
        case num => (num / Int.MaxValue).toDouble
      }
    }
  }

  object State {
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))
    def get[S]: State[S, S] = State(s => (s, s))
    def set[S](s: S): State[S, Unit] = State(_ => ((), s))
    def setArgument[A, S](a: A): State[S, A] = State.get.map(_ => a)

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
      fs.foldRight(unit[S, List[A]](Nil))((element, state) =>
        state.map2(element)((list, head) => head :: list)
        )
    }

    def modify[S](f: S => S): State[S, Unit] = for {
      s <- get
      _ <- set(f(s))
    } yield ()
  }

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input
  case class Machine(locked: Boolean, candies: Int, coins: Int) {
    def canUnlockMachine: Boolean = locked && candies > 0
    def canTurnKnob: Boolean = !locked && candies > 0

    def insertCoin: Machine =
      if (canUnlockMachine) copy(locked = false, coins = coins + 1)
      else this

    def turnKnob: Machine =
      if (canTurnKnob) copy(locked = true, candies = candies - 1)
      else this
  }

  object Machine {
    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
      for {
        _ <- State.sequence(inputs.map(handleInput))
        m <- State.get
      } yield (m.candies, m.coins)
    }

    def handleInput(input: Input): State[Machine, Unit] = {
      State.modify { m =>
        input match {
          case Coin => m.insertCoin
          case Turn => m.turnKnob
        }
      }
    }
  }

  def test: Unit = {
    val machine = new Machine(true, 5, 10)
    val inputs = List(Coin, Turn, Coin, Turn, Turn, Turn, Turn, Turn, Turn, Turn, Coin, Coin, Turn, Coin, Turn, Coin, Turn, Coin)
    val inputs2 = List(Coin, Turn, Coin, Turn, Turn, Coin)
    println(Machine.simulateMachine(inputs2).run(machine))
  }
}
