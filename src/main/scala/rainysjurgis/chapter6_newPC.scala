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

    def flatMap[B](f: A => State[S, B]): State[S, B] = {
      State(state => { this.run(state) match { case (a, newState) => f(a).run(newState) }})
    }

    def map2[B, C](otherState: State[S, B])(f: (A, B) => C): State[S, C] = {
      State(state => this.run(state) match {
        case (a, tempState) => otherState.run(tempState) match {
          case (b, finalState) => (f(a, b), finalState)
        }
      })
    }
  }
  object State {
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))
    def get[S]: State[S, S] = State(s => (s, s))
    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = State(initialRng => {
      fs.foldRight((List.empty[A], initialRng))((h, acc) => {
        val (list, rng) = acc
        val (a, nextState) = h.run(rng)
        (a :: list, nextState)
      })
    })
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

    }

    def simulate(input: Input): State[Machine, Unit] = {
      State(state => {
        ((), input match {
          case Coin => state.insertCoin
          case Turn => state.turnKnob
        })
      })
    }

    def simulateWithFor(input: Input): State[Machine, Unit] = {
      for {

      }
    }
  }

  def test: Unit = {
    val machine = new Machine(true, 5, 10)
    val inputs = List(Coin, Turn, Coin, Turn, Turn, Turn, Turn, Turn, Turn, Turn, Coin)
    println(Machine.simulateMachine(inputs).run(machine))
  }
}
