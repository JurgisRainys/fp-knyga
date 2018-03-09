package rainysjurgis

import java.time.LocalTime

import rainysjurgis.chapter6.SimpleRNG.flatMap
import rainysjurgis.chapter6.{RNG, State, _}

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
  object State {
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))
    def get[S]: State[S, S] = State(s => (s, s))
    def set[S](s: S): State[S, Unit] = State(_ => ((), s))

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
      fs.foldRight(unit[S, List[A]](Nil))((element, state) =>
        state.map2(element)((list, head) => head :: list)
        )
    }

    def numberInRange(start: Int, stopExclusive: Int): State[RNG, Int] = {
//      State(state => {
//          val (x, nextState) = state.nextInt
//          if (x < stopExclusive && x > start) (x, nextState)
//          else numberInRange(start, stopExclusive).run(nextState)
//        }
//      )

      //DABAIGT KAD GRAZU BUTU
      get.flatMap(x => {
        val (num, nextState) = x.nextInt
        if (num < stopExclusive && (num > start)) (num, nextState)
        else numberInRange(start, stopExclusive)
      })

//      val rng = SimpleRNG(System.currentTimeMillis)
//      val initialState = unit(stopExclusive)
//
//      def loop(s: State[RNG, Int]) =
//        s.flatMap(x => if (x < stopExclusive && x > start) x else loop(State.get))
//
//     loop(initialState)
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
