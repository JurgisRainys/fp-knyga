package rainysjurgis

import rainysjurgis.chapter6_newPC._
import rainysjurgis.chapter8.Prop._

object chapter8 {
  // 8.2
  // if (list.filter(x => x > maximum(list)) == 0) && (list.filter(x => x == maximum(list)) != 0)

  case class Gen[A](sample: State[RNG, A]) {
    def choose(start: Int, stopExclusive: Int): Gen[Int] = {
      SimpleRNG.
    }
  }

  trait Prop {
    def check: Either[(FailedCase, SuccessCount), Prop.SuccessCount]
    def &&(p: Prop): Prop = this.check && p.check
  }

  object Prop {
    type SuccessCount = Int
    type FailedCase = String
  }

  def listOf[A](a: Gen[A]): Gen[List[A]] = {
    ???
  }

  def forAll[A](a: Gen[A])(f: A => Boolean): Prop
}
