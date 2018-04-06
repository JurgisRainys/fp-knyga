package rainysjurgis.hkt

object Main {
  trait HKT[W, A] {}

  case object Id {
    def narrow[A](hkt: HKT[Id.type, A]): Id[A] = hkt.asInstanceOf[Id[A]]
  }
  case class Id[A](a: A) extends HKT[Id.type, A]

  case object ListWrapper {
    def narrow[A](hkt: HKT[ListWrapper.type, A]): ListWrapper[A] = hkt.asInstanceOf[ListWrapper[A]]
  }
  case class ListWrapper[A](list: List[A]) extends HKT[ListWrapper.type, A]

  trait Functor[W] {
    def map[A, B](data: HKT[W, A])(f: A => B): HKT[W, B]
  }

  case object IdFunctor extends Functor[Id.type] {
    override def map[A, B](data: HKT[Id.type, A])(f: A => B): HKT[Id.type, B] = {
      Id(f(Id.narrow(data).a))
    }
  }

  case object ListFunctor extends Functor[ListWrapper.type] {
    override def map[A, B](data: HKT[ListWrapper.type, A])(f: A => B): HKT[ListWrapper.type, B] = {
      ListWrapper(ListWrapper.narrow(data).list.map(f))
    }
  }

  def mapIntToPlus10[W](hkt: HKT[W, Int])(fun: Functor[W]): HKT[W, Int] = {
    fun.map(hkt)(_ + 10)
  }

  def run: Unit = {
    val listWrapper = ListWrapper(List(1, 2))
    val id = Id(3)

    val test = ListWrapper.narrow(mapIntToPlus10(listWrapper)(ListFunctor)).list
    val test2 = Id.narrow(mapIntToPlus10(id)(IdFunctor)).a

    println(test)
    println(test2)
  }
}
