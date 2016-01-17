package net.cabworks.scalaz.decontructing.scalaz.decontructing

trait Monoid[T] {
  def mappend(a1: T, a2: T): T
  def mzero: T
}

object Monoid {
  implicit val  IntMonoid: Monoid[Int] = new Monoid[Int] {
    def mappend(a1: Int, a2: Int): Int = a1 + a2
    def mzero: Int = 0
  }

  implicit object StringMonoid extends Monoid[String] {
    override def mappend(a1: String, a2: String): String = a1 + a2
    override def mzero: String = ""
  }


  implicit class MonoidOpts[A](v: A) {
    val value: A = v

    def |+|(a: A)(implicit m: Monoid[A]): A = m.mappend(value, a)
  }

  implicit class MAOpts[M[_], A] (v: M[A]) {
    val value: M[A] = v
    def sumMy(implicit m: Monoid[A], r: Reduce[M]): A =
      r.reduce(value, m.mzero, m.mappend)
  }

}



trait Reduce[F[_]] {
  def reduce[A, B](xs: F[A], b: B, f: (B, A) => B): B
}

object Reduce {

  implicit object ReduceList extends Reduce[List] {
    override def reduce[A, B](xs: List[A], b: B, f: (B, A) => B): B = xs.foldLeft(b)(f)
  }
}


object Main extends App {

  def implicitly[T](implicit t: T): T = t

  val multMonoid = new Monoid[Int] {
    override def mappend(a1: Int, a2: Int): Int = a1 * a2
    override def mzero: Int = 1
  }

  def sum[M[_], T](xs: M[T])(implicit m: Monoid[T], r: Reduce[M]) =
    r.reduce(xs, m.mzero, m.mappend)

  def plus[T](a: T, b: T)(implicit m: Monoid[T]): T = m.mappend(a, b)


  def showResult(d: Any, r: Any) = println(s">> Sum of ${d}}  is " + r)

  val d = (1 to 10).toList
  showResult(d, sum(d))



  val s = List("a", "b", "c")
  showResult(s, sum(s))

  showResult(d, sum(d)(multMonoid, implicitly[Reduce[List]]))

  {
    //MonoidOpts
    import Monoid._
    2 |+| 3
  }

  {
    //MA opts
    import Monoid._
    println(">> " + List(1,2,3,5).sumMy)
  }

//  {
//    implicit val m = multMonoid
//    showResult(d, sum(d)(multMonoid, implicitly[Reduce[List]]))
//  }


  println("Done")
}
