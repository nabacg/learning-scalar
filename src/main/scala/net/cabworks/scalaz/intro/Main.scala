package net.cabworks.scalaz.intro

import scalaz._
import Scalaz._

object Main extends App {


  println( List(1,2) |+| List(3,4) )
  println( 23 |+| 37 )
  println( ">>>>> " |+|  " <<<<<<")

  //NonEmptyList
  val nonEmptyList = List(1,2,3).wrapNel

  val o: Option[Int] = Some(12)

  println(o some {_ + 23} none 0)

  println("Some: " + ~o + " or if none: " + ~(None: Option[Int])
    + " prints mzero from monoid (0 for Int)")

  def isEven(x: Int): Validation[NonEmptyList[String], Int] =
    if (x % 2 == 0) x.success
    else s"$x not even!".wrapNel.failure


  println("validation 2 isEven " + isEven(2) + " 3 isEven? " + isEven(3))

  (1 > 10)? "Nonsense" | "What? "

  //Ord
  3 ?|? 4


  //Enum

  (1 |-> 10).toList == (1 to 10).toList
  'a' |-> 'z'

  'F'.succ


  'a' |--> (3, 'z') // every 3 letter

  //to stream
  'a' |=> 'z'

  //functors

  (1, 2, 3 ) map {_ + 3}
  //function is a functor
  val f = ((x: Int) => x + 2) map {_ * 5}
  f(10)
}
