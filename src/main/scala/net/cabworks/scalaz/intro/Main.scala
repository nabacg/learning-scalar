package net.cabworks.scalaz.intro

import scalaz._
import Scalaz._

object Main extends App {


  println( List(1,2) |+| List(3,4) )
  println( 23 |+| 37 )
  println( ">>>>> " |+|  " <<<<<<")

  //NonEmptyList
  val nonEmptyList = List(1,2,3).wrapNel


}
