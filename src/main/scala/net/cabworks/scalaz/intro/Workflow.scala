package net.cabworks.scalaz.intro

import scala.concurrent.{Await, Future}
import scala.util.{Random, Try}
import scalaz._
import Scalaz._

object system {
  def uuid: String = java.util.UUID.randomUUID.toString
}

class Workflow {


}


trait Error
case class UserInputError(msg: String) extends Error
case class EnvironmentError(msg: String) extends Error
case class ProgrammingError(msg: String) extends Error

case class Result(payload: String, jobId: String = system.uuid)

object Workflow extends App {

  val in = (1 |-> 10).map(a => Random.nextString(10)).toStream

  def downloadFile(path: String): \/[Error, Result] = path match {
    case p if p.startsWith("a") => UserInputError("invalid path").left
    case p if p.hashCode % 3 == 0 => EnvironmentError("Cannot reach disk").left
    case p => Result( s"targetDir/$p").right
  }

  def copyFile(r: Result): \/[Error, Result] = r.payload match {
    case p if Random.nextBoolean() => EnvironmentError("Can't copy").left
    case p if Random.nextBoolean() => Result("Outstanding Result").right
    case _ => Result("OK").right
  }

  def calculate(r: Result): \/[Error, Int] =
    Try(12 / (r.payload.length - 2)).map(_.right)
      .getOrElse(ProgrammingError("Divide by zero").left)

  val workflow =
    in.map(i => downloadFile(i) flatMap copyFile flatMap calculate flatMap {i => if(i % 3 ==0) "OK".right else UserInputError("Invalid checksum").left })

  workflow.foreach(println)

}

object AsyncWorkflow extends App {

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  val in = (1 |-> 10).map(a => Random.nextString(10)).toStream

  def downloadFile(path: String): \/[Error,Result] = {
    println("Download file")
    Thread.sleep(700)

    path match {
      case p if p.startsWith("a") => UserInputError("invalid path").left
      case p if p.hashCode % 3 == 0 => EnvironmentError("Cannot reach disk").left
      case p => Result(s"targetDir/$p").right
    }
  }


  def copyFile(r: Result): \/[Error, Result] = {
    println("Copy file")
    Thread.sleep(300)
    r.payload match {
      case p if Random.nextBoolean() => EnvironmentError("Can't copy").left
      case p if Random.nextBoolean() => Result("Outstanding Result").right
      case _ => Result("OK").right
    }
  }

  def calculate(r: Result): \/[Error, Int] =
    Try({
      println("Calculating..")
      Thread.sleep(50)
      12 / (r.payload.length - 2)}).map(_.right)
      .getOrElse(ProgrammingError("Divide by zero").left)


  val workflow =
    in.map(i => Future(downloadFile(i))
                .flatMap(fr => Future(fr.flatMap(r => copyFile(r))))
                .flatMap(fr => Future(fr.flatMap(r => calculate(r))))
                .map(fr => fr.flatMap(i => if(i % 3 ==0) "OK".right else UserInputError("Invalid checksum").left ))
    )

  workflow.map(p => {
    p.onComplete(println)
    p.map({
          case -\/(UserInputError(msg)) => "Workflow failed on user Input " + msg
          case -\/(EnvironmentError(msg)) => "Workflow failed due to Environment Error " + msg
          case -\/(ProgrammingError(msg)) => "Workflow failed due to bug " + msg
          case \/-(p) => "SUCCESS " + p
      })})
        .foreach(p => Await.result(p, 5.second))




}