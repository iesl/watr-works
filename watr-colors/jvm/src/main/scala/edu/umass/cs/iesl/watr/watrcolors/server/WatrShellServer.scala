package edu.umass.cs.iesl.watr
package watrcolors
package server

import ammonite.ops._

import GeometricFigure._

import autowire._
import boopickle.DefaultBasic._
import Picklers._

import scala.concurrent._
import scala.concurrent.duration._
import scala.async.Async.{async, await}
import _root_.edu.umass.cs.iesl.watr.shell._


object DataExchange {
  import scala.collection.mutable

  val queue = mutable.Queue[HtmlUpdate]()


}
class WatrShellServer(
  rootDirectory: Path
) extends WatrShellApi  {
  import fs2._
  import fs2.util._
  import fs2.async._
  implicit val S = Strategy.fromFixedDaemonPool(4, "workers")
  // val T = implicitly[Async[Task]]


  val semaphore = mutable.Semaphore[Task](1)

  def enqueueUpdates[A]: Pipe[Task, Either[Unit, String], Unit] = _.evalMap{ a =>
    a.fold(
      {_ => Task.delay{ () }},
      {str => Task.delay {
        val up = HtmlAppend("#main", str)
        DataExchange.queue.enqueue(up)
      } }
    )

  }

  def startShell(): Unit = {
    val stream = WatrShell.replStream().through(enqueueUpdates)
    stream.run.unsafeRunAsyncFuture()
  }



  def update(): List[HtmlUpdate] = {
    /// non-blocking stream
    List(DataExchange.queue.dequeue())
  }

}
