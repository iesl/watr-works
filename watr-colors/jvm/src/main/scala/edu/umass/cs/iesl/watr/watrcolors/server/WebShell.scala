package edu.umass.cs.iesl.watr
package watrcolors
package server

import ammonite.ops._
import pprint.PPrinter
import scala.util.matching.Regex
import spindex._
import EnrichGeometricFigures._
import ComponentTypeEnrichments._
import TypeTags._
import _root_.edu.umass.cs.iesl.watr.shell._

import textboxing.{TextBoxing => TB}, TB._

object WebShell {

  def main(args: Array[String]): Unit = {
    val server =  new Server("localhost", 9999)

    WatrShell.replMain().run(
      "server" -> server
    )

    server.kill()
  }
}




