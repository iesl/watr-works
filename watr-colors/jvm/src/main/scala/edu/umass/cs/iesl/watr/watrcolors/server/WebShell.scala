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


  // def run(): Unit = {
  //   val hello = "Hello"
  //   val predef =
  //     s"""|import edu.umass.cs.iesl.watr
  //         |import watr._, spindex._, ComponentRendering._
  //         |import shell._
  //         |import ShellCommands._
  //         |implicit val pp0 = pprintComponent
  //         |implicit val pp1 = pprintBox
  //         | println("..")
  //         |""".stripMargin

  //   val welcomeBanner =
  //     s"""|>> WatrColors Shell <<
  //         |""".stripMargin


  //   ammonite.Main(
  //     predef = predef,
  //     //   defaultPredef: Boolean = true,
  //     //   storageBackend: Storage = new Storage.Folder(Defaults.ammoniteHome),
  //     wd = pwd,
  //     welcomeBanner = Some(welcomeBanner),
  //     inputStream = System.in,
  //     outputStream  = System.out,
  //     errorStream = System.err,
  //     verboseOutput = true
  //   ).run(
  //     "corpus" -> initCorpus()
  //   )
  // }

}




// object WebShow extends ScalatagsDefs
