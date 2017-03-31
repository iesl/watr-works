package edu.umass.cs.iesl.watr
package watrcolors
package client

import scaladget.stylesheet.{all => sheet}
import sheet._
  // import scalatags.JsDom.{ styles  }

import scaladget.api.{BootstrapTags => bs}
import scalatags.JsDom.all._
import bs._

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.scalajs.js.annotation._
import scala.concurrent.Future

import TypeTagPicklers._

import autowire._
import upickle.{default => UPickle}
import UPickle._

import rx._
import scaladget.tools.JsRxTags._


@JSExportTopLevel("PdfDocumentPage")
object PdfDocumentPage extends LabelerRendering {

}
