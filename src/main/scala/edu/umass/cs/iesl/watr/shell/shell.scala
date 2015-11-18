package edu.umass.cs.iesl.watr
package shell

import java.io.{File => JFile}

import ammonite.ops._
import ammonite.ops.ImplicitWd._

import scala.language.implicitConversions

sealed trait Artifact

case class FileArtifact(file: JFile) extends Artifact {

  def toSVG: SVGArtifact = SVGArtifact(
    ops.pdfToSVG(file)
  )

}
case class PDFArtifact() extends Artifact
case class SVGArtifact(svg: String) extends Artifact

class Artifacts(
  val artifacts: Seq[Artifact]
) {

  def add(as: Seq[Artifact]): Artifacts = new Artifacts(
    artifacts ++ as
  )

  implicit def nioFileToArtifact(np: java.nio.file.Path) = FileArtifact(
    np.toFile()
  )

}

// val files = ls up/pdfs/*.pdf
// val artifacts = Artiacts add files
//

object Artifacts {
  def apply(): Artifacts = new Artifacts(Seq())
}


object ops {

  val pdfToSVGPath = cwd/up/"iesl-pdf-to-text"
  val pdfToSVGExe = pdfToSVGPath/"bin"/"run.sh"

  def pdfToSVG(pdf: JFile): String = {
    println("running: " + pdfToSVGExe)

    val result = %%pdfToSVGExe("-i", pdf.toPath().toString, "-o", "stdout")

    result.mkString
  }




}
