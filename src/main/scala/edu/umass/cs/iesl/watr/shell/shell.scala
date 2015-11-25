package edu.umass.cs.iesl.watr
package shell

import java.io.FileInputStream
import java.io.InputStream
import java.io.{File => JFile}

import ammonite.ops._
import ammonite.ops.ImplicitWd._
import org.jdom.output.Format
import pl.edu.icm.cermine.ComponentConfiguration

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


  import pl.edu.icm.cermine.ExtractionUtils

  def cerminePDF(pdf: JFile): Unit = {
    val in = new FileInputStream(pdf);
    val conf = new ComponentConfiguration()
    val result = ExtractionUtils.extractRawTextWithLabels(conf, in);

    println("result: "+result)

    val fmt = Format.getPrettyFormat()
    // val fmt = Format.getCompactFormat()
    val xmlout = new org.jdom.output.XMLOutputter(fmt)
    val strout = xmlout.outputString(result)
    println(strout)
  }

  import pl.edu.icm.cermine.structure.model.BxDocument

  def transferCermineToSVG(bxDocument: BxDocument): Unit = {

  }

  // cermineXml = cermine(pdf)
  // svg = pdf2svg(pdf)
  // mergedSVG = merge(cermineXml, svg)




}
