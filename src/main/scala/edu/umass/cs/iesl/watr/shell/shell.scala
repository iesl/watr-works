package edu.umass.cs.iesl.watr
package shell

import java.io.FileInputStream
import java.io.InputStream
import java.io.{File => JFile}
import pl.edu.icm.cermine.structure.model.BxChunk
import pl.edu.icm.cermine.structure.model.BxWord

// import ammonite.ops._
// import ammonite.ops.ImplicitWd._
import org.jdom.output.Format
import org.jdom2.Document
import org.jdom2.input.SAXBuilder
import pl.edu.icm.cermine.ComponentConfiguration
import pl.edu.icm.cermine.content.model.ContentStructure
import pl.edu.icm.cermine.structure.model.BxDocument

import scala.language.implicitConversions

import scala.collection.JavaConversions._

sealed trait Artifact

case class FileArtifact(file: JFile) extends Artifact {

  // def toSVG: SVGArtifact = SVGArtifact(
  //   ops.pdfToSVG(file)
  // )

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

import scala.sys.process._


object ops {
  import pl.edu.icm.cermine.ExtractionUtils

  // val pdfToSVGPath = cwd/up/"iesl-pdf-to-text"
  // val pdfToSVGExe = pdfToSVGPath/"bin"/"run.sh"
  val pdf2svg = "../iesl-pdf-to-text/bin/run.sh"

  def pdfToSVG(pdf: JFile): Document = {
    println("running: " + pdf2svg)

    // "ls" #| "grep .scala" #&& Seq("sh", "-c", "scalac *.scala") #|| "echo nothing found" lines
    val inputPath = pdf.toPath().toString()
    val outputPath = inputPath+".svg"
    println(s"running: ${pdf2svg} on ${inputPath} -> ${outputPath}")

    // val result = List(pdf2svg, "-i", pdf.toPath().toString(), "-o", outputPath).!

    val jdomBuilder = new SAXBuilder();
    val svgJDom = jdomBuilder.build(new FileInputStream(outputPath))
    svgJDom
  }



  def cerminePDF(pdf: JFile): (BxDocument, ContentStructure) = {
    val in = new FileInputStream(pdf);
    val conf = new ComponentConfiguration()
    val doc = ExtractionUtils.extractStructure(conf, in);
    val contentStructure = ExtractionUtils.extractText(conf, doc);



    // println(s"result: doc=${doc.toText()}")

    // val fmt = Format.getPrettyFormat()
    // val fmt = Format.getCompactFormat()
    // val xmlout = new org.jdom.output.XMLOutputter(fmt)
    // val strout = xmlout.outputString(result)
    // println(strout)

    (doc, contentStructure)
  }

  import pl.edu.icm.cermine.structure.model.BxDocument



  def compareCermineAndSVG(
    bxDocument: BxDocument, bxStructure: pl.edu.icm.cermine.content.model.ContentStructure,
    svg: org.jdom2.Document
  ): Unit = {
    // println("words: " + bxDocument.asWords().mkString(", "))
    val tspans = edu.umass.cs.iesl.watr.watrmarkup.DOMUtils.getTSpanElements(svg)

    import scala.collection.mutable

    val bxChunks = mutable.ArrayBuffer[BxChunk]()

    bxDocument.asPages.foreach{ p =>
      println("page: " + p.toText().take(40).mkString)
      p.iterator().foreach { zone =>
        zone.iterator().foreach { line =>
          line.iterator().foreach {word =>
            println(s"word: ${word.toText()}, ${word.getBounds().getX}, ${word.getBounds().getY} ")
            word.iterator().foreach {chunk =>
              // println(s"    ${chunk.toText()}, ${chunk.getBounds().getX}, ${chunk.getBounds().getY} ")
              bxChunks.append(chunk)
            }
          }
        }
      }
    }



  }



  def alignCermineAndSVG(pdf: JFile): Unit = {
    println("pdf -> svg")
    val svg = pdfToSVG(pdf)
    println("pdf -> cermine")
    val (bxDoc, bxStruct) = cerminePDF(pdf)

    println("comparing cermine/svg")

    compareCermineAndSVG(bxDoc, bxStruct, svg)

  }
  // cermineXml = cermine(pdf)
  // svg = pdf2svg(pdf)
  // mergedSVG = merge(cermineXml, svg)



  def test1() {
    val pdf = new JFile("""corpus~/samples-mit/6376.pdf""")
    alignCermineAndSVG(pdf)

  }



}
