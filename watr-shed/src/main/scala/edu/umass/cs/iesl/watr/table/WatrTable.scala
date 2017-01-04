package edu.umass.cs.iesl.watr
package table  //;import acyclic.file

import ammonite.ops._
import pprint.PPrinter

import edu.umass.cs.iesl.watr.segment.DocumentSegmenter
import spindex._
import textreflow._


object WatrTable {
  import ShellCommands._

  def main(args: Array[String]): Unit = {
    replMain().run(
      "corpus" -> initCorpus()
    )
  }

  val predef =
    s"""|import edu.umass.cs.iesl.watr
        |import watr._, spindex._, geometry._, table._
        |import ShellCommands._
        |implicit val pp0 = pprintComponent
        |implicit val pp1 = pprintBox
        |implicit val pp2 = pprintTextReflow
        |""".stripMargin

  val welcomeBanner = s""">> WatrTable Shell <<"""

  def replMain() = ammonite.Main(
    // storageBackend = new Storage.Folder(Defaults.ammoniteHome)
    predef = predef,
    // defaultPredef = true,
    wd = pwd,
    welcomeBanner = Some(welcomeBanner),
    inputStream = System.in,
    outputStream  = System.out,
    errorStream = System.err,
    verboseOutput = false
  )

}

object ShellCommands extends CorpusEnrichments {


  def pprintComponent: PPrinter[Component] = PPrinter({(component, config) =>
    val box = component.show
    Iterator(box.toString())
  })

  def pprintBox: PPrinter[TB.Box] = PPrinter({(box, config) =>
    Iterator(box.toString())
  })

  def pprintTextReflow: PPrinter[TextReflow] = PPrinter({(textReflow, config) =>
    val text = textReflow.toText()
    Iterator(text)
  })

  def initCorpus(): Corpus = {
    initCorpus(pwd)
  }


  /*

   Current problems:

   Patterns like these:
       cind @nitt.edu
       www.*.*
       Pt/PANi
       [7,17–24]
       10^{5}s^{1}
       doi: 10.1029/2005JD006318
       70^{◦}C
       Fe_{3}O_{4}@C


   - [ ] words parsed as stream of single chars (particularly on line w/one word)
        - 2 0 1 2 J D 0 1 7 45 9 . 1010022013gl058232.pdf.d <target pg:4 (l:319.97, t:560.87, w:50.71, h:7.28)
   - [ ] sup/subs are squished w/adjacent words
   - [ ] ligatures get sup/subscripted
   - [ ] join words before rendering
   - [ ] Proper names
   - [ ] technical vocab needs work
   - [ ] phrase parsing (e.g, "et al." as a single token)
   - [ ] parse footnote sup/sub markers as independent tokens
   - [ ] Reversed words across entire paper ??!!
   - [ ] Non-textline elimination
   - [ ] weight/measure/quantity parsing

   */


  implicit class RicherDocumentSegmenter(val theDocumentSegmenter: DocumentSegmenter) extends AnyVal {

    def lines(): Seq[TextReflow] = {
      theDocumentSegmenter
        .mpageIndex
        .getVisualLineTextReflows()
    }
  }

  implicit class RicherCorpusEntry(val theCorpusEntry: CorpusEntry) extends AnyVal {

    // def textBlocks(): Unit = {}
    // def paragraphs(): Unit = {}

    def pageImages(): ImageArtifacts = {
      import ammonite.{ops => fs}
      import fs._
      import fs.ImplicitWd._
      val artifacts = new ImageArtifacts(
        theCorpusEntry.ensureArtifactGroup("page-images")
      )
      val imgPath = artifacts.artifactGroup.rootPath

      for {
        pdf <- theCorpusEntry.getPdfArtifact
        pdfPath <- pdf.asPath
      } {
        val pageImageFilespec = imgPath / "page-%d.png"

        val res = %%("mudraw", "-r", "128", "-o", pageImageFilespec, pdfPath)
      }

      artifacts.artifactGroup.getArtifacts.foreach { a =>
        println(s"extracted image ${a}")
      }

      artifacts
    }

    def lines(): Seq[TextReflow]= {
      val lls = for {
        segmenter <- theCorpusEntry.segment()
      } yield {
        segmenter.lines()
      }
      lls.getOrElse(Seq())
    }


    def segment(): Option[DocumentSegmenter] = {
      for {
        pdfArtifact    <- theCorpusEntry.getPdfArtifact
        pdfPath        <- pdfArtifact.asPath.toOption
      } yield {

        val segmenter = DocumentSegmenter
          .createSegmenter(theCorpusEntry.getURI, pdfPath, Seq())

        segmenter.runPageSegmentation()
        segmenter
      }
    }

  }


}
