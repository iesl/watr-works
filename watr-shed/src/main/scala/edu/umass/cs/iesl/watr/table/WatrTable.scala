package edu.umass.cs.iesl.watr
package table  //;import acyclic.file


import ammonite.ops._
import pprint.PPrinter

import edu.umass.cs.iesl.watr.segment.DocumentSegmenter
import spindex._
// import textreflow.TextReflowRendering._

// import geometry._
import textreflow._

// import apps._
// import GeometricFigure._
// import EnrichGeometricFigures._
// import ComponentTypeEnrichments._

// import textboxing.{TextBoxing => TB}, TB._
object WatrTable {
  import ShellCommands._

  def main(args: Array[String]): Unit = {
    replMain().run(
      "corpus" -> initCorpus()
    )
  }

  val predef =
    s"""|import edu.umass.cs.iesl.watr
        |import watr._, spindex._, geometry._
        |import table._
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


object ShellCommands extends CorpusAPI {

  // implicit val ppConfig = pprint.Config(
  // width: Int = Config.defaultMaxWidth,
  // height: Int = Config.defaultLines,
  // depth: Int = 0,
  // indent: Int = Config.defaultIndent,
  // colors: Colors = pprint.Colors.BlackWhite,
  // renames: Map[String, String] = Config.defaultRenames)

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
    Corpus(pwd / "corpus-test")
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


  implicit class RicherDocumentSegmenter(val thisDocumentSegmenter: DocumentSegmenter) extends AnyVal {

    def lines(): Seq[TextReflow] = {
      val mpageIndex = thisDocumentSegmenter.mpageIndex
      mpageIndex.getTextReflows()
    }
  }

  implicit class RicherCorpusEntry(val thisCorpusEntry: CorpusEntry) extends AnyVal {

    def textBlocks(): Unit = {

    }

    def paragraphs(): Unit = {

    }

    def lines(): Seq[TextReflow]= {
      val lls = for {
        segmenter <- thisCorpusEntry.segment()
      } yield {
        segmenter.lines()
      }
      lls.getOrElse(Seq())
    }


    def segment(): Option[DocumentSegmenter] = {
      for {
        pdfArtifact    <- thisCorpusEntry.getPdfArtifact
        pdfPath        <- pdfArtifact.asPath.toOption
      } yield {

        val segmenter = DocumentSegmenter
          .createSegmenter(thisCorpusEntry.getURI, pdfPath, Seq())

        segmenter.runPageSegmentation()
        segmenter
      }
    }

  }


}


object WebShow extends ScalatagsDefs
