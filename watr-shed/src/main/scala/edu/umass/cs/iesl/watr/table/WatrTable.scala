package edu.umass.cs.iesl.watr
package table  //;import acyclic.file

import ammonite.ops._
import pprint.PPrinter

import edu.umass.cs.iesl.watr.segment.DocumentSegmenter
import spindex._
import textreflow._
import extract.images._
import corpora._
import segment._


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



  implicit class RicherDocumentSegmenter(val theSegmentation: DocumentSegmentation) extends AnyVal {

    def lines(): Seq[TextReflow] = {
      theSegmentation.mpageIndex
        .getVisualLineTextReflows()
    }

    def pageImages(): PageImages = {
      theSegmentation.pageImages
    }

  }

  implicit class RicherCorpusEntry(val theCorpusEntry: CorpusEntry) extends AnyVal {

    def segment(): Option[DocumentSegmentation] = {
      for {
        pdfArtifact    <- theCorpusEntry.getPdfArtifact
        pdfPath        <- pdfArtifact.asPath.toOption
      } yield {


        val segmenter = DocumentSegmenter
          .createSegmenter(theCorpusEntry.getURI, pdfPath, Seq())

        segmenter.runPageSegmentation()

        val pageImageArtifacts = theCorpusEntry.ensureArtifactGroup("page-images")
        val pageImages =
        if (pageImageArtifacts.getArtifacts.isEmpty) {
          ExtractImages.extract(pdfPath, pageImageArtifacts.rootPath)
        } else {
          ExtractImages.load(pageImageArtifacts.rootPath)
        }


        DocumentSegmentation(
          segmenter.mpageIndex,
          pageImages
        )
      }
    }

  }


}
