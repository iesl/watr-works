package edu.umass.cs.iesl.watr
package table  //;import acyclic.file

import geometry._

case class ImageArtifacts(
  artifactGroup: CorpusArtifactGroup
) {
  def rootPath = artifactGroup.rootPath

}
case class PageImage(
  imageArtifact: CorpusArtifact,
  pageGeometry: PageGeometry
)

trait ImageArtifactEnrichments extends ImageManipulation {

  implicit class RicherImageArtifacts(val theImageArtifacts: ImageArtifacts)  {
    // def corpusEntry
    def getPageArtifacts(): Seq[CorpusArtifact] = {
      theImageArtifacts.artifactGroup.getArtifacts
    }

    def getPageArtifact(p: Int): Option[CorpusArtifact] = {
      val pps = getPageArtifacts()
      if (p < pps.length) Option(pps(p))
      else                None
    }

    def getPages(): Seq[PageImage] = {

      ???
    }

    def getPage(pageId: Int@@PageID): Seq[PageImage] = {

      ???
    }

    def clipTo(tr: TargetRegion): Unit = {
      import com.sksamuel.scrimage._

      val TargetRegion(id, docId, pageId, bbox) = tr

      for {
        page <- getPage(pageId)
        ins <- page.asInputStream
      } {

        val corpusEntry = theImageArtifacts.artifactGroup.entry

        val image = Image.fromStream(ins, 0)
        val cropped = cropTo(image, bbox, theComponent.getPageGeometry)

      }

    }
  }

}
