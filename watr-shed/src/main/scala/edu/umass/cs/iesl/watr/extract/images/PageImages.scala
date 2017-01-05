package edu.umass.cs.iesl.watr
package extract
package images //;import acyclic.file

import geometry._
import GeometricFigure._
import com.sksamuel.scrimage._

case class ImageArtifacts(
  artifactGroup: CorpusArtifactGroup
) {
  def rootPath = artifactGroup.rootPath

}
case class PageImage(
  imageArtifact: CorpusArtifact,
  pageGeometry: PageGeometry,
  clipped: Option[(LTBounds, Image)]
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

    def getPage(pageId: Int@@PageID): Option[PageImage] = {

      ???
    }

    def clipTo(tr: TargetRegion): Either[String, PageImage] = {

      val TargetRegion(id, docId, pageId, bbox) = tr

      val clipped = for {
        page <- getPage(pageId)
        ins <- page.imageArtifact.asInputStream.toOption
      } yield {
        val image = Image.fromStream(ins, 0)

        page.copy(
          clipped= (bbox, cropTo(image, bbox, page.pageGeometry)).some
        )
      }
      clipped.toRight(s"error clipping ${tr}")
    }
  }

}
