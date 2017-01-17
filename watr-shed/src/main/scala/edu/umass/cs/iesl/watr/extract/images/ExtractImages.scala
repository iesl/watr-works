package edu.umass.cs.iesl.watr
package extract
package images 


import com.sksamuel.scrimage._

case class PageImages(
  images: Seq[Image]
) {
  lazy val dims = images.map({image =>
    image.dimensions
  })

  override def toString():String = {
    dims
      .map({case (w, h) => s"img:${w}x${h}"})
      .mkString("[", ", ", "]")
  }

  def page(p: Int@@PageID): Image = {
    if (p.unwrap < images.length) {
      images(p.unwrap)
    } else {
      sys.error(s"PageImage ${p} not found in pages ${toString()}")
    }
  }

  def pageBytes(p: Int@@PageID): Array[Byte]= {
    val image = page(p)
    image.bytes
  }

}

object ExtractImages extends ImageManipulation {
  import ammonite.{ops => fs}, fs._

  // def sourceImage(): URI = {
  //   import com.sksamuel.scrimage._

  //   val pageId = theComponent.pageId.unwrap+1
  //   val srcUri = theComponent.getSrcUri()

  //   val pageSrc = srcUri.resolve("page-images/").resolve(s"page-${pageId}.png")
  //   println(s"page src: ${pageSrc}")
  //   val image = Image.fromFile(new java.io.File(pageSrc.getPath))
  //   val cropped = ImageManipulation.cropTo(image, theComponent.bounds, theComponent.getPageGeometry)

  //   val x = theComponent.bounds.left.toInt
  //   val y = theComponent.bounds.top.toInt
  //   val w = cropped.width
  //   val h = cropped.height

  //   val imgDst = srcUri.resolve("page-images/").resolve(s"page-${pageId}-x$x-y$y-w$w-h$h.png")
  //   println(s"page dest: ${imgDst}")
  //   cropped.output(new java.io.File(imgDst.getPath))

  //   imgDst
  // }

  def load(rootPath: Path): PageImages = {
    val images = ls(rootPath)
      .filter(_.ext=="png")
      .sortBy(_.name.drop(5).dropRight(4).toInt)
      .map(p => Image.fromFile(p.toNIO.toFile))

    PageImages(images)
  }

  def extract(pdfPath: Path, outputPath: Path): PageImages = {
    import fs.ImplicitWd._
    val out = outputPath / "page-%d.png"

    val res = %%("mudraw", "-r", "128", "-o", out, pdfPath)

    load(outputPath)
  }


  // implicit class RicherImageArtifacts(val theImageArtifacts: ImageArtifacts)  {

  //   // def corpusEntry
  //   def getPageArtifacts(): Seq[CorpusArtifact] = {
  //     theImageArtifacts.artifactGroup.getArtifacts
  //   }

  //   def getPageArtifact(p: Int): Option[CorpusArtifact] = {
  //     val pps = getPageArtifacts()
  //     if (p < pps.length) Option(pps(p))
  //     else                None
  //   }

  //   def getPages(): Seq[PageImage] = {

  //     ???
  //   }

  //   def getPage(pageId: Int@@PageID): Option[PageImage] = {

  //     ???
  //   }

  //   def clipTo(tr: TargetRegion): Either[String, PageImage] = {

  //     val TargetRegion(id, docId, pageId, bbox) = tr

  //     val clipped = for {
  //       page <- getPage(pageId)
  //       ins <- page.imageArtifact.asInputStream.toOption
  //     } yield {
  //       val image = Image.fromStream(ins, 0)

  //       page.copy(
  //         clipped= (bbox, cropTo(image, bbox, page.pageGeometry)).some
  //       )
  //     }
  //     clipped.toRight(s"error clipping ${tr}")
  //   }
  // }

}
// case class ImageArtifacts(
//   artifactGroup: CorpusArtifactGroup
// ) {
//   def rootPath = artifactGroup.rootPath

//   def pageImages(): ImageArtifacts = {
//     import ammonite.{ops => fs}
//     import fs._
//     import fs.ImplicitWd._
//     val artifacts = new ImageArtifacts(
//       theCorpusEntry.ensureArtifactGroup("page-images")
//     )
//     val imgPath = artifacts.artifactGroup.rootPath

//     for {
//       pdf <- theCorpusEntry.getPdfArtifact
//       pdfPath <- pdf.asPath
//     } {
//       val pageImageFilespec = imgPath / "page-%d.png"

//       val res = %%("mudraw", "-r", "128", "-o", pageImageFilespec, pdfPath)
//     }

//     artifacts.artifactGroup.getArtifacts.foreach { a =>
//       println(s"extracted image ${a}")
//     }

//     artifacts
//   }

// }
