package edu.umass.cs.iesl.watr
package corpora

import database._
import filesys._
import workflow._
import corpora.{RelationModel => Rel}
import spindex.MultiPageIndex
import geometry.syntax._
import scalaz.syntax.equal._
import java.nio.{file => nio}
import scalaz.{
  \/, -\/, \/-
}

object RegionImageResponse {
  type RegionImageResponse  = String \/ (Array[Byte] \/ nio.Path)

  def anError(r:String): RegionImageResponse = -\/(r)
  def aByteArray(r:Array[Byte]): RegionImageResponse = \/-(-\/(r))
  def aPath(r:nio.Path): RegionImageResponse = \/-(\/-(r))

  def fold[T](v: RegionImageResponse)(
    onError: String => T,
    onBytes: Array[Byte] => T,
    onPath: nio.Path => T
  ) = v.fold(onError, _.fold(onBytes, onPath))

}
trait CorpusAccessApi {
  def corpusAccessDB: CorpusAccessDB
  def corpus: Corpus

  def docStore: DocumentZoningApi = corpusAccessDB.docStore
  def workflowApi: WorkflowApi = corpusAccessDB.workflowApi
  def userbaseApi: UserbaseApi = corpusAccessDB.userbaseApi

  def getPageAndDocument(pageId: Int@@PageID): (Rel.Page, Rel.Document) = {
    corpusAccessDB.getPageAndDocument(pageId)
  }

  def getPageIndexes(stableId: String@@DocumentID): Option[MultiPageIndex] = {
    for {
      entry     <- corpus.entry(stableId.unwrap)
      group     <- entry.getArtifactGroup("rtrees")
      rtreeBlob <- group.getArtifact(s"page-${pageNum}.rtree")
      rtreePath <- rtreeBlob.asPath
    } {
    }
    //   rindex.RTreeIndex.load(rtreePath.toNIO)

    ???
  }


  import RegionImageResponse._

  def serveTargetRegionImageUpdate(regionId: Int@@RegionID): RegionImageResponse = {
    val corpusRoot = corpus.corpusRoot.toNIO
    val targetRegion = docStore.getTargetRegion(regionId)
    val pageId = targetRegion.page.pageId
    val (rPage, rDoc) = getPageAndDocument(pageId)
    val tbbox = targetRegion.bbox
    val pbbox = rPage.bounds

    val entryPath = rDoc.stableId.unwrap
    val imagePath = corpusRoot
      .resolve(entryPath)
      .resolve("page-images")
      .resolve(s"page-${rPage.pagenum.unwrap+1}.opt.png")

    if (tbbox === pbbox) {
      // Client is requesting the entire page
      aPath(imagePath)
    } else {
      // Client is requesting a clipped page region
      import com.sksamuel.scrimage._
      val image = Image.fromPath(imagePath)
      val cropped = images.ImageManipulation.cropTo(image, tbbox, pbbox)
      def bytes: Array[Byte] = cropped.bytes
      aByteArray(bytes)
    }
  }


  // def serveTargetRegionImage(regionId: Int@@RegionID): Array[Byte] = {
  //   val corpusRoot = corpus.corpusRoot.toNIO
  //   val targetRegion = docStore.getTargetRegion(regionId)
  //   val pageId = targetRegion.page.pageId
  //   val (rPage, rDoc) = getPageAndDocument(pageId)
  //   val tbbox = targetRegion.bbox
  //   val pbbox = rPage.bounds
  //   if (tbbox === pbbox) {
  //     // Client is requesting the entire page
  //     val entryPath = rDoc.stableId.unwrap
  //     val imagePath = corpusRoot
  //       .resolve(entryPath)
  //       .resolve("page-images")
  //       .resolve(s"page-${rPage.pagenum.unwrap+1}.opt.png")
  //     imagePath
  //   } else {
  //     // Client is requesting a clipped page region
  //   }
  //   ???
  // }

}

object CorpusAccessApi {

  def apply(db: CorpusAccessDB, corpus0: Corpus): CorpusAccessApi =
    new CorpusAccessApi {
      def corpusAccessDB: CorpusAccessDB = db
      def corpus: Corpus = corpus0
    }

}
