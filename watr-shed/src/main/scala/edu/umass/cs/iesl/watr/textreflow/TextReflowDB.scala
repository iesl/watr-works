package edu.umass.cs.iesl.watr
package textreflow //;import acyclic.file

import doobie.imports._
import scalaz.syntax.applicative._

import geometry._
import spindex._
import segment._

import com.sksamuel.scrimage._
import shapeless._
import databasics._

import scalaz.std.list._
import scalaz.syntax.traverse._


// import java.io.PrintWriter
// import doobie.free.{ drivermanager => DM }

class TextReflowDB(
  val tables: TextReflowDBTables
) extends DoobiePredef {

  lazy val xa = tables.xa

  def addSegmentation(ds: DocumentSegmentation): Unit = {

    // val pw = new PrintWriter(System.out)
    // _         <- HC.delay{ DM.setLogWriter(pw)}

    val mpageIndex = ds.mpageIndex
    val docId = mpageIndex.getDocumentID()

    val query = for {
      _         <- putStrLn("Starting ...")
      docPrKey  <- getOrInsertDocumentID(docId)
      _         <- putStrLn("insert multi pages...")
      _         <- insertMultiPageIndex(docId, mpageIndex)
    } yield docPrKey

    val docPrKey = query.transact(xa).unsafePerformSync

    for {
      pageId <- mpageIndex.getPages
    } {
      val q2 = for {
        _         <- putStrLn(s"insert page ${pageId}")
        pagePrKey <- insertPageIndex(docPrKey, mpageIndex.getPageIndex(pageId), ds.pageImages.page(pageId))
      } yield ()

      q2.transact(xa).unsafePerformSync
    }

  }

  def insertMultiPageIndex(docId: String@@DocumentID, mpageIndex: MultiPageIndex): ConnectionIO[List[Unit]] = {
    // To start, just create entries for all Component/Zones labelled VisualLine

    val query: ConnectionIO[List[Unit]] = (for {
      vline <-  mpageIndex.getDocumentVisualLines.flatten.toList
      maybeReflow = mpageIndex.getTextReflowForComponent(vline.id)
      vlineZoneId = mpageIndex.getZoneForComponent(vline.id).get
    } yield for {
      _ <- putStrLn(s"Inserting zone for line ${vline.id}")
      _ <- insertZone(docId, vlineZoneId)
    } yield ()).sequenceU

    query
  }

  def insertZone(docId: String@@DocumentID, zoneId: Int@@ZoneID): ConnectionIO[Int] = {

    sql"""
       insert into zone (zoneid, document)
       values (${zoneId}, (select id from document where stable_id='${docId}'))
    """.update.withUniqueGeneratedKeys[Int]("id")
  }

  def insertPageIndex(docPrKey: Int, pageIndex: PageIndex, pageImage: Image): ConnectionIO[Int] = {
    println(s"adding page w/image dims ${pageImage.dimensions} ")

    val PageGeometry(pageId, LTBounds(l, t, w, h)) = pageIndex.pageGeometry
    val (bl, bt, bw, bh) = (dtoi(l), dtoi(t), dtoi(w), dtoi(h))

    sql"""
      insert into page (document, pagenum, pageimg, bleft, btop, bwidth, bheight)
      values (${docPrKey}, ${pageId}, ${pageImage.bytes}, $bl, $bt, $bw, $bh)
    """.update.withUniqueGeneratedKeys("id")

  }


  def insertDocumentID(docId: String@@DocumentID): ConnectionIO[Int] = {
    sql"""
       insert into document (stable_id)
       values (${docId})
    """.update.withUniqueGeneratedKeys[Int]("id")
  }

  def getOrInsertDocumentID(docId: String@@DocumentID): ConnectionIO[Int] = {
    sql"select id from document where stable_id=${docId}"
      .query[Int].option
      .flatMap({
        case Some(pk) => FC.delay(pk)
        case None => insertDocumentID(docId)
      })
  }


  def selectPageGeometry(docId: String@@DocumentID, pageId: Int@@PageID): ConnectionIO[PageGeometry] = {
    val query = sql"""
       select bleft, btop, bwidth, bheight from page p join document d
       using p.document
       where d.stable_id = ${docId} AND p.pagenum = ${pageId}
    """.query[Int :: Int :: Int :: Int :: HNil]
      .unique
      .map({case l :: t :: w :: h :: HNil =>
        val (bl, bt, bw, bh) = (
          itod(l), itod(t), itod(w), itod(h)
        )
        PageGeometry(pageId, LTBounds(bl, bt, bw, bh))
      })

    query
  }

  def selectPageImage(docId: String@@DocumentID, pageId: Int@@PageID): ConnectionIO[Image] = {
    val query = sql"""
       select pageimg, bleft, btop, bwidth, bheight from page p join document d
       using p.document
       where d.stable_id = ${docId} AND p.pagenum = ${pageId}
    """.query[Array[Byte]]
      .unique
      .map({case bytes => Image(bytes)})


    query
  }


  def selectTargetRegionImage(targetRegion: TargetRegion): ConnectionIO[Option[Image]] = {
    val trUri = targetRegion.uri
    val query = sql"""
       select image from targetregion_image t join targetregion u
       using t.targetregion
       where t.targetregion.uri = ${trUri}
    """.query[Array[Byte]]
      .option
      .map(_.map(Image(_)))

    query
  }

  def insertTargetRegion(targetRegion: TargetRegion): ConnectionIO[Int] = {
    val TargetRegion(id, docId, pageId, LTBounds(l, t, w, h) ) = targetRegion
    val (bl, bt, bw, bh) = (dtoi(l), dtoi(t), dtoi(w), dtoi(h))
    val trUri = targetRegion.uri

    sql"""
       insert into targetregion (document, page, bleft, btop, bwidth, bheight)
       values (${docId}, ${pageId}, $bl, $bt, $bw, $bh)
    """.update.withUniqueGeneratedKeys[Int]("id")
  }

  def insertTargetRegionImage(targetRegion: TargetRegion, image: Image): ConnectionIO[Unit] = {
    val TargetRegion(id, docId, pageId, LTBounds(l, t, w, h) ) = targetRegion
    val (bl, bt, bw, bh) = (dtoi(l), dtoi(t), dtoi(w), dtoi(h))
    val trUri = targetRegion.uri

    val imagebytes = image.bytes
    val query = for {
      trPrKey <- sql"""
              insert into targetregion (document, page, bleft, btop, bwidth, bheight)
              values (${docId}, ${pageId}, $bl, $bt, $bw, $bh)
              """.update.withUniqueGeneratedKeys[Int]("id")

      _ <- (sql"""
              insert into targetregion_image (image, targetregion)
              values (${imagebytes}, ${trPrKey})
              """.update.run)

    } yield ()

    query
  }

  def getOrCreateTargetRegionImage(targetRegion: TargetRegion): ConnectionIO[Image] = {
    val TargetRegion(id, docId, pageId, bbox) = targetRegion

    val query = for {
      maybeTrClip <- selectTargetRegionImage(targetRegion)
      clipped     <- maybeTrClip match {
        case Some(tr) => tr.point[ConnectionIO]
        case None     => for {
          pageImage <- selectPageImage(docId, pageId)
          clippedImage = {
            Image(Array[Byte]())
          }
          _  <- insertTargetRegionImage(targetRegion, clippedImage)
        } yield clippedImage
      }
    } yield clipped

    query
  }

  def serveImageWithURI(targetRegion: TargetRegion): Image = {
    val TargetRegion(id, docId, pageId, bbox) = targetRegion

    getOrCreateTargetRegionImage(targetRegion)
      .transact(xa)
      .unsafePerformSync

  }
  // val imagesOpt = for {
  //   entry <- corpus.entry(docId)
  //   group <- entry.getArtifactGroup("page-images")
  // } yield {
  //   val image = ExtractImages.load(group.rootPath)
  //   val pageImage  = image.page(pageId)
  //   val pageGeometry = PageGeometry(pageId, LTBounds(
  //                                     0, 0, 594.0, 783.0
  //                                   ))
  //   val cropped = ExtractImages.cropTo(
  //     pageImage, bbox, pageGeometry
  //   )
  //   cropped
  // }

  None


}
