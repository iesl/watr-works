package edu.umass.cs.iesl.watr
package textreflow

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

class TextReflowDB(
  val tables: TextReflowDBTables
) extends DoobiePredef {

  lazy val xa = tables.xa

  def addSegmentation(ds: DocumentSegmentation): Unit = {

    val mpageIndex = ds.mpageIndex
    val docId = mpageIndex.getDocumentID()

    def insertPages(docPk: Int) = for {
      pageId <- mpageIndex.getPages.toList
    } yield for {
      _         <- putStrLn(s"inserting page rec for page ${pageId}")
      pagePrKey <- insertPageIndex(docPk, mpageIndex.getPageIndex(pageId), ds.pageImages.page(pageId))
    } yield ()

    val query = for {
      _         <- putStrLn("Starting ...")
      docPrKey  <- getOrInsertDocumentID(docId)
      _         <- insertPages(docPrKey).sequenceU
      _         <- putStrLn("insert multi pages...")
      _         <- insertMultiPageIndex(docId, mpageIndex)
    } yield docPrKey

    val docPrKey = query.transact(xa).unsafePerformSync


  }

  def insertMultiPageIndex(docId: String@@DocumentID, mpageIndex: MultiPageIndex): ConnectionIO[List[Unit]] = {
    // To start, just create entries for all Component/Zones labelled VisualLine

    val query: ConnectionIO[List[Unit]] = (for {
      vlineCC      <- mpageIndex.getDocumentVisualLines.flatten.toList
      maybeReflow   = mpageIndex.getTextReflowForComponent(vlineCC.id)
      vlineZoneId   = mpageIndex.getZoneForComponent(vlineCC.id).get
    } yield for {
      _            <- putStrLn(s"Inserting zone for line ${vlineCC.id}")
      zpk          <- insertZone(docId, vlineZoneId)
      _            <- putStrLn(s"   inserting textreflow")
      treflowPk    <- maybeReflow.toList.map(r => insertTextReflow(zpk, r)).sequence

      // insert zone -> targetregion (w/ordering)
      _            <- putStrLn(s"   inserting targetregion")
      targetRegPk  <- insertTargetRegion(vlineCC.targetRegion)
      _            <- putStrLn(s"   linking zone -> targetregion ")
      _            <- linkZoneToTargetRegion(zpk, targetRegPk)

      // insert zone -> label (VisualLine is the only important label right now)
      _            <- linkZoneToLabel(zpk, LB.VisualLine)

    } yield ()).sequenceU

    query
  }

  def insertZone(docId: String@@DocumentID, zoneId: Int@@ZoneID): ConnectionIO[Int] = {
    sql"""
       insert into zone (zoneid, document)
       values (${zoneId}, (select id from document where stable_id=${docId}))
    """.update.withUniqueGeneratedKeys[Int]("id")
  }

  def insertTextReflow(zonePk: Int, textReflow: TextReflow): ConnectionIO[Int] = {
    import TextReflowJsonCodecs._
    import play.api.libs.json, json._
    val js = textReflow.toJson()
    val jsStr = Json.stringify(js)
    sql"""
       insert into textreflow (reflow, zone)
       values (${jsStr}, ${zonePk})
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

  def selectDocumentID(docId: String@@DocumentID): ConnectionIO[Int] = {
    sql"select id from document where stable_id=${docId}"
      .query[Int].unique
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

  import watrmarks.Label

  def linkZoneToLabel(zonePk: Int, label: Label): ConnectionIO[Unit] = {
    for {
      labelPk <- sql""" insert into label (key) values (${label.fqn}) """.update.run
      _       <- sql""" insert into zone_to_label (zone, label) values (${zonePk}, ${labelPk})""".update.run
    } yield ()
  }

  def linkZoneToTargetRegion(zonePk: Int, targetRegionPk: Int): ConnectionIO[Unit] = {
    sql"""
       insert into zone_to_targetregion (zone, targetregion)
       values (${zonePk}, ${targetRegionPk})
    """.update.run.map(_ => ())
  }


  def selectPage(docPk: Int, pageId: Int@@PageID): ConnectionIO[Int] = {
    sql"""select id from page where document=${docPk} and pagenum=${pageId.unwrap}"""
      .query[Int].unique
  }

  def insertTargetRegion(targetRegion: TargetRegion): ConnectionIO[Int] = {
    val TargetRegion(id, docId, pageId, LTBounds(l, t, w, h) ) = targetRegion
    val (bl, bt, bw, bh) = (dtoi(l), dtoi(t), dtoi(w), dtoi(h))

    for {
      docPk <- selectDocumentID(docId)
      _   <- putStrLn(s"   selected docid ${docPk}")
      pagePk <- selectPage(docPk, pageId)
      _   <- putStrLn(s"   selected pageid ${pagePk}")
      trPk <- sql"""
       insert into targetregion (page, bleft, btop, bwidth, bheight, uri)
       values (${pagePk}, $bl, $bt, $bw, $bh, ${targetRegion.uri})
      """.update.run
    } yield trPk

  }

  def insertTargetRegionImage(targetRegion: TargetRegion, image: Image): ConnectionIO[Unit] = {
    val imagebytes = image.bytes
    for {

     trPk <- insertTargetRegion(targetRegion)

      _ <- (sql"""
              insert into targetregion_image (image, targetregion)
              values (${imagebytes}, ${trPk})
              """.update.run)
    } yield ()
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
