package edu.umass.cs.iesl.watr
package textreflow //;import acyclic.file

import doobie.imports._

import scalaz.concurrent.Task
import scalaz.syntax.applicative._

import geometry._
import spindex._
import segment._

import com.sksamuel.scrimage._
import shapeless._


class TextReflowDB(
  val tables: TextReflowDBTables
) {

  lazy val xa = tables.xa

  def addSegmentation(documentSegmentation: DocumentSegmentation): Unit = {
    addMultiPageIndex(documentSegmentation)
  }

  def addMultiPageIndex(ds: DocumentSegmentation): Unit = {
    val mpageIndex = ds.mpageIndex
    val docId = mpageIndex.getDocumentID()

    for {
      pageId <- mpageIndex.getPages // .zip(ds.pageImages)
    } {
      val query = for {
        docPrKey <- upsertDocumentID(docId)
        pagePrKey <- insertPageIndex(docPrKey, mpageIndex.getPageIndex(pageId), ds.pageImages.page(pageId))
      } yield ()

      query
        .transact(xa)
        .unsafePerformSync
    }
  }

  def dtoi(d: Double): Int = (d*100.0).toInt
  def itod(i: Int): Double = (i.toDouble)/100.0

  // import doobie.util.composite._
  def insertPageIndex(docPrKey: Int, pageIndex: PageIndex, pageImage: Image): ConnectionIO[Int] = {

    println(s"adding page w/image dims ${pageImage.dimensions} ")

    val PageGeometry(pageId, LTBounds(l, t, w, h)) = pageIndex.pageGeometry
    val (bl, bt, bw, bh) = (dtoi(l), dtoi(t), dtoi(w), dtoi(h))

    sql"""
      insert into page (document, pagenum, pageimg, bleft, btop, bwidth, bheight)
      values (${docPrKey}, ${pageId.unwrap}, ${pageImage.bytes}, $bl, $bt, $bw, $bh)
    """.update.withUniqueGeneratedKeys("id")

  }

  def upsertDocumentID(docId: String@@DocumentID): ConnectionIO[Int] = {
    sql"insert into document (stable_id) values (${docId.unwrap})"
      .update.withUniqueGeneratedKeys("id")
  }


  def selectPageGeometry(docId: String@@DocumentID, pageId: Int@@PageID): ConnectionIO[PageGeometry] = {
    val query = sql"""
       select bleft, btop, bwidth, bheight from page p join document d
       using p.document
       where d.stable_id = ${docId.unwrap} AND p.pagenum = ${pageId.unwrap}
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
       where d.stable_id = ${docId.unwrap} AND p.pagenum = ${pageId.unwrap}
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

  def insertTargetRegionImage(targetRegion: TargetRegion, image: Image): ConnectionIO[Unit] = {
    val TargetRegion(id, docId, pageId, LTBounds(l, t, w, h) ) = targetRegion
    val (bl, bt, bw, bh) = (dtoi(l), dtoi(t), dtoi(w), dtoi(h))
    val trUri = targetRegion.uri

    val imagebytes = image.bytes
    val query = for {
      trPrKey <- sql"""
              insert into targetregion (document, page, bleft, btop, bwidth, bheight)
              values (${docId.unwrap}, ${pageId.unwrap}, $bl, $bt, $bw, $bh)
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
  //   entry <- corpus.entry(docId.unwrap)
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


class TextReflowDBTables(
  val xa: Transactor[Task]
) {
  import xa.yolo._

  val createDocumentTable: Update0 = sql"""
      CREATE TABLE document (
        id            SERIAL PRIMARY KEY,
        stable_id     VARCHAR(128)
      );
      CREATE INDEX document_stable_id ON document USING hash (stable_id);
    """.update

  val createPageTable: Update0 = sql"""
      CREATE TABLE page (
        id          SERIAL PRIMARY KEY,
        document    INTEGER REFERENCES document,
        pagenum     SMALLINT,
        pageimg     BYTEA,
        bleft       INTEGER,
        btop        INTEGER,
        bwidth      INTEGER,
        bheight     INTEGER
      );
    """.update

  val createTargetRegion: Update0 = sql"""
      CREATE TABLE targetregion (
        id          SERIAL PRIMARY KEY,
        document    INTEGER REFERENCES document,
        page        INTEGER REFERENCES page,
        bleft       INTEGER,
        btop        INTEGER,
        bwidth      INTEGER,
        bheight     INTEGER,
        uri         VARCHAR(256) UNIQUE
      );
      CREATE INDEX targetregion_uri ON targetregion USING hash (uri);
    """.update

  val createLabelTable: Update0 = sql"""
      CREATE TABLE label (
        id             SERIAL PRIMARY KEY,
        ns             VARCHAR(8),
        key            VARCHAR(20),
        value          VARCHAR(256)
      );
    """.update

  val createTextReflowTable: Update0 = sql"""
      CREATE TABLE textreflow (
        id            SERIAL PRIMARY KEY,
        reflow        JSON,
        targetregion  INTEGER REFERENCES targetregion
      )
    """.update


  val createTargetRegionImageTable: Update0 = sql"""
      CREATE TABLE targetregion_image (
        id            SERIAL PRIMARY KEY,
        image         BYTEA,
        targetregion  INTEGER REFERENCES targetregion
      )
    """.update

  def createAll = {
    (createDocumentTable.quick
       *> createPageTable.quick
       *> createTargetRegion.quick
       *> createTextReflowTable.quick
       *> createLabelTable.quick
       *> createTargetRegionImageTable.quick)
  }
  val dropAll: Update0 = sql"""
    DROP TABLE IF EXISTS targetregion_image;
    DROP TABLE IF EXISTS textreflow;
    DROP TABLE IF EXISTS label;
    DROP TABLE IF EXISTS targetregion;
    DROP TABLE IF EXISTS page;
    DROP TABLE IF EXISTS document;
  """.update

  def dropAndCreate = {
    (dropAll.quick *> createAll)
  }

}
