package edu.umass.cs.iesl.watr
package docstore

import doobie.imports._
import doobie.free.{ connection => C }
import scalaz.syntax.applicative._

import geometry._

import com.sksamuel.scrimage.Image
import databasics._
import corpora._

import scalaz.std.list._
import scalaz.syntax.traverse._

import textreflow._
import textreflow.data._
// import watrmarks.{StandardLabels => LB}
import watrmarks._
import utils.EnrichNumerics._
import TypeTags._
import PageComponentImplicits._


class TextReflowDB(
  val tables: TextReflowDBTables,
  dbname: String, dbuser: String, dbpass: String
) extends DoobiePredef {
  self =>

  def time[R](prefix: String)(block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println(s"${prefix}: " + (t1 - t0)/1000000.0d + "ms")
    result
  }

  import scalaz.concurrent.Task
  import doobie.hikari.imports._

  val xa: Transactor[Task] = (for {
    xa <- HikariTransactor[Task]("org.postgresql.Driver", s"jdbc:postgresql:${dbname}", dbuser, dbpass)
    _  <- xa.configure(hx => Task.delay( /* do something with hx */ ()))
  } yield xa).unsafePerformSync


  def runq[A](query: C.ConnectionIO[A]): A = {
    // time("runq") {
    // ensuring xa.shutdown
    query.transact(xa)
      .unsafePerformSync
  }

  def updateGetKey[A: Composite](key: String, up: Update0): C.ConnectionIO[A] = {
    up.withUniqueGeneratedKeys(key)
  }


  def dropAndRecreate() = runq {
    for{
      _ <- tables.dropAll.run
      _ <- tables.createAll
    } yield ()
  }

  def selectDocumentStableIds(): List[String@@DocumentID] = {
    runq {
      sql"""select stable_id from document""".query[String@@DocumentID].list
    }
  }


  def selectTargetRegion(regionId: Int@@RegionID): ConnectionIO[Model.TargetRegion] = {
    sql""" select * from targetregion where targetregion=${regionId} """
      .query[Model.TargetRegion].unique
  }

  // def selectTargetRegionPC(regionId: Int@@RegionID): TargetRegion = {
  //   val query = sql"""
  //    select
  //         tr.targetregion, d.stable_id, pg.pagenum, tr.bleft, tr.btop, tr.bwidth, tr.bheight
  //    from
  //         targetregion   as tr join
  //         page           as pg on (pg.page=tr.page) join
  //         document       as d  on (pg.document=d.document)
  //    where
  //       tr.targetregion=${regionId}
  //   """.query[TargetRegion].unique

  //   query.transact(xa).unsafePerformSync
  // }

  def selectTargetRegions(pageId: Int@@PageID): ConnectionIO[List[Int@@RegionID]] = {
    sql""" select targetregion from targetregion where page=${pageId} """
      .query[Int@@RegionID].list
  }

  def insertZone(docId: Int@@DocumentID): ConnectionIO[Int@@ZoneID] = {
    sql""" insert into zone (document) values ( ${docId} ) """
      .update.withUniqueGeneratedKeys[Int]("zone").map(ZoneID(_))
  }


  def selectZonesForDocument(docId: Int@@DocumentID): ConnectionIO[List[Int@@ZoneID]] = {
    sql""" select * from  zone where document=${docId} """
      .query[Int@@ZoneID].list
  }
  def selectZone(zoneId: Int@@ZoneID): ConnectionIO[Model.Zone] = {
    sql""" select * from  zone where zone=${zoneId} """
      .query[Model.Zone].unique
  }

  def selectTextReflow(textReflowId: Int@@TextReflowID): ConnectionIO[Model.TextReflow] = {
    sql"""
     select * from  textreflow
     where textreflow=${textReflowId}
    """.query[Model.TextReflow].unique
  }


  def insertTextReflow(zonePk: Int@@ZoneID, textReflow: TextReflow): ConnectionIO[Int] = {
    import TextReflowJsonCodecs._
    import play.api.libs.json, json._
    val js = textReflow.toJson()
    val jsStr = Json.stringify(js)
    val query = for {
      pk <- sql"""
       insert into textreflow (reflow, zone)
       values (${jsStr}, ${zonePk})
      """.update.withUniqueGeneratedKeys[Int]("textreflow")
    } yield { pk }

    query
  }


  def selectZoneTargetRegions(zoneId: Int@@ZoneID): ConnectionIO[List[Int@@RegionID]] = {
    sql"""
     select z2tr.targetregion
     from
        zone                      as zn
        join zone_to_targetregion as z2tr  on (zn.zone=z2tr.zone)
     where
        z2tr.zone=${zoneId}
    """.query[Int@@RegionID].list
  }

  def selectZoneLabels(zoneId: Int@@ZoneID): ConnectionIO[List[Model.Label]] = {
    sql"""
     select lb.*
     from
        zone                      as zn
        join zone_to_label        as z2l   on (zn.zone=z2l.zone)
        join label                as lb    on (z2l.label=lb.label)
     where
        z2l.zone=${zoneId}
    """.query[Model.Label].list
  }

  def selectZoneForTargetRegion(regionId: Int@@RegionID, label: Label): ConnectionIO[Option[Int@@ZoneID]] = {
    val query = sql"""
     select z2tr.zone
     from
        zone                      as zn
        join zone_to_label        as z2l   on (zn.zone=z2l.zone)
        join label                as lb    on (z2l.label=lb.label)
        join zone_to_targetregion as z2tr  on (zn.zone=z2tr.zone)
     where
        z2tr.targetregion=${regionId}
    """.query[Int@@ZoneID].option
    // tr.targetregion=${regionId} AND lb.key=${label.fqn}

    query
  }

  def selectTextReflowForZone(zoneId: Int@@ZoneID): ConnectionIO[Option[TextReflow]] = {
    import TextReflowJsonCodecs._
    import play.api.libs.json, json._

    val query = sql"""
     select tr.reflow
     from   textreflow as tr join zone as z on (tr.zone=z.zone)
     where  z.zone=${zoneId}
    """.query[String]
      .option
      .map({maybeStr =>
        maybeStr.map({str =>
          jsonToTextReflow(Json.parse(str))
        })
      })

    query
  }

  def updatePage(pageId: Int@@PageID, geom: LTBounds): ConnectionIO[Int] = {
    val LTBounds(l, t, w, h) = geom
    val (bl, bt, bw, bh) = (dtoi(l), dtoi(t), dtoi(w), dtoi(h))

    sql"""
       update page set bleft=$bl, btop=$bt, bwidth=$bw, bheight=$bh
    """.update.run
  }

  def insertPage(docPrKey: Int@@DocumentID, pageNum: Int@@PageNum): ConnectionIO[Int@@PageID] = {
    updateGetKey("page",
      sql""" insert into page
                (document, pagenum, pageimg, bleft, btop, bwidth, bheight)
         values (${docPrKey}, ${pageNum}, null, 0, 0, 0, 0) """.update
    )
  }

  def selectPages(docId: Int@@DocumentID): ConnectionIO[List[Int@@PageID]] = {
    sql"""
     select pg.page
     from   page as pg
            join document as d on (pg.document=d.document)
     where d.document = ${docId}
     order by pg.pagenum
    """.query[Int]
      .map(PageID(_))
      .list
  }

  def selectPageForDocument(docId: Int@@DocumentID, pageNum: Int@@PageNum): ConnectionIO[Int@@PageID] = {
    sql"""
     select pg.page
     from   page as pg join document as d on (pg.document=d.document)
     where  d.document=${docId} AND pg.pagenum=${pageNum}
    """.query[Int@@PageID].unique
  }

  def selectPage(pageId: Int@@PageID): ConnectionIO[Model.Page] = {
    sql"""
     select pg.page, pg.document, pg.pagenum, imageclip, pg.bleft, pg.btop, pg.bwidth, pg.bheight
     from   page as pg
     where  pg.page=${pageId}
    """.query[Model.Page].unique
  }

  def selectPageImage(pageId: Int@@PageID): ConnectionIO[Option[Array[Byte]]] = {
    sql"""select pageimg from page where page=${pageId}"""
      .query[Array[Byte]].option
  }

  // // def selectPageGeometry(docId: Int@@DocumentID, pageNum: Int@@PageNum): ConnectionIO[LTBounds] = {
  // def selectPageGeometry(pageId: Int@@PageID): ConnectionIO[LTBounds] = {
  //   sql"""
  //      select bleft, btop, bwidth, bheight from page where page=${pageId}
  //   """.query[LTBounds].unique
  // }
  // def getPageGeometry(stableId: String@@DocumentID, pageNum: Int@@PageNum): PageGeometry= {
  //   val query = for {
  //     pageGeometry <- selectPageGeometry(stableId, pageNum)
  //   } yield pageGeometry

  //   query.transact(xa).unsafePerformSync
  // }
  // def getPageImageAndGeometry(stableId: String@@DocumentID, pageNum: Int@@PageNum): Option[(Image, PageGeometry)] = {
  //   val query = for {
  //     docId <- selectDocumentID(stableId)
  //     pageId <- selectPage(docId, pageNum)
  //     pageImage    <- selectPageImage(stableId, pageNum)
  //     pageGeometry <- selectPageGeometry(stableId, pageNum)
  //   } yield pageImage.map(i => (i, pageGeometry))

  //   query.transact(xa).unsafePerformSync
  // }

  def updatePageImage(pageId: Int@@PageID, pageImage: Image): Update0 = {
    sql"""update page set pageimg=${pageImage.bytes}""".update
  }

  def updatePageGeometry(pageId: Int@@PageID, pageBounds: LTBounds): Update0 = {
    val LTBounds(l, t, w, h) = pageBounds
    val (bl, bt, bw, bh) = (dtoi(l), dtoi(t), dtoi(w), dtoi(h))

    sql"""
       update page set bleft=${bl}, btop=${bt}, bwidth=${bw}, bheight=${bh}
       where page = ${pageId}
    """.update
  }


  def insertDocumentID(stableId: String@@DocumentID): ConnectionIO[Int@@DocumentID] = {
    sql""" insert into document (stable_id) values (${stableId}) """.update
      .withUniqueGeneratedKeys[Int]("document")
      .map(DocumentID(_))
  }

  def getDocuments(): List[String@@DocumentID] = {
    runq{
      sql"select stable_id from document".query[String@@DocumentID].list
    }
  }

  def selectDocumentID(stableId: String@@DocumentID): ConnectionIO[Int@@DocumentID] = {
    sql"select document from document where stable_id=${stableId}"
      .query[Int].unique.map(DocumentID(_))
  }

  def selectDocument(docId: Int@@DocumentID): ConnectionIO[Model.Document] = {
    sql"select * from document where document=${docId}"
      .query[Model.Document].unique
  }

  def hasDocumentID(stableId: String@@DocumentID): Boolean = {
    val zz = runq{
      sql"select 1 from document where stable_id=${stableId}"
        .query[Int].option
    }
    zz.isDefined
  }
  def getOrInsertDocumentID(stableId: String@@DocumentID): ConnectionIO[Int@@DocumentID] = {
    sql"""select document from document where stable_id=${stableId}"""
      .query[Int].option
      .flatMap({
        case Some(pk) => FC.delay(DocumentID(pk))
        case None => insertDocumentID(stableId)
      })
  }




  def deleteTargetRegionImage(targetRegion: TargetRegion): ConnectionIO[Int] = {
    val trUri = targetRegion.uri
    sql"""
       delete from targetregion_image
       using
          targetregion as tr
       where
          tr.targetregion = targetregion_image.targetregion AND
          tr.uri = ${trUri}
    """.update.run
  }

  def selectTargetRegionImageBytes(targetRegion: TargetRegion): ConnectionIO[Option[Array[Byte]]] = {
    val trUri = targetRegion.uri
    sql"""
       select img.image
       from targetregion_image as img join targetregion as tr
       on img.targetregion=tr.targetregion
       where tr.uri = ${trUri}
    """.query[Array[Byte]]
      .option
  }

  def selectTargetRegionImage(targetRegion: TargetRegion): ConnectionIO[Option[Image]] = {
    selectTargetRegionImageBytes(targetRegion)
      .map(_.map(Image(_)))

  }

  def ensureLabel(label: Label): ConnectionIO[Int] = for {
    maybePk <- sql"""select label from label where key=${label.fqn}""".query[Int].option
    pk      <- maybePk match {
      case Some(id)    => id.point[ConnectionIO]
      case None        => sql"""insert into label (key) values (${label.fqn})""".update.run
    }
  } yield pk


  def linkZoneToLabel(zonePk: Int@@ZoneID, label: Label): ConnectionIO[Unit] = {
    for {
      labelPk <- ensureLabel(label)
      _       <- sql"""insert into zone_to_label (zone, label) values (${zonePk}, ${labelPk})""".update.run
    } yield ()
  }

  def linkZoneToTargetRegion(zoneId: Int@@ZoneID, regionId: Int@@RegionID): ConnectionIO[Unit] = {
    sql"""
       insert into zone_to_targetregion (zone, targetregion)
       values (${zoneId}, ${regionId})
    """.update.run.map(_ => ())
  }


  def selectPage(docId: Int@@DocumentID, pageNum: Int@@PageNum): ConnectionIO[Option[Int@@PageID]] = {
    sql"""select page from page where document=${docId} and pagenum=${pageNum}"""
      .query[Int]
      .option
      .map(_.map(PageID(_)))
  }


  def insertTargetRegion(pageId: Int@@PageID, bbox: LTBounds): ConnectionIO[Int@@RegionID] = {
    val LTBounds(l, t, w, h) = bbox
    val (bl, bt, bw, bh) = (dtoi(l), dtoi(t), dtoi(w), dtoi(h))

    for {
      page       <- selectPage(pageId)
      doc        <- selectDocument(page.document)
      uri         = createTargetRegionUri(doc.stableId, page.pagenum, bbox)
      regionId   <- sql"""
                     insert into targetregion (page, imageclip, bleft, btop, bwidth, bheight, uri)
                     values ($pageId, ${Option.empty[Int]}, $bl, $bt, $bw, $bh, $uri)
                    """.update.withUniqueGeneratedKeys[Int]("targetregion").map(RegionID(_))
    } yield regionId
  }

  def insertTargetRegionImage(regionId: Int@@RegionID, image: Image): ConnectionIO[Int] = {
    sql"""
        insert into targetregion_image (image, targetregion)
        values (${image.bytes}, ${regionId})
    """.update.run
  }


  // def overwriteTargetRegionImage(targetRegion: TargetRegion, image: Image): Unit = {
  //   val query = for {
  //     // _  <- putStrLn(s"overwriteTargetRegionImage for ${targetRegion}")
  //     _  <- deleteTargetRegionImage(targetRegion)
  //     _  <- insertTargetRegionImage(targetRegion, image)
  //   } yield ()

  //   query
  //     .transact(xa)
  //     .unsafePerformSync
  // }
  // def putTargetRegionImage(targetRegion: TargetRegion, image: Image): Unit = {
  //   val query = for {
  //     // _           <- putStrLn(s"putTargetRegionImage for ${targetRegion}")
  //     maybeTrClip <- selectTargetRegionImage(targetRegion)
  //     clipped     <- maybeTrClip match {
  //       case Some(tr) => FC.delay(()) // putStrLn(s"  image exists")
  //       case None     => insertTargetRegionImage(targetRegion, image)
  //     }
  //   } yield ()

  //   query
  //     .transact(xa)
  //     .unsafePerformSync
  // }

  // import extract.images.ExtractImages

  def getOrCreateTargetRegionImage(targetRegion: TargetRegion): ConnectionIO[Array[Byte]] = {
    // val TargetRegion(id, docId, pageId, bbox) = targetRegion

    // val query = for {
    //   maybeTrClip <- selectTargetRegionImageBytes(targetRegion)
    //   clipped     <- maybeTrClip match {
    //     case Some(tr) => tr.point[ConnectionIO]
    //     case None     => for {
    //       pageImage <- selectPageImage(docId, pageId)
    //       pageGeometry <- selectPageGeometry(docId, pageId)
    //       maybeClippedImage = {
    //         pageImage.map(i => (i, pageGeometry)).map({case (pg, geom) =>
    //           ExtractImages.cropTo(pg, bbox, geom)
    //         })
    //       }
    //       clippedImage = maybeClippedImage.getOrElse { sys.error(s"getOrCreateTargetRegionImage: ${targetRegion}") }
    //       _  <- insertTargetRegionImage(targetRegion, clippedImage)
    //     } yield clippedImage.bytes
    //   }
    // } yield clipped

    // query
    ???
  }

  def serveImageWithURI(targetRegion: TargetRegion): Array[Byte] = {
    runq{
      getOrCreateTargetRegionImage(targetRegion)
    }
  }



  object docstorage extends DocumentCorpus {
    def getDocuments(): Seq[String@@DocumentID] = {
      selectDocumentStableIds()
    }
    def addDocument(stableId: String@@DocumentID): Int@@DocumentID = runq{
      self.getOrInsertDocumentID(stableId)
    }

    def getDocument(stableId: String@@DocumentID): Option[Int@@DocumentID] = runq {
      sql"""select document from document where stable_id=${stableId}"""
        .query[Int@@DocumentID]
        .option
    }

    def addPage(docId: Int@@DocumentID, pageNum: Int@@PageNum): Int@@PageID = {
      runq { insertPage(docId, pageNum) }
    }

    def getPage(docId: Int@@DocumentID, pageNum: Int@@PageNum): Option[Int@@PageID] = {
      runq { selectPage(docId, pageNum) }
    }

    def getPages(docId: Int@@DocumentID): Seq[Int@@PageID] = {
      runq{ selectPages(docId) }
    }

    def getPageGeometry(pageId: Int@@PageID): LTBounds = {
      runq { selectPage(pageId).map(_.bounds) }
    }

    def getPageImage(pageID: Int@@PageID): Option[Array[Byte]] = {
      ???
    }
    def setPageGeometry(pageId: Int@@PageID, geom: LTBounds): Unit = {
      runq {
        updatePage(pageId, geom)
          // .flatMap(_ => ())
      }
    }

    def setPageImage(pageId: Int@@PageID, bytes: Array[Byte]): Unit = {
      ???
    }

    def addCharAtom(pageId: Int@@PageID, charAtom: CharAtom): Unit = {

    }

    def getCharAtoms(pageId: Int@@PageID): Seq[CharAtom] = {
      ???
    }

    def addTargetRegion(pageId: Int@@PageID, bbox:LTBounds): Int@@RegionID = {
      runq { insertTargetRegion(pageId, bbox) }
    }

    // G.TargetRegion = M.TargetRegion+M.Page+M.Document
    def selTargetRegion(regionId: Int@@RegionID): ConnectionIO[TargetRegion] =
      for {
        tr <- selectTargetRegion(regionId)
        page <- selectPage(tr.page)
        doc <- selectDocument(page.document)
      } yield {
        TargetRegion(tr.prKey, doc.stableId, page.pagenum, tr.bounds)
      }

    def getTargetRegion(regionId: Int@@RegionID): TargetRegion =
      runq { selTargetRegion(regionId) }

    def setTargetRegionImage(regionId: Int@@RegionID, bytes: Array[Byte]): Unit = {
      ???
    }

    def getTargetRegionImage(regionId: Int@@RegionID): Option[Array[Byte]] = {
      ???
    }
    def deleteTargetRegionImage(regionId: Int@@RegionID): Unit = {
      ???
    }

    def getTargetRegions(pageId: Int@@PageID): Seq[Int@@RegionID] = {
      runq { selectTargetRegions(pageId) }
    }


    def createZone(docId: Int@@DocumentID): Int@@ZoneID = {
      runq {
        insertZone(docId)
      }
    }

    def getZone(zoneId: Int@@ZoneID): Zone = runq {
      // Zone = M.Zone+TargetRegions+Labels
      for {
        zone     <- selectZone(zoneId)
        regions  <- {
          selectZoneTargetRegions(zone.prKey)
            .flatMap(ts =>
              ts.map(t => selTargetRegion(t)).sequenceU
            )
        }
        labels  <- {
          selectZoneLabels(zone.prKey)
            .map(_.map(mlabel =>
              Labels.fromString(mlabel.key).copy(
                id=mlabel.prKey
              )
            ))
        }
      } yield {
        Zone(zone.prKey, regions, labels)
      }
    }

    def setZoneTargetRegions(zoneId: Int@@ZoneID, targetRegions: Seq[TargetRegion]): Unit = {
      targetRegions.foreach { tr =>
        runq{ linkZoneToTargetRegion(zoneId, tr.id) }
      }
    }


    def addZoneLabel(zoneId: Int@@ZoneID, label: Label): Unit = {
      runq { linkZoneToLabel(zoneId, label) }
    }

    def getZonesForDocument(docId: Int@@DocumentID, label: Option[Label]=None): Seq[Int@@ZoneID] = {
      runq {
        selectZonesForDocument(docId)
      }
    }

    def getZoneForTargetRegion(regionId: Int@@RegionID, label: Label): Option[Int@@ZoneID] = {
      runq { selectZoneForTargetRegion(regionId, label) }
    }


    def getTextReflowForZone(zoneId: Int@@ZoneID): Option[TextReflow] = {
      runq { selectTextReflowForZone(zoneId) }
    }

    def setTextReflowForZone(zoneId: Int@@ZoneID, textReflow: TextReflow): Unit = {
      runq { insertTextReflow(zoneId, textReflow) }
    }

    def deleteZone(zoneId: Int@@ZoneID): Unit = {
      ???
    }

  }

}


  // def addSegmentation(ds: DocumentSegmentation): Unit = {

  //   val mpageIndex = ds.mpageIndex
  //   val docId = mpageIndex.docId
  //   val stableId = mpageIndex.getStableId()

  //   def insertPagesInfo(docPk: Int) = for {
  //     pageId <- mpageIndex.getPages.toList
  //   } yield for {
  //     // _         <- putStrLn(s"inserting page rec for page ${pageId}")
  //     pagePrKey <- insertPageGeometry(docPk, mpageIndex.getPageGeometry(pageId), ds.pageImages.page(pageId))
  //   } yield ()

  //   val query = for {
  //     // _         <- putStrLn("Starting ...")
  //     docPrKey  <- getOrInsertDocumentID(stableId)
  //     _         <- insertPagesInfo(docPrKey).sequenceU
  //     // _         <- putStrLn("insert multi pages...")
  //     _         <- insertMultiPageIndex(docId, mpageIndex)
  //   } yield docPrKey

  //   val docPrKey = query.transact(xa).unsafePerformSync


  // }

  // def insertMultiPageIndex(stableId: String@@DocumentID, mpageIndex: MultiPageIndex): ConnectionIO[List[Unit]] = {
  //   // To start, just create entries for all Component/Zones labelled VisualLine
  //   val query: ConnectionIO[List[Unit]] = (for {
  //     vlineCC      <- mpageIndex.getDocumentVisualLines.flatten.toList
  //     maybeReflow   = mpageIndex.getTextReflowForComponent(vlineCC.id)
  //     vlineZoneId   = mpageIndex.getZoneForComponent(vlineCC.id).get
  //   } yield for {
  //     // _            <- putStrLn(s"Inserting zone for line ${vlineCC.id}")
  //     zoneId          <- insertZone(docId)
  //     treflowPk    <- maybeReflow.toList.map(r => insertTextReflow(zoneId, r)).sequence

  //     // insert zone -> targetregion (w/ordering)
  //     // _            <- putStrLn(s"   inserting targetregion")
  //     targetRegPk  <- insertTargetRegion(vlineCC.targetRegion)
  //     regionID = RegionID(targetRegPk)
  //     // _            <- putStrLn(s"   linking zone -> targetregion ")
  //     _            <- linkZoneToTargetRegion(zoneId, regionID)

  //     // insert zone -> label (VisualLine is the only important label right now)
  //     // _            <- putStrLn(s"   linking zone -> label ")
  //     _            <- linkZoneToLabel(zoneId, LB.VisualLine)

  //     // insert targetregion images for VisualLines
  //     // _            <- putStrLn(s"   creating VisualLines images")
  //     // _            <- getOrCreateTargetRegionImage(vlineCC.targetRegion)
  //   } yield ()).sequenceU

  //   query
  // }

  // def selectZonesXXX(stableId: String@@DocumentID, pageNum: Int@@PageNum, label: Label): List[Zone] = {
  //   val query = sql"""
  //    select
  //       zn.zone, tr.targetregion, d.stable_id, pg.pagenum, tr.bleft, tr.btop, tr.bwidth, tr.bheight
  //    from
  //       zone                      as zn
  //       join zone_to_label        as z2l   on (zn.zone=z2l.zone)
  //       join label                as lb    on (z2l.label=lb.label)
  //       join zone_to_targetregion as z2tr  on (zn.zone=z2tr.zone)
  //       join targetregion         as tr    on (z2tr.targetregion=tr.targetregion)
  //       join page                 as pg    on (pg.page=tr.page)
  //       join document             as d     on (pg.document=d.document)
  //    where
  //       d.stable_id=${docId}
  //       AND pg.pagenum=${pageNum}
  //       AND lb.key=${label.fqn}
  //   """.query[ZonedRegion]
  //     .list
  //     .map(_.foldLeft(List[Zone]())({ case (zones, zrec) =>
  //       import scala.::
  //       zones match {
  //         case Nil =>
  //           Zone(zrec.id, List(zrec.targetRegion), List()) :: Nil

  //         case h :: tail if h.id == zrec.id =>
  //           h.copy(regions = zrec.targetRegion +: h.regions) :: tail

  //         case h :: tail =>
  //           Zone(zrec.id, List(zrec.targetRegion), List()) :: zones
  //       }

  //     }))
  //     .map({zoneList =>
  //       zoneList
  //         .map(z => z.copy(regions = z.regions.reverse))
  //         .reverse
  //     })

  //   query.transact(xa).unsafePerformSync
  // }
