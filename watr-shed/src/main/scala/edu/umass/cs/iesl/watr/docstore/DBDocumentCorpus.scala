package edu.umass.cs.iesl.watr
package docstore

import doobie.imports._
import doobie.free.{ connection => C }
import scalaz.syntax.applicative._

import geometry._

import databasics._
import corpora._

import scalaz.std.list._
import scalaz.syntax.traverse._

import textreflow._
import textreflow.data._
import watrmarks._
import utils.EnrichNumerics._
import TypeTags._
import play.api.libs.json, json._


class TextReflowDB(
  val tables: TextReflowDBTables,
  dbname: String, dbuser: String, dbpass: String
) extends DoobiePredef { self =>

  val Rel = RelationModel

  def time[R](prefix: String)(block: => R): R = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    println(s"${prefix}: " + (t1 - t0)/1000000.0d + "ms")
    result
  }

  import scalaz.concurrent.Task
  import doobie.hikari.imports._

  val xa: HikariTransactor[Task] = (for {
    xa <- HikariTransactor[Task]("org.postgresql.Driver", s"jdbc:postgresql:${dbname}", dbuser, dbpass)
    _  <- xa.configure(hx => Task.delay( /* do something with hx */ ()))
  } yield xa).unsafePerformSync


  def shutdown() = xa.shutdown

  def runq[A](query: C.ConnectionIO[A]): A = {
    try {
      query.transact(xa)
        .unsafePerformSync
    } catch {
      case t: Throwable =>
        val message = s"""error: ${t}: ${t.getCause}: ${t.getMessage} """
        println(s"ERROR: ${message}")
        t.printStackTrace()
        throw t
    }
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

  def selectTargetRegion(regionId: Int@@RegionID): ConnectionIO[Rel.TargetRegion] = {
    sql""" select * from targetregion where targetregion=${regionId} """
      .query[Rel.TargetRegion].unique
  }

  def selectTargetRegionForBbox(pageId: Int@@PageID, bbox: LTBounds): ConnectionIO[Option[Rel.TargetRegion]] = {
    val LTBounds(l, t, w, h) = bbox
    val (bl, bt, bw, bh) = (dtoi(l), dtoi(t), dtoi(w), dtoi(h))

    sql""" select * from targetregion where
       page=${pageId} AND
       bleft=${bl} AND btop=${bt} AND
       bwidth=${bw} AND bheight=${bt}
       order by rank
    """.query[Rel.TargetRegion].option
  }

  // def selectTargetRegionForUri(uri: String): ConnectionIO[Option[Rel.TargetRegion]] = {
  //   sql"""select * from targetregion where uri=${uri}"""
  //     .query[Rel.TargetRegion].option
  // }

  def selectTargetRegions(pageId: Int@@PageID): ConnectionIO[List[Int@@RegionID]] = {
    sql""" select targetregion from targetregion where page=${pageId} order by rank"""
      .query[Int@@RegionID].list
  }

  def insertZone(docId: Int@@DocumentID): ConnectionIO[Int@@ZoneID] = {
    sql""" insert into zone (document) values ( ${docId} ) """
      .update.withUniqueGeneratedKeys[Int]("zone").map(ZoneID(_))
  }


  def selectZonesForDocument(docId: Int@@DocumentID): ConnectionIO[List[Int@@ZoneID]] = {
    sql""" select * from  zone where document=${docId} order by rank"""
      .query[Int@@ZoneID].list
  }

  def selectZone(zoneId: Int@@ZoneID): ConnectionIO[Rel.Zone] = {
    sql""" select * from  zone where zone=${zoneId} order by rank """
      .query[Rel.Zone].unique
  }

  def selectTextReflow(textReflowId: Int@@TextReflowID): ConnectionIO[Rel.TextReflow] = {
    sql"""
     select * from  textreflow
     where textreflow=${textReflowId}
    """.query[Rel.TextReflow].unique
  }


  def insertTextReflow(zonePk: Int@@ZoneID, textReflowJs: String, asText: String): ConnectionIO[Int] = {
    for {
      pk <- sql"""
       insert into textreflow (reflow, astext, zone)
       values (${textReflowJs}, $asText, ${zonePk})
      """.update.withUniqueGeneratedKeys[Int]("textreflow")
    } yield pk
  }


  def selectZoneTargetRegions(zoneId: Int@@ZoneID): ConnectionIO[List[Int@@RegionID]] = {
    sql"""
     select z2tr.targetregion
     from
        zone                      as zn
        join zone_to_targetregion as z2tr  on (zn.zone=z2tr.zone)
     where
        z2tr.zone=${zoneId}
     order by z2tr.rank
    """.query[Int@@RegionID].list
  }

  def selectZoneLabels(zoneId: Int@@ZoneID): ConnectionIO[List[Rel.Label]] = {
    sql"""
     select lb.*
     from
        zone                      as zn
        join zone_to_label        as z2l   on (zn.zone=z2l.zone)
        join label                as lb    on (z2l.label=lb.label)
     where
        z2l.zone=${zoneId}
    """.query[Rel.Label].list
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
        z2tr.targetregion=${regionId} AND
        lb.key=${label.fqn}
    """.query[Int@@ZoneID].option

    query
  }

  def selectModelTextReflowForZone(zoneId: Int@@ZoneID): ConnectionIO[Option[Rel.TextReflow]] = {
    val query = sql"""
     select tr.*
     from   textreflow as tr join zone as z on (tr.zone=z.zone)
     where  z.zone=${zoneId}
    """.query[Rel.TextReflow].option

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
       update page set bleft=$bl, btop=$bt, bwidth=$bw, bheight=$bh where page=${pageId}
    """.update.run
  }

  def insertPage(docPrKey: Int@@DocumentID, pageNum: Int@@PageNum): ConnectionIO[Int@@PageID] = {
    updateGetKey("page",
      sql""" insert into page
                (document, pagenum, imageclip, bleft, btop, bwidth, bheight)
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

  def selectPage(pageId: Int@@PageID): ConnectionIO[Rel.Page] = {
    sql"""
     select pg.page, pg.document, pg.pagenum, imageclip, pg.bleft, pg.btop, pg.bwidth, pg.bheight
     from   page as pg
     where  pg.page=${pageId}
    """.query[Rel.Page].unique
  }

  def selectPageImage(pageId: Int@@PageID): ConnectionIO[Option[Array[Byte]]] = {
    sql"""select i.image
          from page as p join imageclips as i
               on(p.imageclip=i.imageclip)
          where p.page=${pageId}
       """.query[Array[Byte]].option
  }

  def selectTargetRegionImage(regionId: Int@@RegionID): ConnectionIO[Option[Array[Byte]]] = {
    sql"""select i.image
          from targetregion as tr join imageclips as i
               on(tr.imageclip=i.imageclip)
          where tr.targetregion=${regionId}
       """.query[Array[Byte]].option
  }

  def insertImageClip(imageBytes: Array[Byte]): ConnectionIO[Int] = {
    sql"""insert into imageclips (image) values(${imageBytes})"""
      .update.withUniqueGeneratedKeys[Int]("imageclip")
  }

  def deleteImageClip(clipId: Int@@ImageID): ConnectionIO[Int] = {
    sql"""delete from imageclips where imageclip=${clipId}""".update.run
  }

  def delTargetRegionImage(regionId: Int@@RegionID): ConnectionIO[Unit] = {
    for {
      tr <- selectTargetRegion(regionId)
      del <- {
        tr.imageclip.fold(FC.delay { 0 })(
          deleteImageClip(_)
        )
      }
      pup <- sql"""update targetregion set imageclip=null where targetregion=${regionId}""".update.run
    } yield ()
  }

  def updatePageImage(pageId: Int@@PageID, imageBytes: Array[Byte]): ConnectionIO[Int] = {
    for {
      page <- selectPage(pageId)
      del <- {
        page.imageclip.fold(FC.delay { 0 })(
          deleteImageClip(_)
        )
      }
      clipId <- insertImageClip(imageBytes)
      pup <- sql"""update page set imageclip=${clipId} where page=${pageId}""".update.run
    } yield pup

  }

  def updatePageGeometry(pageId: Int@@PageID, pageBounds: LTBounds): Update0 = {
    val LTBounds(l, t, w, h) = pageBounds
    val (bl, bt, bw, bh) = (dtoi(l), dtoi(t), dtoi(w), dtoi(h))

    sql"""
       update page set bleft=${bl}, btop=${bt}, bwidth=${bw}, bheight=${bh} where page = ${pageId}
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

  def selectDocument(docId: Int@@DocumentID): ConnectionIO[Rel.Document] = {
    sql"select * from document where document=${docId}"
      .query[Rel.Document].unique
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



  def ensureTargetRegion(pageId: Int@@PageID, bbox: LTBounds): ConnectionIO[Int@@RegionID] = {
    val LTBounds(l, t, w, h) = bbox
    val (bl, bt, bw, bh) = (dtoi(l), dtoi(t), dtoi(w), dtoi(h))

    for {
      page         <- selectPage(pageId)
      maybeRegion  <- selectTargetRegionForBbox(pageId, bbox)
      regionId     <- maybeRegion.fold(
          sql"""insert into targetregion (page, imageclip, bleft, btop, bwidth, bheight)
                 values ($pageId, null, $bl, $bt, $bw, $bh)
             """.update.withUniqueGeneratedKeys[Int]("targetregion").map(RegionID(_))
      )(tr => FC.delay(tr.prKey))
    } yield regionId
  }

  def createTargetRegionImage(regionId: Int@@RegionID): ConnectionIO[Array[Byte]] = {
    // val TargetRegion(regionId, stableId, pageNum, bbox) = targetRegion

    def cropTo(bs: Array[Byte], cbbox: LTBounds, pbbox: LTBounds): ConnectionIO[Array[Byte]] = {
      images.ImageManipulation.cropTo(bs, cbbox, pbbox)
        .bytes
        .point[ConnectionIO]
    }

    for {
      _              <- putStrLn(s"createTargetRegionImage(${regionId})")
      _              <- putStrLn(s"  selectTargetRegion(${regionId})")
      region         <- selectTargetRegion(regionId)
      _              <- putStrLn(s"  selectPage(${region})")
      page           <- selectPage(region.page)
      _              <- putStrLn(s"  selectPageImage(${page})")
      maybePageImage <- selectPageImage(region.page)

      imageBytes     <- maybePageImage.map(cropTo(_, region.bounds, page.bounds)).getOrElse { sys.error(s"  createTargetRegionImage: no page image found!") }
      // TODO delete old image if exists
      _              <- putStrLn("  insertTargetRegionImage()")
      clipId         <- insertTargetRegionImage(regionId, imageBytes)
    } yield {
      imageBytes
    }
  }

  def insertTargetRegionImage(regionId: Int@@RegionID, imageBytes: Array[Byte]): ConnectionIO[Int@@ImageID] = {
    for {
      clipId <- insertImageClip(imageBytes)
      pup <- sql"""update targetregion set imageclip=${clipId} where targetregion=${regionId}""".update.run
    } yield ImageID(clipId)
  }


  def getOrCreateTargetRegionImage(regionId: Int@@RegionID): ConnectionIO[Array[Byte]] = {
    for {
      _          <- putStrLn(s"getOrCreateTargetRegionImage(${regionId})")
      _          <- putStrLn("  selectTargetRegionImage()")
      maybeImage <- selectTargetRegionImage(regionId)
      _          <- putStrLn("  maybe createTargetRegionImage()")
      imageBytes <- maybeImage.fold(createTargetRegionImage(regionId))(_.point[ConnectionIO])
    } yield imageBytes
  }


  def serveTargetRegionImage(regionId: Int@@RegionID): Array[Byte] = {
    println(s"serveTargetRegionImage(${regionId})")
    runq{
      getOrCreateTargetRegionImage(regionId)
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

    def getPageImage(pageId: Int@@PageID): Option[Array[Byte]] = {
      runq { selectPageImage(pageId) }
    }

    def getPageDef(pageId: Int@@PageID): Option[Rel.Page] = {
      Option(
        runq { selectPage(pageId) }
      )
    }

    def setPageGeometry(pageId: Int@@PageID, geom: LTBounds): Unit = {
      runq { updatePage(pageId, geom) }
    }

    def setPageImage(pageId: Int@@PageID, bytes: Array[Byte]): Unit = {
      runq { updatePageImage(pageId, bytes) }
    }

    // def addCharAtom(pageId: Int@@PageID, charAtom: CharAtom): Unit = {}
    // def getCharAtoms(pageId: Int@@PageID): Seq[CharAtom] = {}

    def addTargetRegion(pageId: Int@@PageID, bbox:LTBounds): Int@@RegionID = {
      runq { ensureTargetRegion(pageId, bbox) }
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
      runq { insertTargetRegionImage(regionId, bytes) }
    }

    def getTargetRegionImage(regionId: Int@@RegionID): Option[Array[Byte]] = {
      runq { selectTargetRegionImage(regionId) }
    }

    def deleteTargetRegionImage(regionId: Int@@RegionID): Unit = {
      runq { delTargetRegionImage(regionId) }
    }

    def getTargetRegions(pageId: Int@@PageID): Seq[Int@@RegionID] = {
      runq { selectTargetRegions(pageId) }
    }


    def createZone(docId: Int@@DocumentID): Int@@ZoneID = {
      runq { insertZone(docId) }
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

    def getZoneForRegion(regionId: Int@@RegionID, label: Label): Option[Int@@ZoneID] = {
      runq { selectZoneForTargetRegion(regionId, label) }
    }


    def getModelTextReflowForZone(zoneId: Int@@ZoneID): Option[Rel.TextReflow] = {
      runq { selectModelTextReflowForZone(zoneId) }
    }
    def getTextReflowForZone(zoneId: Int@@ZoneID): Option[TextReflow] = {
      runq { selectTextReflowForZone(zoneId) }
    }

    def setTextReflowForZone(zoneId: Int@@ZoneID, textReflow: TextReflow): Unit = {
      import TextReflowJsonCodecs._
      val js = textReflow.toJson()
      val str = textReflow.toText()
      val jsStr = Json.stringify(js)
      runq { insertTextReflow(zoneId, jsStr, str) }
    }


    def createZone(regionId: Int@@RegionID): Int@@ZoneID = {
      ???
    }
    def createZone(zoneIds: Seq[Int@@ZoneID]): Int@@ZoneID = {
      ???
    }
    def addZoneLabel(zoneId: Int@@ZoneID, label: Label): Zone = {
      ???
    }
    def deleteZone(zoneId: Int@@ZoneID): Unit = {
      ???
    }
    def getZone(zoneId: Int@@ZoneID): Zone = {
      ???
    }
    def getZoneLabelsForDocument(docId: Int@@DocumentID): Seq[Label] = {
      ???
    }
    def getZonesForDocument(docId: Int@@DocumentID, label: Label): Seq[Zone] = {
      ???
    }
    def getZonesForRegion(geoRegion: GeometricRegion, label: Label): Zone = {
      ???
    }
    def getModelTextReflowForZone(zoneId: Int@@ZoneID): Option[Rel.TextReflow] = {
      ???
    }
    def getTextReflowForZone(zoneId: Int@@ZoneID): Option[TextReflow] = {
      ???
    }
    def setTextReflowForZone(zoneId: Int@@ZoneID, textReflow: TextReflow): Unit = {
      ???
    }

  }

}
