package edu.umass.cs.iesl.watr
package corpora

import textboxing.{TextBoxing => TB}, TB._
import TypeTags._

trait CorpusTestingUtil extends PlainTextCorpus {
  def createEmptyDocumentCorpus(): DocumentCorpus
  // def regionIdGen: utils.IdGenerator[RegionID]
  val regionIdGen = utils.IdGenerator[RegionID]()

  var freshDocstore: Option[DocumentCorpus] = None

  def docStore: DocumentCorpus = freshDocstore
    .getOrElse(sys.error("Uninitialized DocumentCorpus; Use CleanDocstore() class"))

  def initEmpty(): Unit = {
    try {
      regionIdGen.reset()
      freshDocstore = Some(createEmptyDocumentCorpus())
    } catch {
      case t: Throwable =>
        val message = s"""error: ${t}: ${t.getCause}: ${t.getMessage} """
        println(s"ERROR: ${message}")
        t.printStackTrace()
    }
  }

  trait CleanDocstore {
    initEmpty()
  }

  class FreshDocstore(pageCount: Int=0) {
    initEmpty()
    try {
      loadSampleDoc(pageCount)
    } catch {
      case t: Throwable =>
        val message = s"""error: ${t}: ${t.getCause}: ${t.getMessage} """
        println(s"ERROR: ${message}")
        t.printStackTrace()
    }
  }

  val stableId = DocumentID("stable-id#23")

  def loadSampleDoc(pageCount: Int): Unit = {
    val pages = MockPapers.genericTitle
      .take(pageCount)

    addDocument(stableId, pages)
  }

  def putStrLn(s: String): Seq[Unit] = Seq({
    println(s)
  })

  def visualizeDocStore(): Unit = {
    for {
      stableId     <- docStore.getDocuments()
      docId        <- docStore.getDocument(stableId).toSeq
      _            <- putStrLn(s"Document $stableId id:${docId}")
      pageId       <- docStore.getPages(docId)
      pageGeometry  = docStore.getPageGeometry(pageId)
      _            <- putStrLn(s"  Page  ${pageId}: ${pageGeometry}")
    } ()
  }

  def visualizeDocuments(): TB.Box = {
    val docBoxes = for {
      stableId     <- docStore.getDocuments()
    } yield visualizeDocument(stableId)

    TB.vjoins()(docBoxes)
  }

  def visualizeDocument(stableId: String@@DocumentID): TB.Box = {
    val docBoxes = for {
      docId <- docStore.getDocument(stableId).toSeq
    } yield {
      val pagesBox = for {
        pageId <- docStore.getPages(docId)
      } yield {
        val pageGeometry = docStore.getPageGeometry(pageId)

        val allTargetRegions = docStore.getTargetRegions(pageId)

        val regions = allTargetRegions
          .map(r => docStore.getTargetRegion(r).toString().box)

        ("PageGeometry"
          % indent(4)(pageGeometry.toString.box)
          % indent(2)(s"TargetRegions for page ${pageId}: ${allTargetRegions.length} ")
          % indent(4)(vcat(regions)))
      }

      val zoneBoxes = for {
        labelId <- docStore.getZoneLabelsForDocument(docId)
        zoneId <- docStore.getZonesForDocument(docId, labelId)
      } yield {
        val zbox = docStore.getZone(zoneId).toString().box
        docStore.getTextReflowForZone(zoneId)
          .map { reflow =>
            zbox atop indent(2)(reflow.toText.box)
          }
          .getOrElse(zbox)
      }

      (s"Document ${docId} (${stableId}) report"
        % indent(2)(vcat(pagesBox))
        % indent(2)("Zones")
        % indent(4)(vcat(zoneBoxes))
      )
    }
    vcat(docBoxes)
  }

  def reportDocument(stableId: String@@DocumentID): TB.Box = {
    visualizeDocument(stableId)
  }

  //// Tests to be run across mem/db docstore
  import watrmarks.{StandardLabels => LB}

  def addSampleDocs(docs: List[List[String]]): Unit = {
    for { (doc, i) <- docs.zipWithIndex } {
      addDocument(DocumentID(s"doc#${i}"), doc)
    }
  }
  def addSampleDoc(doc: List[String]): Unit = {
    addSampleDocs(List(doc))
  }

  def test1(): Unit = {
    addSampleDoc(MockPapers.sample_4pg_3x3_doc)

    val stableId = DocumentID("doc#0")
    val docId = docStore.getDocument(stableId).get
    val pageId = docStore.getPage(docId, PageNum(0)).get
    val lineZones = docStore.getPageZones(pageId, LB.VisualLine)
    val lineRegions = lineZones.flatMap(_.regions)

    lineRegions.headOption.foreach{ r0 =>
      val zoneId = docStore.createZone(r0.id, LB.PageLines)
      lineRegions.tail.foreach{ r1 =>
        docStore.addZoneRegion(zoneId, r1.id)
      }
    }
  }



}

