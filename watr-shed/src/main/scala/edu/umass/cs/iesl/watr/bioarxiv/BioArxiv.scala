package edu.umass.cs.iesl.watr
package bioarxiv

import ammonite.{ops => fs}, fs._
import java.nio.{file => nio}
import play.api.libs.json, json._
import play.api.data.validation.ValidationError
import watrmarks.{StandardLabels => LB}

import corpora._

import scala.collection.mutable
import textreflow.data._
import TypeTags._
import watrmarks._

object BioArxiv {

  case class PaperRec(
    title: String,
    `abstract`: String,
    doi_link: String,
    pdf_link: String,
    authors: List[String]
  )

}

trait BioArxivJsonFormats  {
  import BioArxiv._

  implicit def optionalFormat[T](implicit jsFmt: Format[T]): Format[Option[T]] =
    new Format[Option[T]] {
      override def reads(json: JsValue): JsResult[Option[T]] = json match {
        case JsNull => JsSuccess(None)
        case js     => jsFmt.reads(js).map(Some(_))
      }
      override def writes(o: Option[T]): JsValue = o match {
        case None    => JsNull
        case Some(t) => jsFmt.writes(t)
      }
    }

  implicit def Format_PaperRec            =  Json.format[PaperRec]

}



object BioArxivOps extends BioArxivJsonFormats {
  import BioArxiv._

  def getBioarxivJsonArtifact(corpusEntry: CorpusEntry): Option[PaperRec] = {
    for {
      rec      <- corpusEntry.getArtifact("bioarxiv.json")
      asJson   <- rec.asJson.toOption
      paperRec <- asJson.validate[PaperRec].fold(
        (errors: Seq[(JsPath, Seq[ValidationError])]) => {
          println(s"errors: ${errors.length}")

          errors.take(10).foreach { case (errPath, errs) =>
            println(s"$errPath")
            errs.foreach { e =>
              println(s"> $e")
            }
          }
          None

        }, ps => Option(ps))
    } yield  paperRec
  }


  def loadPaperRecs(path: Path): Option[Map[String, PaperRec]] = {
    val fis = nio.Files.newInputStream(path.toNIO)
    val papers = json.Json.parse(fis).validate[Seq[PaperRec]]

    papers.fold(
      (errors: Seq[(JsPath, Seq[ValidationError])]) => {
        println(s"errors: ${errors.length}")

        errors.take(10).foreach { case (errPath, errs) =>
          println(s"$errPath")
          errs.foreach { e =>
            println(s"> $e")
          }
        }
        None

      }, (ps: Seq[PaperRec]) => {
        println("predsynth json load successful.")

        ps.map(p=> {
          val pathParts = p.doi_link.split("/")
          val key = pathParts.takeRight(2).mkString("-") + ".d"
          (key -> p)
        }).toMap.some
      })
  }

  def createCorpus(corpusRoot: Path, paperRecs: Map[String, PaperRec]): Unit = {
    val corpus = Corpus(corpusRoot)
    corpus.touchSentinel

    for {
      (key, rec) <- paperRecs
    } {
      val entry =  corpus.ensureEntry(key)
      val pjson = Json.toJson(rec)
      val jsOut = Json.prettyPrint(pjson)
      val artifact = entry.putArtifact("bioarxiv.json", jsOut)
    }
  }



  import sys.process._
  import java.net.URL

  def downloadPdfs(corpusRoot: Path): Unit = {
    val corpus = Corpus(corpusRoot)
    println(s"downloading pdf from ${corpus}")
    for {
      entry  <- corpus.entries().take(1000)
      json   <- entry.getArtifact("bioarxiv.json")
      asJson <- json.asJson
      paper  <- asJson.validate[PaperRec]
    } {
      val link = paper.pdf_link
      val pdfName = link.split("/").last

      if (!entry.hasArtifact(pdfName)) {
        try {

          println(s"downloading ${link}")
          val downloadPath =  (entry.artifactsRoot / s"${pdfName}").toNIO.toFile

          new URL(link) #> downloadPath !!

        } catch {
          case t: Throwable =>
            println(s"Error: ${t}")
        }

        println(s"  ...done ")
      } else {
        println(s"already have ${link}")

      }

    }

  }
}



object AlignBioArxiv {
  import BioArxiv._
  private[this] val log = org.log4s.getLogger

  case class AlignmentScores(
    alignmentLabel: Label
  ) {
    val lineScores = mutable.HashMap[Int, Double]()
    val triScores  = mutable.HashMap[Int, Double]()

    val lineReflows = mutable.HashMap[Int, TextReflow]()
    val triReflows = mutable.HashMap[Int, TextReflow]()

    val consecutiveTriBoosts  = mutable.HashMap[Int, Int]()

    def boostTrigram(lineInfo: ReflowSliceInfo, triInfo: ReflowSliceInfo): Unit = {
      val ReflowSliceInfo(linenum, lineReflow, lineText) = lineInfo
      val ReflowSliceInfo(trinum, triReflow, triText) = triInfo
      val triBoostRun = consecutiveTriBoosts.getOrElseUpdate(trinum-1, 0)+1
      consecutiveTriBoosts.put(trinum, triBoostRun)

      val lineScore = lineScores.getOrElse(linenum, 1d)
      val triScore = triScores.getOrElse(trinum, 1d)

      lineScores.put(linenum, lineScore + triBoostRun)
      triScores.put(trinum, triScore + triBoostRun)

      lineReflows.getOrElseUpdate(linenum, lineReflow)
      triReflows.getOrElseUpdate(trinum, triReflow)
    }


    def alignStringToPage(str: String, pageTrigrams: Seq[(ReflowSliceInfo, ReflowSliceInfo)]): Unit = {
      println(s"aligning ${str}")
      // // Init lineScores/lineReflows
      // for ((ReflowSliceInfo(linenum, lineReflow, lineText), _) <- pageTrigrams) {
      //   lineScores.put(linenum, 0d)
      //   lineReflows.put(linenum, lineReflow)
      // }
      for {
        tri <- makeTrigrams(str)
        (lineInfo@ReflowSliceInfo(linenum, lineReflow, lineText), triInfo@ReflowSliceInfo(trinum, triReflow, triText)) <- pageTrigrams
      } {
        if (tri == triText) {
          boostTrigram(lineInfo, triInfo)
        }
      }
    }

    def report(lineText: Seq[String]): Unit = {
      println(s"Top candidates:")
      for {
        (k, v) <- lineScores.toList.sortBy(_._2).reverse.take(10)
      } {
        val text = lineText(k)
        println(s"score ${v}; ln ${k}>  $text")
      }
    }
  }

  def makeTrigrams(str: String): Seq[String] = {
    str.sliding(3).toList
  }

  case class ReflowSliceInfo(
    index: Int,
    reflow: TextReflow,
    text: String
  )


  def alignPaperWithDB(docStore: DocumentCorpus, paper: PaperRec, stableId: String@@DocumentID): List[AlignmentScores] = {

    log.debug("aligning bioarxiv paper")

    val titleBoosts = new AlignmentScores(LB.Title)
    val authorBoosts = new AlignmentScores(LB.Authors)
    val abstractBoosts = new AlignmentScores(LB.Abstract)


    val page0 = PageNum(0)

    val lineReflows = for {
      (vlineZone, linenum) <- docStore.getPageVisualLines(stableId, page0).zipWithIndex
    } yield {
      println(s"${vlineZone}")

      val vlineReflow = docStore.getTextReflowForZone(vlineZone.id)
      val reflow = vlineReflow.getOrElse { sys.error(s"no text reflow found for line ${linenum}") }
      val text = reflow.toText
      println(s"  >>${text}<<")
      (linenum, reflow, text)
    }

    val lineTrisAndText = for {
      (linenum, vlineReflow, lineText) <- lineReflows
      // _            = println(s"${linenum}> ${lineText}")
      lineInfo = ReflowSliceInfo(linenum, vlineReflow, vlineReflow.toText())
    } yield for {
      i <- 0 until vlineReflow.length
      (slice, sliceIndex)       <- vlineReflow.slice(i, i+3).zipWithIndex
    } yield {
      val triInfo = ReflowSliceInfo(sliceIndex, slice, slice.toText())
      (lineInfo, triInfo)
    }

    val page0Trigrams = lineTrisAndText.flatten.toList

    titleBoosts.alignStringToPage(paper.title, page0Trigrams)
    abstractBoosts.alignStringToPage(paper.`abstract`, page0Trigrams)

    paper.authors.map(author =>
      authorBoosts.alignStringToPage(author, page0Trigrams)
    )

    println(s"Actual title> ${paper.title}")
    titleBoosts.report(lineReflows.map(_._3))

    println(s"""Actual Authors> ${paper.authors.mkString(", ")}""")
    authorBoosts.report(lineReflows.map(_._3))

    println("Abstract lines")
    println(s"""Actual Abstract> ${paper.`abstract`.substring(0, 20)}...""")
    abstractBoosts.report(lineReflows.map(_._3))

    List(
      titleBoosts,
      authorBoosts,
      abstractBoosts
    )

  }



}
