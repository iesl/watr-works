package edu.umass.cs.iesl.watr
package bioarxiv

import ammonite.{ops => fs}, fs._
import corpora.filesys._

import _root_.io.circe, circe._, circe.syntax._
import circe.generic.semiauto._
import circe.{parser => CirceParser}

import utils.DoOrDieHandlers._

import sys.process._

object BioArxiv {

  case class PaperRec(
    doi_link: String,
    pdf_link: String,
    pmid: Option[Long],
    sourceJson: Option[Json]
  )

}

trait BioArxivJsonFormats  {
  import BioArxiv._

  // implicit def optionalFormat[T](implicit jsFmt: Format[T]): Format[Option[T]] =
  //   new Format[Option[T]] {
  //     override def reads(json: JsValue): JsResult[Option[T]] = json match {
  //       case JsNull => JsSuccess(None)
  //       case js     => jsFmt.reads(js).map(Some(_))
  //     }
  //     override def writes(o: Option[T]): JsValue = o match {
  //       case None    => JsNull
  //       case Some(t) => jsFmt.writes(t)
  //     }
  //   }

  implicit def Encode_PaperRec: Encoder[PaperRec] =  deriveEncoder
  implicit def Decode_PaperRec: Decoder[PaperRec] =  deriveDecoder


}


object BioArxivOps extends BioArxivJsonFormats {
  private[this] val log = org.log4s.getLogger
  import BioArxiv._


  def getBioarxivJsonArtifact(corpusEntry: CorpusEntry): Option[PaperRec] = {
    for {
      rec      <- corpusEntry.getArtifact("bioarxiv.json")
      asJson   <- rec.asJson.toOption
    } yield { asJson.decodeOrDie[PaperRec]() }
  }

  def loadPaperRecs(path: Path): Map[String, PaperRec] = {
    // val fis = nio.Files.newInputStream(path.toNIO)
    val jsonStr = _root_.scala.io.Source.fromFile(path.toIO).mkString

    val papers = CirceParser.parse(jsonStr) match {
      case Left(failure) => die(s"Invalid JSON : ${failure}")
      case Right(json) =>
        json.hcursor.values.map { jsons =>
          jsons.map { jsonRec =>
            // println(s"decoding ${jsonRec.spaces2}")
            val paperRec = jsonRec.decodeOrDie[PaperRec]()
            paperRec.copy(sourceJson = Some(jsonRec))
          }
        }.orDie()
    }

    println("bioarxiv json load successful.")

    papers.map{  p=>
      val pathParts = p.doi_link.split("/")
      val key = pathParts.takeRight(2).mkString("-") + ".d"
      (key, p)
    }.toMap
  }

  def createCorpus(corpusRoot: Path, paperRecs: Map[String, PaperRec]): Unit = {
    val corpus = Corpus(corpusRoot)
    corpus.touchSentinel

    for {
      (key, rec) <- paperRecs
    } {
      val entry =  corpus.ensureEntry(key)
      val pjson = rec.sourceJson.orDie("no source json found")
      val jsOut = pjson.spaces2
      val artifact = entry.putArtifact("bioarxiv.json", jsOut)
      val path = artifact.rootPath
      log.info(s"entry $key created in $path")
    }
  }


  def downloadPdfs(corpusRoot: Path): Unit = {
    val corpus = Corpus(corpusRoot)
    println(s"downloading pdf from ${corpus}")
    for {
      entry  <- corpus.entries()
      json   <- entry.getArtifact("bioarxiv.json")
      asJson <- json.asJson
    } {
      val paper = asJson.decodeOrDie[PaperRec]()
      val link = paper.pdf_link
      val pdfName = link.split("/").last

      if (!entry.hasArtifact(pdfName)) {
        try {

          println(s"downloading ${link}")
          val downloadPath =  (entry.artifactsRoot / s"${pdfName}").toNIO.toFile

          val cmd = Seq("curl", "-L", link, "--output", downloadPath.toString())

          val status = cmd.!

          println(s"code ${status} for ${link}")

          // val asdf = new URL(link) #> downloadPath !!

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

object BioArxivCLI extends App with utils.AppMainBasics {
  import BioArxivOps._
  import utils.PathUtils._

  def run(args: Array[String]): Unit = {
    val argMap = argsToMap(args)

    // val json = argMap.get("json").flatMap(_.headOption)
    //   .getOrElse(sys.error("no bioarxiv json file supplied (--json ...)"))

    val corpusRoot = argMap.get("corpus").flatMap(_.headOption)
      .getOrElse(sys.error("no corpus root  (--corpus ...)"))

    // val paperRecs = loadPaperRecs(json.toPath)
    // val withPmids = paperRecs.filter { case (_, rec) => rec.pmid.isDefined }

    // createCorpus(corpusRoot.toPath, withPmids)

    downloadPdfs(corpusRoot.toPath())


  }

  run(args)
}


