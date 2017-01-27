package edu.umass.cs.iesl.watr
package table

import ammonite.{ops => fs}, fs._
import java.nio.{file => nio}
import play.api.libs.json, json._
import play.api.data.validation.ValidationError

import corpora._

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
          val key = pathParts.takeRight(2).mkString("-")
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



  // download pdfs..
  // 

  // create corpus structure
  // store per-corpusentry json record
  // download pdfs
  // run text alignment code over first page extractions
  // optimistically create bounding boxes
  // display in watrcolors
  // {
  //      "abstract": "In the pea beetle egg (Callosobruchus maculatus), segment sequence can be reversed in the posterior half of the egg by temporary constriction in the anterior half (van der Meer 1984). This replaces anterior segments with posterior se
  //      "authors": [
   //          "Jitse Michiel van der Meer"
   //      ],
   //      "doi_link": "https://doi.org/10.1101/059162",
   //      "pdf_link": "http://biorxiv.org/content/early/2016/06/15/059162.full.pdf",
   //      "title": "RNase reverses segment sequence in the anterior of a beetle egg (Callosobruchus maculatus, Coleoptera)"
   //  },(2) JSON structure for matched bioRxiv papers:.

// {
//     'title': String,
//     'abstract': String,
//     'doi_link': String,
//     'pdf_link': String,
//     'authors': List[String],
//     'pmid': Integer,
//     'affiliations': [
//           {
//               'affiliations': [
//                   {
//                       'affiliation': String
//                   },
//               ],
//               'first_name': String,
//               'last_name': String,
//               'initials': String
//           }
//     ]
//    'references': [
//           {
//               'pmid': Integer,
//               'title': String,
//               'journal': String,
//               'authors': [
//                   {
//                       'affiliations': [
//                           {
//                               'affiliation': String
//                           },
//                       ],
//                       'first_name': String,
//                       'last_name': String,
//                       'initials': String
//                   }
//               ]
//           }
//     ]
//    'journal': String
// }


}
