package edu.umass.cs.iesl.watr
package spindex


import ammonite.{ops => fs}, fs._
import java.nio.{file => nio}
import play.api.libs.json, json._
import play.api.data.validation.ValidationError


trait PredsynthPaperAnnotationTypes extends TypeTagFormats {
  import play.api.libs.json._
  import play.api.libs.functional.syntax._

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

  implicit def formatModified: Format[Modified] =
    new Format[Modified] {
      override def reads(json: JsValue): JsResult[Modified] = json match {
        case _ => JsSuccess(Modified(0))
      }
      override def writes(o: Modified): JsValue = JsNull
    }

  implicit def Format_RawText          =  Json.format[RawText]
  implicit def Format_Apparatus        =  Json.format[Apparatus]
  implicit def Format_Condition        =  Json.format[Condition]
  implicit def Format_Amount           =  Json.format[Amount]
  implicit def Format_EntityDescriptor =  Json.format[EntityDescriptor]
  implicit def Format_Operation        =  Json.format[Operation]
  implicit def Format_Connection       =  Json.format[Connection]
  implicit def Format_Paragraph        =  Json.format[Paragraph]
  implicit def Format_Entity           =  Json.format[Entity]
  implicit def Format_Paper            =  Json.format[Paper]

}

case class Modified(n: Int)

case class Entity(
  _id : Option[String],
  is_target: Option[Boolean],
  raw_texts : Seq[Option[RawText]],
  amounts: Seq[Option[Amount]],
  entdescriptors: Seq[Option[EntityDescriptor]],
  x: Double, y: Double
)

case object Missing

case class Apparatus(raw_text : RawText)
case class Condition(raw_text : RawText)
case class Amount(raw_text : RawText)
case class EntityDescriptor(raw_text: RawText)

case class RawText(
  end_char_index : Option[Int],
  start_char_index : Option[Int],
  raw_text : String,
  paragraph_id : Option[String]
)

case class Operation(
  _id : Option[String],
  raw_texts : Seq[Option[RawText]],
  apparatuses: Seq[Option[Apparatus]],
  conditions : Seq[Option[Condition]],
  x: Double, y: Double,
  order :Int
)

case class Paper(
  doi : String,
  title : String,
  plaintext : Option[String],
  `abstract` :  Option[String],
  annotator : Option[String],
  modified : Modified,
  pdf_loc: String,
  tags : Seq[String],
  connections : Seq[Option[Connection]],
  entities: Seq[Option[Entity]],
  operations: Seq[Option[Operation]],
  paragraphs: Seq[Paragraph]
) {

  def name: String = {
    pdf_loc.split("/").last.trim()
  }
}

case class Paragraph(
  _id : String,
  text: String,
  is_recipe: Boolean
)

case class Connection(
  x1 : Double, x2 : Double,
  y1 : Double, y2 : Double,
  id1 : Option[String], id2 : Option[String],
  paper_dois : Option[Seq[String]]
)

case class TextPosition(
 postions: Seq[(Int, Int, Int)]
)

case class MappedContext(
  textContext: RawTextContext,
  position: TextPosition
)
case class RawTextContext(
  paper: Paper,
  rawText: RawText,
  paragaph: Paragraph,
  begin: Int,
  textType: String
) {
  override def toString = {
    val text = rawText.raw_text

    // println(s"  >>>for ${text}, trying para l=${paragaph.text.length()}> ${paragaph.text}")

    if (paragaph.text.isEmpty()){
      s"""${textType} (no context) [${text}] """
    } else {
      val (pre, text, post) =  stringContext(15)
      s"""${textType} :: ${pre}[${text}]${post}"""
    }
  }

  def stringContext(width: Int): (String, String, String) = {
    val text = rawText.raw_text
    val prebegin = math.max(0, begin-width)
    val preend = begin
    val postbegin = begin + text.length()
    val postend = math.min(paragaph.text.length(), postbegin+width)

    val prectx = paragaph.text.substring(prebegin, preend)
    val postctx = paragaph.text.substring(postbegin, postend)
    (prectx, text, postctx)

  }
}

object PredsynthLoad extends PredsynthPaperAnnotationTypes {

  def findMatchingParagraphs(paragraphs: Seq[Paragraph], r: RawText): Seq[(Paragraph, Int)] = {
    for {
      p <- paragraphs
      if p.text.indexOf(r.raw_text) > -1
    } yield (p, p.text.indexOf(r.raw_text))
  }


  def loadPaper(jsonPath: Path): Option[Paper] = {
    val fis = nio.Files.newInputStream(jsonPath.toNIO)
    val paper = json.Json.parse(fis).validate[Paper]

    paper.fold(
      (errors: Seq[(JsPath, Seq[ValidationError])]) => {
        println(s"errors: ${errors.length}")

        errors.take(10).foreach { case (errPath, errs) =>
          println(s"$errPath")
          errs.foreach { e =>
            println(s"> $e")
          }
        }
        None

      }, (paper: Paper) => {
        println("load went ok.")
        Option(paper)
      }
    )
  }

  def loadPapers(predsynthPath: Path): Option[Map[String, Paper]] = {
    val fis = nio.Files.newInputStream(predsynthPath.toNIO)
    val papers = json.Json.parse(fis).validate[Seq[Paper]]

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

      }, (ps: Seq[Paper]) => {
        println("load went ok.")
        // val jsonTmp = pwd / RelPath("predsynth-jsons.d")

        // rm(jsonTmp)
        // mkdir(jsonTmp)

        // ps.foreach { p =>
        //   val name = paperName(p)
        //   val asJson = Json.toJson(p)
        //   val pPath = jsonTmp / name
        //   val jstr = Json.prettyPrint(asJson)
        //   if (exists(pPath)) {
        //       val vpath = jsonTmp / (name+".v2")
        //     write(vpath, jstr)
        //   } else {
        //     write(pPath, jstr)
        //   }
        // }


        ps.map(p=> {
          (p.name -> p)
        }).toMap.some
      })

  }


  case class EntityContexts(
    ent: Seq[RawTextContext],
    amounts: Seq[RawTextContext],
    edesc: Seq[RawTextContext]
  )
  case class OpContexts(
    ops: Seq[RawTextContext],
    app: Seq[RawTextContext],
    cond: Seq[RawTextContext]
  )

  case class Contexts(
    entityContexts: Seq[EntityContexts],
    opContexts: Seq[OpContexts]
  )

  def buildContexts(paper: Paper): Contexts = {

    def findContexts(rawTexts: Seq[RawText], ct: String): Seq[RawTextContext] = for {
      rt <- rawTexts
      (p, i) <- matchRawTextToPara(rt, paper.paragraphs)
    } yield RawTextContext(paper, rt, p, i, ct)


    val ents = for {
      entity <- paper.entities.flatten
    } yield {
      val entities = findContexts(entity.raw_texts.flatten, "entity")
      val amounts = findContexts(entity.amounts.flatten.map(_.raw_text), "entity/amounts")
      val edesc = findContexts(entity.entdescriptors.flatten.map(_.raw_text), "entity/descriptor")

      EntityContexts(entities, amounts, edesc)
    }


    val ops = for {
      operation <- paper.operations.flatten
    } yield {
      val ops = findContexts(operation.raw_texts.flatten, "operation")
      val apps = findContexts(operation.apparatuses.flatten.map(_.raw_text), "operation/apparatus")
      val conds = findContexts(operation.conditions.flatten.map(_.raw_text), "operation/condition")

      OpContexts(ops, apps , conds)
    }

    Contexts(ents, ops)
  }

  def matchRawTextToPara(rawText: RawText, paragraphs: Seq[Paragraph]):  Seq[(Paragraph, Int)] = {

    val paras = paragraphs.map({ para =>
      para._id -> para
    }).toMap


    val pId = rawText.paragraph_id.getOrElse { "" }
    val start = rawText.start_char_index.getOrElse { 0 }
    val end = rawText.end_char_index.getOrElse { 0 }
    val rtext = rawText.raw_text.trim()

    if (rtext.isEmpty()) Seq() else {

      paras.get(pId) match {
        case Some(para) =>
          val tlen = para.text.length
          val exactMatch = start <= end && end <= tlen && {
            val substr = para.text.substring(start, end)
            rtext == substr
          }

          // println(s"""   trying to match '${rtext}' to '${substr}' """)

          if (exactMatch) {
            // println("    success!")
            Seq((para, start))
          } else {
            // println("    fail!")

            findMatchingParagraphs(paragraphs, rawText)
          }

        case None =>
          findMatchingParagraphs(paragraphs, rawText)
      }
    }
  }

  import scala.util.matching.Regex
  import ammonite.ops._
  import ammonite.ops.ImplicitWd._

  def runAGrep(patterns: Seq[String], text: String): Unit = {
    println(s"running agrep with ${patterns.length} patterns")

    // write text to a tmp file
    val txtPath = pwd / "agrep.txt"
    if (exists(txtPath)) rm(txtPath)

    write(txtPath, text)

    patterns.map{regex =>
      val patt = regex

      println(s"""=> ${patt}""")

      try {
        val res = %%("tre-agrep", "-k", "--show-position", // "--ignore-case",
          "-9",
          "-e", s"$patt",
          s"${txtPath}"
        )

        val out = res.out.lines
        val ranges = out.map(_.split(":", 2)(0))
        val rangeStr = ranges.mkString(",")
        if (ranges.length==1) {
          println(s"Unique match: ${rangeStr}")
        } else {
          println(s"No unique match")
          println(ranges)
        }
        println(res.err.lines.mkString("\n"))

      } catch {
        case s: ShelloutException =>
          println("error")
          println(s.result.toString())
      }

    }
  }

  def alignContexts(paper: Paper, oneLine: String): Unit = { // Seq[()]

    val paperContexts = buildContexts(paper)

    def mkSearchPatterns(contexts: Seq[RawTextContext]): Seq[String] = {
      contexts.map { textContext =>
        val (pre, text, post) = textContext.stringContext(10)
        s"$pre$text$post"
      }
    }

    val searchPatterns = paperContexts.entityContexts.map { ectx =>
      mkSearchPatterns(ectx.ent) ++
      mkSearchPatterns(ectx.amounts) ++
      mkSearchPatterns(ectx.edesc)
    }


    val opPatterns = paperContexts.opContexts.map { ectx =>
      mkSearchPatterns(ectx.ops) ++
        mkSearchPatterns(ectx.app) ++
        mkSearchPatterns(ectx.cond)
    }
    val allPatterns = searchPatterns.flatten ++ opPatterns.flatten

    runAGrep(allPatterns, oneLine)

  }



  def main(args: Array[String]) = {
    // for {
    //   paper <- loadPaper(pwd / "predsynth-jsons.d" /
    //     "101016jsolidstatesciences200901008.pdf"
    //   )
    //   context <- buildContexts(paper)
    // } {
    //   println(s"context: $context")
    // }

    val jsonPath = pwd / RelPath(args(0))

    for {
      papers <- loadPapers(jsonPath)
      (paperName, paper) <- papers
    } {
      println(s"${paper.name} =========")
      val context = buildContexts(paper)
      println(s" Entities ")
      context.entityContexts.foreach { ectx =>
        ectx.ent.foreach { rt => println(s"     ${rt}") }
        ectx.amounts.foreach { rt => println(s"         ${rt}") }
        ectx.edesc.foreach { rt => println(s"         ${rt}") }
      }
      println(s" Operations")
      context.opContexts.foreach { ectx =>
        ectx.ops.foreach { rt => println(s"     ${rt}") }
        ectx.app.foreach { rt => println(s"         ${rt}") }
        ectx.cond.foreach { rt => println(s"         ${rt}") }
      }
    }

  }
}
