package edu.umass.cs.iesl.watr
package predsynth

import ammonite.{ops => fs}, fs._
import java.nio.{file => nio}
import play.api.libs.json, json._
import play.api.data.validation.ValidationError
import TypeTags._
import scalaz.@@


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

case class TextMentionGroup(
  groupNumber: Int,
  id: Option[String],
  rawTextContexts: Seq[RawTextContext],
  props: Seq[Relation.PropKV]

)

case class Contexts(
  groups: Seq[TextMentionGroup],
  props: Seq[Relation.PropKV]
)

sealed trait AlignedContext
case class AlignSuccess(
  rawTextContext: RawTextContext,
  range: (Int, Int)

) extends AlignedContext

case class AlignFailure(
  rawTextContext: RawTextContext,
  msg: String
) extends AlignedContext


case class AlignedGroup(
  textMentionGroup: TextMentionGroup,
  groupType: String,
  alignedContexts: Seq[AlignedContext]
)


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
        println("predsynth json load successful.")
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
        println("predsynth json load successful.")

        ps.map(p=> {
          (p.name -> p)
        }).toMap.some
      })

  }



  def buildContexts(paper: Paper): Contexts = {

    def findContexts(rawTexts: Seq[RawText], ct: String): Seq[RawTextContext] = for {
      rt <- rawTexts
      (p, i) <- matchRawTextToPara(rt, paper.paragraphs)
    } yield RawTextContext(paper, rt, p, i, ct)


    val ents = for {
      (entity, groupNum) <- paper.entities.flatten.zipWithIndex
    } yield {
      // Group these and record the sha1-id:
      val id = entity._id
      val entities = findContexts(entity.raw_texts.flatten, "entity")
      val amounts = findContexts(entity.amounts.flatten.map(_.raw_text), "entity/amount")
      val edesc = findContexts(entity.entdescriptors.flatten.map(_.raw_text), "entity/descriptor")

      val relations = Seq(
        Relation.PropKV(
          "isTarget",
          Relation.Prop.Bool(entity.is_target.exists(t=>t))
        )
      )

      // EntityContexts(entities, amounts, edesc)
      TextMentionGroup(
        groupNum,
        entity._id,
        entities ++ amounts ++ edesc,
        relations
      )

    }


    val ops = for {
      (operation, index) <- paper.operations.flatten.zipWithIndex
    } yield {
      val groupNum = ents.length+index

      val ops = findContexts(operation.raw_texts.flatten, "operation")
      val apps = findContexts(operation.apparatuses.flatten.map(_.raw_text), "operation/apparatus")
      val conds = findContexts(operation.conditions.flatten.map(_.raw_text), "operation/condition")

      val props = Seq(
        Relation.PropKV(
          "hasOrder",
          Relation.Prop.Num(operation.order)
        )
      )

      TextMentionGroup(
        groupNum,
        operation._id,
        ops ++ apps ++ conds,
        props
      )
    }

    // TODO: paragraph: isRecipe
    // TODO: paper: hasTag
    // paper.paragraphs

    Contexts(
      ents++ops,
      Seq()
    )
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

          if (exactMatch) {
            Seq((para, start))
          } else {

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
  import scala.collection.mutable

  def runAGrep(pattern: String, filePath: Path): Either[String, (Int, Int)] = {
    try {
      val res = %%("tre-agrep", "-k", "--show-position",
        "--no-filename",
        "--substitute-cost", "3",
        "--insert-cost", "1",
        "--delete-cost", "1",
        "-4",
        "-e", pattern,
        s"${filePath}"
      )


      val ranges = res.out.lines.map(_.split(":", 2)(0))
      if (ranges.length==1) {
        val Array(begin, end) = ranges(0).trim.split("-")
        val range = (begin.toInt, end.toInt)
        Right(range)
      } else {
        Left("No unique match")
      }
    } catch {
      case s: ShelloutException =>
        Left(s"Exit code ${s.result.toString()}")
    }

  }


  def byteOffsetToCharOffset(instr: String, boff: Int): Int = {
    var coff = 0
    var bcurr = 0
    val charsAndWidths = instr.map(c => (c, c.toString().getBytes.length))

    charsAndWidths.dropWhile {case (c:Char, w:Int) =>
      if (bcurr < boff) {
        bcurr += w
        coff += 1
        true
      } else false
    }
    coff
  }

  def strByteLen(s: String): Int = {
    s.getBytes.length
  }

  def findPatternContexts(patterns: Seq[(String, String, String)], text: String): Seq[Either[String, (Int, Int)]] = {
    println(s"running agrep with ${patterns.length} patterns")

    // write text to a tmp file
    val txtPath = pwd / "agrep.tmp"
    if (exists(txtPath)) rm(txtPath)

    write(txtPath, text)

    patterns.map{ case (strPre, str, strPost) =>

      // val res = runAGrep(s"$strPre$str$strPost", txtPath)

      // res match {
      //   case Right((bOffBegin, bOffEnd)) =>

      //     val strPreByteLen = strByteLen(strPre)
      //     val strMidByteLen = strByteLen(str)

      //     val midStrBegin = bOffBegin + strPreByteLen
      //     val midStrEnd = midStrBegin + strMidByteLen

      //     val charBegin = byteOffsetToCharOffset(text, midStrBegin)
      //     val charEnd = byteOffsetToCharOffset(text, midStrEnd)

      //     val inContext = s"""${strPre} [${str}] ${strPost}"""
      //     println(s"""findPatternContexts: ${inContext}  """)
      //     println(s"    ==> text.slice($charBegin, $charEnd): '${text.slice(charBegin, charEnd)}'          ${text.slice(charBegin-10, charEnd+10)}")

      //     Right((charBegin, charEnd))

      //   case _ =>
      //     Left("Could not match ")
      // }

      val resBegin = runAGrep(s"$str$strPost", txtPath)
      val resEnd = runAGrep(s"$strPre$str", txtPath)


      (resBegin,  resEnd) match {
        case (Right(rbegin), Right(rend)) =>
          val charBegin = byteOffsetToCharOffset(text, rbegin._1)
          var charEnd = byteOffsetToCharOffset(text, rend._2)

          if ( charEnd <= charBegin || charEnd - charBegin > str.length()*2) {
            // found context is way too long, try again...
            charEnd = charBegin + str.length()
          }

          // val inContext = s"""${strPre} [${str}] ${strPost}"""
          // println(s"""findPatternContexts:    ${inContext}  byte ranges = ${rbegin} -> ${rend} """)
          // println(s"    ==> text.slice($charBegin, $charEnd):    '${text.slice(charBegin, charEnd)}'")

          Right((charBegin, charEnd))

        case _ =>
          Left("Could not match ")
      }
    }
  }


  def alignContexts(paper: Paper, oneLine: String): Seq[AlignedGroup] = {

    val paperContexts = buildContexts(paper)

    def mkSearchPatterns(contexts: Seq[RawTextContext]): Seq[(String, String, String)] = {
      contexts.map { textContext =>
        textContext.stringContext(15)
      }
    }

    paperContexts.groups.map { textMentionGroup: TextMentionGroup =>

      val allContexts = textMentionGroup.rawTextContexts
      val allPatterns = allContexts.map(_.stringContext(10))

      val groupType = allContexts.headOption
        .map(_.textType.takeWhile(_ != '/'))
        .getOrElse("")

      val foundContexts = findPatternContexts(allPatterns, oneLine)

      val alignments = foundContexts.zip(allContexts)
        .map({ case (foundCtx, context) =>
          foundCtx.fold(
            msg => AlignFailure(context, msg),
            range => AlignSuccess(context, range)
          )
        })

      AlignedGroup(
        textMentionGroup,
        groupType,
        alignments
      )
    }
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

    // val jsonPath = pwd / RelPath(args(0))

    // for {
    //   papers <- loadPapers(jsonPath)
    //   (paperName, paper) <- papers
    // } {
    //   println(s"${paper.name} =========")
    //   val context = buildContexts(paper)
    //   println(s" Entities ")
    //   context.entityContexts.foreach { ectx =>
    //     ectx.ent.foreach { rt => println(s"     ${rt}") }
    //     ectx.amounts.foreach { rt => println(s"         ${rt}") }
    //     ectx.edesc.foreach { rt => println(s"         ${rt}") }
    //   }
    //   println(s" Operations")
    //   context.opContexts.foreach { ectx =>
    //     ectx.ops.foreach { rt => println(s"     ${rt}") }
    //     ectx.app.foreach { rt => println(s"         ${rt}") }
    //     ectx.cond.foreach { rt => println(s"         ${rt}") }
    //   }
    // }
  }

}
