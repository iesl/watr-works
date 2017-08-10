package edu.umass.cs.iesl.watr
package predsynth
package ioformats

import ammonite.{ops => fs}, fs._
import java.nio.{file => nio}
import play.api.libs.json, json._
import play.api.data.validation.ValidationError

import spindex._
import Prop._

trait PredsynthJsonFormats  {

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

  implicit def Format_Modified: Format[Modified] =
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
  paragaphIndex: Int,
  begins: List[Int],
  textType: String
) {

  lazy val asString = {
    val text = rawText.raw_text

    if (paragaph.text.isEmpty()){
      s"""${textType} (no context) [${text}] """
    } else {
      val contexts = stringContexts(15)
      if (contexts.isEmpty) {
        s"""${textType} (no context) [${text}] """
      } else if (contexts.length==1) {
        val (pre, text, post) = contexts(0)
        s"""${textType} :: ${pre}[${text}]${post}"""
      } else {
        val (pre, text, post) = contexts(0)
        s"""(multiple) ${textType} :: ${pre}[${text}]${post}"""
      }
    }
  }

  override def toString(): String = asString

  def stringContexts(width: Int): List[(String, String, String)] = {
    begins.map({begin =>
      val text = rawText.raw_text
      val prebegin = math.max(0, begin-width)
      val preend = begin
      val postbegin = begin + text.length()
      val postend = math.min(paragaph.text.length(), postbegin+width)

      val prectx = paragaph.text.substring(prebegin, preend)
      val postctx = paragaph.text.substring(postbegin, postend)
      (prectx, text, postctx)
    })
  }
}

case class TextMentionGroup(
  groupNumber: Int,
  id: Option[String],
  rawTextContexts: Seq[RawTextContext],
  props: Seq[PropKV]
)

case class Contexts(
  groups: Seq[TextMentionGroup],
  props: Seq[PropKV]
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


object PredsynthLoad extends PredsynthJsonFormats {
  private[this] val log = org.log4s.getLogger

  def allIndices(instr: String, query: String, ioffset: Int=0): List[Int] = {
    val i = instr.indexOf(query)
    if (i > -1) {
      val (_, head) = instr.splitAt(i)
      val (_, tail) = head.splitAt(query.length())
      i :: allIndices(tail, query, ioffset+i+query.length())
    } else {
      Nil
    }
  }

  def findMatchingParagraphs(paragraphs: Seq[Paragraph], r: RawText): Seq[(Int, List[Int])] = {
    for {
      (p, paraIndex) <- paragraphs.zipWithIndex
      indices         = allIndices(p.text, r.raw_text)
      if indices.nonEmpty
    } yield {
      (paraIndex, indices)
    }
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
    val paperParagraphs = paper.paragraphs

    def findContexts(rawTexts: Seq[RawText], contextType: String): Seq[RawTextContext] =  {
      log.debug(s"finding paragraph locations+offsets for mention type $contextType")
      for {
        rt <- rawTexts
        (paraIndex, mentionOffsets) <- matchRawTextToPara(rt, paper.paragraphs)
      } yield RawTextContext(paper, rt, paperParagraphs(paraIndex), paraIndex, mentionOffsets, contextType)
    }

    def filterToOverlappingParas(rawTextContexts: Seq[RawTextContext]*): Seq[Seq[RawTextContext]] = {
      val nonEmptyParas = for {
        c <- rawTextContexts
        pis = c.map(_.paragaphIndex).toSet
        _ = log.debug(s"""  paraIndexes: ${pis}""")
        if pis.nonEmpty
      } yield pis

      if (nonEmptyParas.isEmpty){
        rawTextContexts
      } else {
        val intersection = nonEmptyParas.reduce(_ intersect _)
        rawTextContexts.map(_.filter(c => {
          val x = intersection.contains(c.paragaphIndex)
          intersection.contains(c.paragaphIndex)
        }))
      }
    }

    val ents = for {
      (entity, groupNum) <- paper.entities.flatten.zipWithIndex
    } yield {
      // Group these and record the sha1-id:
      val id = entity._id

      val entities = findContexts(entity.raw_texts.flatten, "entity")
      val amounts = findContexts(entity.amounts.flatten.map(_.raw_text), "entity/amount")
      val edesc = findContexts(entity.entdescriptors.flatten.map(_.raw_text), "entity/descriptor")

      val allMentions = filterToOverlappingParas(entities, amounts, edesc).flatten
      log.debug(s"filtered all but ${allMentions.length} mentions")

      val relations = Seq(
        PropKV(
          "isTarget",
          Value(JsBoolean(entity.is_target.exists(t=>t)))
        )
      )

      TextMentionGroup(
        groupNum+1,
        entity._id,
        allMentions,
        relations
      )
    }


    val ops = for {
      (operation, index) <- paper.operations.flatten.zipWithIndex
    } yield {
      val groupNum = ents.length+index+1

      val ops = findContexts(operation.raw_texts.flatten, "operation")
      val apps = findContexts(operation.apparatuses.flatten.map(_.raw_text), "operation/apparatus")
      val conds = findContexts(operation.conditions.flatten.map(_.raw_text), "operation/condition")

      val allMentions = filterToOverlappingParas(ops, apps, conds).flatten
      log.debug(s"filtered all but ${allMentions.length} operation mentions")

      val props = Seq(
        PropKV(
          "hasOrder",
          Value(JsNumber(operation.order))
        )
      )

      TextMentionGroup(
        groupNum,
        operation._id,
        allMentions,
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

  def matchRawTextToPara(rawText: RawText, paragraphs: Seq[Paragraph]):  Seq[(Int, List[Int])] = {

    val paras = paragraphs.zipWithIndex
      .map(pi => (pi._1._id, pi))
      .toMap


    val pId = rawText.paragraph_id.getOrElse { "" }
    val start = rawText.start_char_index.getOrElse { 0 }
    val end = rawText.end_char_index.getOrElse { 0 }
    val rtext = rawText.raw_text.trim()

    if (rtext.isEmpty()) {
      log.warn(s"Empty raw text context ${rawText}")
    }

    if (rtext.isEmpty()) Seq() else {

      paras.get(pId) match {
        case Some((para, paraIndex)) =>
          val tlen = para.text.length
          val exactMatch = start <= end && end <= tlen && {
            val substr = para.text.substring(start, end)
            rtext == substr
          }

          if (exactMatch) {
            log.debug(s"matched raw text to paragraph ${paraIndex} by exact string: $rtext")
            Seq((paraIndex, List(start)))
          } else {
            val m = findMatchingParagraphs(paragraphs, rawText)
            val mentionCount = m.map(_._2.length).mkString(",")
            val paraIndexes = m.map(_._1).mkString(",")
            log.debug(s"fuzzy matched raw text to ${m.length} paragraph(s) (${paraIndexes}) w/mention counts [$mentionCount]: $rtext")
            m
          }

        case None =>
          val m = findMatchingParagraphs(paragraphs, rawText)
          val mentionCount = m.map(_._2.length).mkString(",")
          val paraIndexes = m.map(_._1).mkString(",")
          log.debug(s"fuzzy matched raw text to ${m.length} paragraph(s) (${paraIndexes}) w/mention counts [$mentionCount]: $rtext")
          m
      }
    }
  }

  import ammonite.ops._
  import ammonite.ops.ImplicitWd._

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
        Left(s"No unique match for pattern '${pattern}'")
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

  def writeAgrepTmpfile(text: String): Path = {
    // write text to a tmp file
    val txtPath = pwd / "agrep.tmp"
    if (exists(txtPath)) rm(txtPath)
    write(txtPath, text)
    txtPath
  }

  def findPatternContext(pattern: (String, String, String), txtPath: Path, text:String): Either[String, (Int, Int)] = {
    val (strPre, str, strPost) = pattern
    val prePatt = s"$strPre$str"
    val postPatt = s"$str$strPost"
    val patts = s"pre: $prePatt / post: $postPatt"

    log.debug(s"grepping for ${patts}")

    val resBegin = runAGrep(postPatt, txtPath)
    val resEnd = runAGrep(prePatt, txtPath)


    (resBegin,  resEnd) match {
      case (Right(rbegin), Right(rend)) =>
        val charBegin = byteOffsetToCharOffset(text, rbegin._1)
        var charEnd = byteOffsetToCharOffset(text, rend._2)

        val beginEnd = s"($charBegin, $charEnd)"
        if ( charEnd <= charBegin || charEnd - charBegin > str.length()*2) {
          // found context is way too long, try again...
          charEnd = charBegin + str.length()
          val beginEndAdj = s"($charBegin, $charEnd)"

          log.debug(s"   adjusting match (begin,end) $beginEnd to $beginEndAdj")
        }

        log.debug(s"    matched '${text.slice(charBegin, charEnd)}'")

        Right((charBegin, charEnd))

      case _ =>
        log.debug(s"   no match")
        Left(s"Could not match ${patts} ")

    }
  }

  def alignContexts(paper: Paper, oneLine: String): Seq[AlignedGroup] = {

    def mkSearchPatterns(contexts: Seq[RawTextContext]): Seq[(String, String, String)] = {
      for {
        textContext <- contexts
        tc <- textContext.stringContexts(15)
      } yield tc
    }

    val paperContexts = buildContexts(paper)

    log.debug(s"Aligning contexts")
    val agrepTextFile = writeAgrepTmpfile(oneLine)

    for {
      textMentionGroup <- paperContexts.groups
    } yield {
      val groupType = textMentionGroup
        .rawTextContexts.headOption
        .map(_.textType.takeWhile(_ != '/'))
        .getOrElse("")

      val alignments = for {
        context <- textMentionGroup.rawTextContexts
        pattern <- context.stringContexts(10)
      } yield {
        log.debug(s"""aligning context {${context}} using pattern [${pattern}]""")
        val foundContext = findPatternContext(pattern, agrepTextFile, oneLine)

        foundContext.fold(
          msg   => AlignFailure(context, msg),
          range => AlignSuccess(context, range)
        )
      }
      AlignedGroup(
        textMentionGroup,
        groupType,
        alignments
      )
    }

  }

}
