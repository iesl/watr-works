package edu.umass.cs.iesl.watr
package table

import scala.util.matching.Regex

import ammonite.ops._
import pprint.PPrinter

import edu.umass.cs.iesl.watr.segment.DocumentSegmenter
import edu.umass.cs.iesl.watr.utils.EnglishDictionary
import spindex._
import ComponentTypeEnrichments._
import TypeTags._
import textflow.TextReflowRendering._

import textboxing.{TextBoxing => TB}, TB._

object WatrTable {
  import ShellCommands._

  def main(args: Array[String]): Unit = {
    replMain().run(
      "corpus" -> initCorpus()
    )
  }

  val predef =
    s"""|import edu.umass.cs.iesl.watr
        |import watr._, spindex._, ComponentRendering._
        |import table._
        |import ShellCommands._
        |implicit val pp0 = pprintComponent
        |implicit val pp1 = pprintBox
        |""".stripMargin

  val welcomeBanner = s""">> WatrTable Shell <<"""

  def replMain() = ammonite.Main(
    // storageBackend = new Storage.Folder(Defaults.ammoniteHome)
    // predef = predef,
    // defaultPredef = true,
    wd = pwd,
    welcomeBanner = Some(welcomeBanner),
    inputStream = System.in,
    outputStream  = System.out,
    errorStream = System.err,
    verboseOutput = false
  )

}


object ShellCommands {
  import ComponentRendering.VisualLine

  // implicit val ppConfig = pprint.Config(
  // width: Int = Config.defaultMaxWidth,
  // height: Int = Config.defaultLines,
  // depth: Int = 0,
  // indent: Int = Config.defaultIndent,
  // colors: Colors = pprint.Colors.BlackWhite,
  // renames: Map[String, String] = Config.defaultRenames)

  def pprintComponent: PPrinter[Component] = PPrinter({(component, config) =>
    val box = component.show
    Iterator(box.toString())
  })

  def pprintBox: PPrinter[TB.Box] = PPrinter({(box, config) =>
    Iterator(box.toString())
  })

  def initCorpus(): Corpus = {
    Corpus(pwd / "corpus-test")
  }

  val supplementalDict: Set[String] = (
    """|backscatter
       |colloidal
       |compressive
       |crystallites
       |ellipsoidal
       |Elsevier
       |in-situ
       |IPF
       |nucleation
       |mortem
       |reorientation
       |serrations
       |slurry
       |Springer
       |topologies
       |transversal
       |MATLAB
       |TEM
       |CCD
       |CENIM-CSIC
       |EBSD
       |ECAP
       |SEM
       |FEI
       |HCl
       |IMDEA
       |XRD
       |""".stripMargin.split("\n").map(_.trim).toSet
  )

  val prefixes = Seq[String](
    "mis",
    "micro",
    "nano",
    "trans",
    "uni",
    "poly",
    "post",
    "pre",
    "electro"
  )

  def wordInDictionary(word: String): Boolean = {
    val dict = EnglishDictionary.global
    def isWord(w: String):Boolean = dict.contains(w) || supplementalDict.contains(w)

    def isHyphentedWords(w: String) = w.split("-—–".toArray)
      .map(isWord(_))
      .reduce(_ && _)

    def stripPrefix(w: String): Option[String] = {
      val suffix = for {
        p <- prefixes
        if w.startsWith(p)
      } yield w.drop(p.length()).mkString
      suffix.headOption
    }

    def isPrefixedWord(w: String): Boolean = {
      stripPrefix(w).map(isWord(_)).getOrElse(false)
    }

    isWord(word) || isHyphentedWords(word) || isPrefixedWord(word)
  }



  /*

   Current problems:

   Patterns like these:
       cind @nitt.edu
       www.*.*
       Pt/PANi
       [7,17–24]
       10^{5}s^{1}
       doi: 10.1029/2005JD006318
       70^{◦}C
       Fe_{3}O_{4}@C


   - [ ] words parsed as stream of single chars (particularly on line w/one word)
        - 2 0 1 2 J D 0 1 7 45 9 . 1010022013gl058232.pdf.d <target pg:4 (l:319.97, t:560.87, w:50.71, h:7.28)
   - [ ] sup/subs are squished w/adjacent words
   - [ ] ligatures get sup/subscripted
   - [ ] join words before rendering
   - [ ] Proper names
   - [ ] technical vocab needs work
   - [ ] phrase parsing (e.g, "et al." as a single token)
   - [ ] parse footnote sup/sub markers as independent tokens
   - [ ] Reversed words across entire paper ??!!
   - [ ] Non-textline elimination
   - [ ] weight/measure/quantity parsing

   */

  val acceptableWordPatterns = Seq[Regex](
    """[A-Z]\.[A-Z]""".r,
    """\d+(\.(\d+))*""".r,
    """\d+[-—–]\d+""".r
  )

  implicit class RicherComponent(val thisComponent: Component) extends AnyVal {
    def showUnknowns(): TB.Box = {
      val nonDictWords = for {
        word <- thisComponent.words() if !wordInDictionary(word)
      } yield word

      val nonAcceptedWords = for {
        word <- nonDictWords
        if !acceptableWordPatterns.exists(re => word.matches(re.regex))
      } yield word.box

      val line = thisComponent.show
      val b0 = alignHoriz(left, 30, vjoins()(nonAcceptedWords))
      vjoin()(
        hjoin()(
          b0,
          vjoin()(line, indent(4)(thisComponent.targetRegion.prettyPrint.box))
        ),
        emptyBox(1)(1)
      )
    }

    def show(): TB.Box = {
      VisualLine.toTextReflow(thisComponent).map(t => t.toText().box).getOrElse("<could not render>".box)
    }

    def webShow(): String = {
      val text = thisComponent.show()
      import WebShow._
      import texttags._
      val imgUri = thisComponent.extractImage()

      val html =
        <.tr(
          <.td(<.a(^.href := imgUri.toString())),
          <.td(text.toString)
        )

      html.toString()
    }

    def filter(fn: (Component) => Boolean): Option[Component] = {
      if (fn(thisComponent)) {
        thisComponent.some
      } else {
        None
      }
    }


    def words(): Seq[String] = {
      def trimWord(w: String): String = {
        w.dropWhile(!_.isLetterOrDigit).reverse
          .dropWhile(!_.isLetterOrDigit).reverse
          .mkString.trim
      }
      VisualLine.toTextReflow(thisComponent)
        .map(_.toString.split(" ").map(trimWord(_)).toSeq)
        .getOrElse(Seq())
    }

    def hasUnknownWords(): Boolean= {
      thisComponent.words().exists({ word =>
        val isDictWord = wordInDictionary(word)
        def isAcceptable = acceptableWordPatterns.exists({re =>
          word.matches(re.regex)
        })

        !(isDictWord || isAcceptable)
      })
    }

    def allWordLine(): Option[Component] = {
      thisComponent.filter((c) =>
        c.words().map(wordInDictionary(_)).reduce(_ && _)
      )
    }
    def nonWordLine(): Option[Component] = {
      thisComponent.filter((c) =>
        c.words().exists(!wordInDictionary(_))
      )
    }

    def grep(re: String): Option[Component]= {
      val rendered = VisualLine.toTextReflow(thisComponent).toString

      if (rendered.matches(re)) {
        Some(thisComponent)
      } else None
    }

    import java.net.URI

    def extractImage(): URI = {
      import com.sksamuel.scrimage._

      val pageId = thisComponent.pageId.unwrap+1
      val srcUri = thisComponent.getSrcUri()

      val pageSrc = srcUri.resolve("page-images/").resolve(s"page-${pageId}.png")
      println(s"page src: ${pageSrc}")
      val image = Image.fromFile(new java.io.File(pageSrc.getPath))
      val cropped = ImageManipulation.cropTo(image, thisComponent.bounds, thisComponent.getPageGeometry)

      val x = thisComponent.bounds.left.toInt
      val y = thisComponent.bounds.top.toInt
      val w = cropped.width
      val h = cropped.height

      val imgDst = srcUri.resolve("page-images/").resolve(s"page-${pageId}-x$x-y$y-w$w-h$h.png")
      println(s"page dest: ${imgDst}")
      cropped.output(new java.io.File(imgDst.getPath))

      imgDst
    }
  }



  implicit class RicherDocumentSegmenter(val thisDocumentSegmenter: DocumentSegmenter) extends AnyVal {

    def lines(): Seq[Component] = {
      ???
    }
  }

  implicit class RicherCorpusEntry(val thisCorpusEntry: CorpusEntry) extends AnyVal {

    def textBlocks(): Unit = {

    }

    def paragraphs(): Unit = {

    }

    def lines(): Seq[Component]= {
      val lls = for {
        segmenter <- thisCorpusEntry.segment()
      } yield {
        segmenter.lines()
      }
      lls.getOrElse(Seq())
    }

    def linesw(): Seq[String]= {
      val lls = for {
        segmenter <- thisCorpusEntry.segment()
      } yield {
        segmenter.lines()
      }

      lls.map(_.map(_.webShow())).getOrElse(Seq())
    }


    def segment(): Option[DocumentSegmenter] = {

      val rootDirectory = thisCorpusEntry.corpus.corpusRoot
      val entryDescriptor = thisCorpusEntry.entryDescriptor

      val conf = extract.AppConfig(
        corpusRoot = rootDirectory.toIO.some,
        inputEntryDescriptor = entryDescriptor.some,
        action = "docseg".some,
        force = true
      )

      extract.Works.segmentDocument(conf)
    }

  }


  implicit class RicherCorpus(val thisCorpus: Corpus) extends AnyVal {

    def chooseEntries(n: Int = 0, skip: Int = 0): Seq[CorpusEntry] = {
      val allEntries = thisCorpus.entries()
      val skipped = if (skip > 0) allEntries.drop(skip) else allEntries
      val entries = if (n > 0) skipped.take(n) else skipped
      entries
    }

    def formatLineComponent(entry: CorpusEntry, c: Component): TB.Box = {
      c.showUnknowns beside indent(8)(entry.entryDescriptor.box)
    }

    def sketchyLines(n: Int = 0, skip: Int = 0): Seq[Component] = {
      val lls = for (entry <- chooseEntries(n, skip)) yield {
        entry.lines.filter(_.hasUnknownWords())
      }
      lls.flatten
    }

    def showSketchyLines(n: Int = 0, skip: Int = 0): Seq[Box] = {
      val lls = for (entry <- chooseEntries(n, skip)) yield {
        entry.lines
          .filter(_.hasUnknownWords())
          .map(formatLineComponent(entry, _))
      }
      lls.flatten
    }
  }
}


object WebShow extends ScalatagsDefs
