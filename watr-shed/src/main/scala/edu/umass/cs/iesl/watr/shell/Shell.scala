package edu.umass.cs.iesl.watr
package shell

import ammonite.ops._
import edu.umass.cs.iesl.watr.segment.DocumentSegmenter
import edu.umass.cs.iesl.watr.spindex.{ Component, ComponentRendering }
import edu.umass.cs.iesl.watr.utils.EnglishDictionary
import pprint.PPrinter
import scala.util.matching.Regex
import spindex._
// import EnrichGeometricFigures._
import ComponentTypeEnrichments._

import textboxing.{TextBoxing => TB}, TB._

object Shell {

  import ShellCommands._

  def main(args: Array[String]): Unit = {
    val hello = "Hello"
    val predef =
      s"""|import edu.umass.cs.iesl.watr
          |import watr._, spindex._, ComponentRendering._
          |import shell._
          |import ShellCommands._
          |implicit val pp0 = pprintComponent
          |implicit val pp1 = pprintBox
          |""".stripMargin

    val welcomeBanner =
      s"""|>> WatrWorks Shell <<
          |""".stripMargin


    ammonite.Main(
      predef = predef,
      //   defaultPredef: Boolean = true,
      //   storageBackend: Storage = new Storage.Folder(Defaults.ammoniteHome),
      wd = pwd,
      welcomeBanner = Some(welcomeBanner),
      inputStream = System.in,
      outputStream  = System.out,
      errorStream = System.err,
      verboseOutput = true
    ).run(
      "corpus" -> initCorpus()
    )
  }
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

      val line = VisualLine.render(thisComponent).getOrElse("<could not render>".box)
      val b0 = alignHoriz(left)(30)(vjoins()(nonAcceptedWords))
      vjoin()(
        hjoin()(
          b0,
          vjoin()(line, indent(4)(thisComponent.targetRegion.prettyPrint.box))
        ),
        emptyBox(1)(1)
      )
    }

    def show(): TB.Box = {
      VisualLine.render(thisComponent).getOrElse("<could not render>".box)
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
      VisualLine.render(thisComponent)
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
      val rendered = VisualLine.render(thisComponent).toString

      if (rendered.matches(re)) {
        Some(thisComponent)
      } else None
    }
  }

  implicit class RicherDocumentSegmenter(val thisDocumentSegmenter: DocumentSegmenter) extends AnyVal {
    def lines(): Seq[Component] = {
      val zoneIndexer = thisDocumentSegmenter.zoneIndexer
      val lineSpine = zoneIndexer.bioSpine("TextBlockSpine")

      for {
        linec <- lineSpine
        line = linec.component
      } yield line
    }
  }
  implicit class RicherCorpusEntry(val thisCorpusEntry: CorpusEntry) extends AnyVal {

    def lines(): Seq[Component]= {
      val lls = for {
        segmenter <- thisCorpusEntry.segment()
      } yield {
        segmenter.lines()
      }
      lls.getOrElse(Seq())
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

    def showSketchyLines(n: Int = 0, skip: Int = 0): Seq[Box] = {
      val entries = thisCorpus.entries()
      val skipped = if (skip > 0) entries.drop(skip) else entries
      val taken = if (n > 0) skipped.take(n) else skipped

      val lls = for (entry <- taken) yield {
        val elines = entry.lines
          .filter(_.hasUnknownWords())
          .map(c => c.showUnknowns beside indent(8)(entry.entryDescriptor.box))

        elines
      }
      lls.flatten
    }
  }
}
