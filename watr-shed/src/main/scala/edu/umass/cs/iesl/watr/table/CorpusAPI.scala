package edu.umass.cs.iesl.watr
package table  //;import acyclic.file

import spindex._

trait CorpusAPI extends JargonDictionaries {
  import TB._
  import java.net.URI
  import geometry._
  import textreflow._

  import edu.umass.cs.iesl.watr.utils.EnglishDictionary
  import TextReflowConversion._
  import ComponentTypeEnrichments._

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


  implicit class RicherCorpus(val thisCorpus: Corpus)  {

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
      // val lls = for (entry <- chooseEntries(n, skip)) yield {
      //   entry.lines.filter(_.hasUnknownWords())
      // }
      // lls.flatten
      ???
    }

    def showSketchyLines(n: Int = 0, skip: Int = 0): Seq[Box] = {
      // val lls = for (entry <- chooseEntries(n, skip)) yield {
      //   entry.lines
      //     .filter(_.hasUnknownWords())
      //     .map(formatLineComponent(entry, _))
      // }
      // lls.flatten
      ???
    }
  }

  implicit class RicherComponent(val thisComponent: Component) {
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
      toTextReflow(thisComponent).map(t => t.toText().box).getOrElse("<could not render>".box)
    }

    def webShow(): String = {
      // val text = thisComponent.show()
      // import WebShow._
      // import texttags._
      // val imgUri = thisComponent.extractImage()

      // val html =
      //   <.tr(
      //     <.td(<.a(^.href := imgUri.toString())),
      //     <.td(text.toString)
      //   )

      // html.toString()
      ???
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
      toTextReflow(thisComponent)
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
      val rendered = toTextReflow(thisComponent).toString

      if (rendered.matches(re)) {
        Some(thisComponent)
      } else None
    }


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



}
