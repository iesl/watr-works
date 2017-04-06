package edu.umass.cs.iesl.watr
package table

import spindex._
import geometry._

import textreflow.data._
import textboxing.{TextBoxing => TB}, TB._

import edu.umass.cs.iesl.watr.utils.EnglishDictionary
import TextReflowConversion._

trait ComponentEnrichments extends JargonDictionaries {

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


  implicit class RicherComponent(val theComponent: Component) {
    def showUnknowns(): TB.Box = {
      val nonDictWords = for {
        word <- theComponent.words() if !wordInDictionary(word)
      } yield word

      val nonAcceptedWords = for {
        word <- nonDictWords
        if !acceptableWordPatterns.exists(re => word.matches(re.regex))
      } yield word.box

      val line = theComponent.show
      val b0 = alignHoriz(left, 30, vjoins()(nonAcceptedWords))
      vjoin()(
        hjoin()(
          b0,
          vjoin()(line, indent(4)(theComponent.targetRegion.toString.box))
        ),
        emptyBox(1)(1)
      )
    }

    def show(): TB.Box = {
      toTextReflow(theComponent).map(t => t.toText().box).getOrElse("<could not render>".box)
    }

    def filter(fn: (Component) => Boolean): Option[Component] = {
      if (fn(theComponent)) {
        theComponent.some
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
      toTextReflow(theComponent)
        .map(_.toString.split(" ").map(trimWord(_)).toSeq)
        .getOrElse(Seq())
    }

    def hasUnknownWords(): Boolean= {
      theComponent.words().exists({ word =>
        val isDictWord = wordInDictionary(word)
        def isAcceptable = acceptableWordPatterns.exists({re =>
          word.matches(re.regex)
        })

        !(isDictWord || isAcceptable)
      })
    }

    def allWordLine(): Option[Component] = {
      theComponent.filter((c) =>
        c.words().map(wordInDictionary(_)).reduce(_ && _)
      )
    }
    def nonWordLine(): Option[Component] = {
      theComponent.filter((c) =>
        c.words().exists(!wordInDictionary(_))
      )
    }

    def grep(re: String): Option[Component]= {
      val rendered = toTextReflow(theComponent).toString

      if (rendered.matches(re)) {
        Some(theComponent)
      } else None
    }


  }

}
