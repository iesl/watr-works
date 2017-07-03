package edu.umass.cs.iesl.watr
package heuristics

import geometry.CharAtom
import Constants._
import Utils._
// import textreflow._
import textreflow.data._
import geometry._
// import geometry.syntax._
// import TypeTags._
import scala.collection.mutable.ListBuffer
// import scalaz.syntax.equal._
// import scalaz.std.anyVal._

import watrmarks.{StandardLabels => LB}

object GenHeuristics {

  def orderCharAtomsByFrequency(textReflow: TextReflow, orderf: (CharAtom) => FloatExact): Seq[(FloatExact, Int)] = {
    val countedAtoms: Map[FloatExact, Int] =
      textReflow.charAtoms()
        .groupBy(orderf(_))
        .mapValues { _.length }

    countedAtoms.toList
      .filter{case (_, count) => count > 1 }
      .sortBy(_._2).reverse
  }

  // ACS if there is more than 1 value with a max frequency, this will arbitrarily pick one as the max
  def getMostCommonTopPosition(textReflow: TextReflow): Option[FloatExact] = {
    orderCharAtomsByFrequency(textReflow, _.bbox.top)
      .headOption.map(_._1)
  }

  def tokenizeTextReflow(textReflow: TextReflow): Seq[TextReflow] = {
    topLevelLabeledSlices(textReflow, LB.Token)
  }
}


object GenericHeuristics {


    def tokenizeTextReflow(textReflow: TextReflow): ListBuffer[String] = {
        val tokens: ListBuffer[String] = ListBuffer[String]()
        val currentToken: ListBuffer[String] = ListBuffer[String]()

        var prevCharPosition: Int = -1 * SPACE_BETWEEN_WORDS_THRESHOLD
        val yPosition: Int = getYPosition(textReflow = textReflow)

        for (charAtom <- textReflow.charAtoms()) {
            if (charAtom.bbox.top.asInstanceOf[Int].==(yPosition)) {
                if (charAtom.bbox.left.asInstanceOf[Int] - prevCharPosition >= SPACE_BETWEEN_WORDS_THRESHOLD) {
                    tokens += currentToken.mkString
                    currentToken.clear()
                }
                currentToken += charAtom.char
                prevCharPosition = charAtom.bbox.right.asInstanceOf[Int]
            }
            else if (charAtom.bbox.right.asInstanceOf[Int] < prevCharPosition) {
                currentToken += charAtom.char
            }
        }

        if (currentToken.nonEmpty) {
            tokens += currentToken.mkString
            currentToken.clear()
        }

        tokens.filter(_.nonEmpty)
    }

    def getSeparateComponentsByText(tokenizedTextReflow: ListBuffer[String]): ListBuffer[String] = {

        val separateComponents: ListBuffer[String] = ListBuffer[String]()
        val separateComponent: ListBuffer[String] = ListBuffer[String]()

        for (textReflowToken <- tokenizedTextReflow) {
            if (WORD_SEPARATORS.contains(textReflowToken.toLowerCase)) {
                separateComponents += separateComponent.mkString(SPACE_SEPARATOR)
                separateComponent.clear()
            }
            else if (PUNCTUATION_SEPARATORS.contains(textReflowToken.takeRight(n = 1))) {
                separateComponent += textReflowToken.dropRight(n = 1)
                separateComponents += separateComponent.mkString(SPACE_SEPARATOR)
                separateComponent.clear()
            }
            else {
                separateComponent += textReflowToken
            }
        }

        if (separateComponent.nonEmpty) {
            separateComponents += separateComponent.mkString(SPACE_SEPARATOR)
        }
        separateComponents.filter(_.nonEmpty)
    }

    def getSeparateComponentsByGeometry(componentsSeparatedByText: ListBuffer[String], textReflow: TextReflow): ListBuffer[String] = {

        val separateComponents: ListBuffer[String] = ListBuffer[String]()
        val separateComponent: ListBuffer[String] = ListBuffer[String]()

        var currentComponentIndex: Int = 0
        var currentComponent: String = componentsSeparatedByText(currentComponentIndex)
        var currentSeparateComponentIndex: Int = 0
        var currentSeparateComponent: String = currentComponent.split(SPACE_SEPARATOR)(currentSeparateComponentIndex)

        var prevCharPosition: Int = -1
        var usualSpaceWidth: Int = 0
        var currentCharAtomIndex: Int = 0
        val yPosition: Int = getYPosition(textReflow = textReflow)

        while (currentCharAtomIndex < textReflow.charAtoms().length && currentComponentIndex < componentsSeparatedByText.length) {
            currentComponent = componentsSeparatedByText(currentComponentIndex)
            currentSeparateComponent = currentComponent.split(SPACE_SEPARATOR)(currentSeparateComponentIndex)
            val (_, currentSeparateComponentEndIndex) = getIndexesForComponents(component = currentSeparateComponent, textReflow = textReflow, (currentCharAtomIndex, textReflow.charAtoms().length))
            var charAtom: CharAtom = textReflow.charAtoms()(currentCharAtomIndex)
            val currentCharacter = charAtom.char.toCharArray.head
            if (currentCharacter.equals(currentSeparateComponent.head)) {
                if (usualSpaceWidth == 0) {
                    usualSpaceWidth = charAtom.bbox.left.asInstanceOf[Int] - prevCharPosition
                }
                else if ((charAtom.bbox.left.asInstanceOf[Int] - prevCharPosition) > 2 * usualSpaceWidth) {
                    separateComponents += separateComponent.mkString(SPACE_SEPARATOR)
                    separateComponent.clear()
                }

                separateComponent += currentSeparateComponent
                currentCharAtomIndex = currentSeparateComponentEndIndex - 1
                val indices = getNextComponentIndices(currentSeparateComponentIndex, currentComponentIndex, currentComponent)
                currentSeparateComponentIndex = indices._1
                currentComponentIndex = indices._2
                if (currentSeparateComponentIndex.==(0)) {
                    separateComponents += separateComponent.mkString(SPACE_SEPARATOR)
                    separateComponent.clear()
                }

            }
            charAtom = textReflow.charAtoms()(currentCharAtomIndex)
            if (charAtom.bbox.top.asInstanceOf[Int].==(yPosition) || (currentCharAtomIndex > 0
                && charAtom.bbox.right.asInstanceOf[Int] < textReflow.charAtoms()(currentCharAtomIndex - 1).bbox.right.asInstanceOf[Int]
                && textReflow.charAtoms()(currentCharAtomIndex - 1).bbox.top.asInstanceOf[Int].==(yPosition))) {
                prevCharPosition = charAtom.bbox.right.asInstanceOf[Int]
            }
            currentCharAtomIndex += 1

        }


        if (separateComponents.nonEmpty) {
            return separateComponents.filter(_.nonEmpty)
        }
        componentsSeparatedByText
    }

}
