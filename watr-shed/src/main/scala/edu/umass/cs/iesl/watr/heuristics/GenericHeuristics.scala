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
import geometry.syntax._
import Constants._
import Utils._
import textreflow.data._
import utils.ExactFloats._

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

        var prevCharRight: Int = -1 * SPACE_BETWEEN_WORDS_THRESHOLD
        val yPosition: Int = getYPosition(textReflow = textReflow)
        var charAtomIndex: Int = 0

        for (charAtom <- textReflow.charAtoms()) {
            if (charAtom.bbox.top.asInt().==(yPosition)) {
                if (charAtom.bbox.left.asInt() - prevCharRight >= SPACE_BETWEEN_WORDS_THRESHOLD) {
                    tokens += currentToken.mkString
                    currentToken.clear()
                }
                currentToken += charAtom.char
                prevCharRight = charAtom.bbox.right.asInt()
            }
            else if (charAtom.bbox.right.asInt() <= prevCharRight) {
                currentToken += charAtom.char
            }
            else if (charAtomIndex < textReflow.charAtoms().length - 1 && (charAtom.bbox.left <= textReflow.charAtoms()(charAtomIndex + 1).bbox.left && charAtom.bbox.right >= textReflow.charAtoms()(charAtomIndex + 1).bbox.right)) {
                currentToken += charAtom.char
            }
            // test this component
            else {
                tokens += currentToken.mkString
                currentToken.clear()
            }
            charAtomIndex += 1
        }

        if (currentToken.nonEmpty) {
            tokens += currentToken.mkString
            currentToken.clear()
        }

        tokens.filter(_.nonEmpty) //.filter( token => !(token.length.==(1) && PUNCTUATIONS.contains(token.head)))
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
                if (usualSpaceWidth == 0 && prevCharPosition.!=(-1)) {
                    usualSpaceWidth = charAtom.bbox.left.asInt - prevCharPosition
                }
                else if ((charAtom.bbox.left.asInt - prevCharPosition) > 2 * usualSpaceWidth && usualSpaceWidth.!=(0)) {
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
            if (charAtom.bbox.top.asInt().==(yPosition) || (currentCharAtomIndex > 0
                && charAtom.bbox.right < textReflow.charAtoms()(currentCharAtomIndex - 1).bbox.right
                && textReflow.charAtoms()(currentCharAtomIndex - 1).bbox.top.asInt().==(yPosition))) {
                prevCharPosition = charAtom.bbox.right.asInt()
            }
            currentCharAtomIndex += 1

        }


        if (separateComponents.nonEmpty) {
            return separateComponents.filter(_.nonEmpty)
        }
        componentsSeparatedByText
    }

}
