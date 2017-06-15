package edu.umass.cs.iesl.watr

package heuristics

import edu.umass.cs.iesl.watr.textreflow.data.TextReflow
import edu.umass.cs.iesl.watr.textreflow.data._

import util.control.Breaks._
import Constants._
import Utils._
import TypeTags._
import edu.umass.cs.iesl.watr.geometry.{CharAtom, LTBounds}

import scala.collection.mutable.ListBuffer

object GenericHeuristics {

    def tokenizeTextReflow(textReflow: TextReflow): ListBuffer[String] = {
        val authorNamesTokens: ListBuffer[String] = ListBuffer[String]()
        val authorName: ListBuffer[String] = ListBuffer[String]()

        var prevCharPosition: Int = -1
        var yPosition: Int = -1

        for (charAtom <- textReflow.charAtoms()) {
            val currentCharacter = charAtom.char
            if (isLetterString(currentCharacter) && yPosition.==(-1)) {
                yPosition = charAtom.bbox.top.asInstanceOf[Int]
                if (prevCharPosition.==(-1)) {
                    authorName += currentCharacter
                }
            }
            if (prevCharPosition > 0) {
                val spaceBetweenChars = charAtom.bbox.left.asInstanceOf[Int] - prevCharPosition
                if (spaceBetweenChars < SPACE_BETWEEN_WORDS_THRESHOLD) {
                    if (charAtom.bbox.top.==(yPosition)) {
                        authorName += currentCharacter
                    }
                    else {
                        if (charAtom.bbox.right.asInstanceOf[Int] > prevCharPosition) {
                            authorNamesTokens += authorName.mkString
                            authorName.clear()
                        }
                        else {
                            authorName += currentCharacter
                        }
                    }

                }
                else if (spaceBetweenChars >= SPACE_BETWEEN_WORDS_THRESHOLD) {
                    authorNamesTokens += authorName.mkString
                    authorName.clear()
                    if (charAtom.bbox.top.==(yPosition)) {
                        authorName += currentCharacter
                    }
                }
            }
            if (charAtom.bbox.top.asInstanceOf[Int].==(yPosition)) {
                prevCharPosition = charAtom.bbox.right.asInstanceOf[Int]
            }
        }

        if (authorName.nonEmpty) {
            authorNamesTokens += authorName.mkString
            authorName.clear()
        }

        authorNamesTokens.filter(_.nonEmpty)
    }

    def getSeparateComponentsByText(tokenizedTextReflow: ListBuffer[String]): ListBuffer[String] = {

        val separateAuthorNames: ListBuffer[String] = ListBuffer[String]()
        val separateAuthorName: ListBuffer[String] = ListBuffer[String]()

        var checkNextFlag: Boolean = false

        for (authorNameToken <- tokenizedTextReflow) {
            breakable {
                if (WORD_SEPARATORS.contains(authorNameToken.toLowerCase)) {
                    separateAuthorNames += separateAuthorName.mkString(NAME_SEPARATOR)
                    separateAuthorName.clear()
                    if (checkNextFlag) {
                        checkNextFlag = false
                    }
                }
                else if (!checkNextFlag && PUNCTUATION_SEPARATORS.contains(authorNameToken.takeRight(n = 1))) {
                    separateAuthorName += authorNameToken.replace(authorNameToken.takeRight(n = 1), BLANK)
                    checkNextFlag = true
                    break
                }
                else if (checkNextFlag) {
                    if (isOfFirstNameInitialFormat(authorNameToken) && separateAuthorName.length.==(1)) {
                        separateAuthorName += authorNameToken.replace(COMMA, BLANK)
                        separateAuthorNames += separateAuthorName.mkString(NAME_SEPARATOR)
                        separateAuthorName.clear()
                    }
                    else {
                        separateAuthorNames += separateAuthorName.mkString(NAME_SEPARATOR)
                        separateAuthorName.clear()
                        if(authorNameToken.takeRight(1).equals(PERIOD)){
                            authorNameToken.dropRight(1)
                        }
                        separateAuthorName += authorNameToken
                    }
                    checkNextFlag = false
                }
                else {
                    if(authorNameToken.takeRight(1).equals(PERIOD)){
                        authorNameToken.dropRight(1)
                    }
                    separateAuthorName += authorNameToken
                }
            }
        }

        if (separateAuthorName.nonEmpty) {
            separateAuthorNames += separateAuthorName.mkString(NAME_SEPARATOR)
        }
        separateAuthorNames.filter(_.nonEmpty)
    }

    def getSeparateComponentsByGeometry(componentsSeparatedByText: ListBuffer[String], textReflow: TextReflow): ListBuffer[String] = {

        val separateAuthorNames: ListBuffer[String] = ListBuffer[String]()
        val separateAuthorName: ListBuffer[String] = ListBuffer[String]()

        var currentAuthorNameIndex: Int = 0
        var currentAuthorName: String = componentsSeparatedByText(currentAuthorNameIndex)
        var currentAuthorNameComponentIndex: Int = 0
        var currentAuthorNameComponent: String = currentAuthorName.split(NAME_SEPARATOR)(currentAuthorNameComponentIndex)
        var prevCharPosition: Int = -1
        var usualSpaceWidth: Int = 0
        var processingComplete: Boolean = false
        var currentCharAtomIndex: Int = 0
        var yPosition: Int = 0

        while (currentCharAtomIndex < textReflow.charAtoms().length && currentAuthorNameIndex < componentsSeparatedByText.length) {
            var charAtom: CharAtom = textReflow.charAtoms()(currentCharAtomIndex)
            currentAuthorName = componentsSeparatedByText(currentAuthorNameIndex)
            currentAuthorNameComponent = currentAuthorName.split(NAME_SEPARATOR)(currentAuthorNameComponentIndex)
            val currentCharacter = charAtom.char.toCharArray.head
            if (currentCharacter.equals(currentAuthorNameComponent.head)) {
                if (yPosition.==(0)) {
                    yPosition = charAtom.bbox.top.asInstanceOf[Int]
                }
                if (prevCharPosition < 0) {
                    separateAuthorName += currentAuthorNameComponent
                }
                else {
                    if (usualSpaceWidth == 0) {
                        usualSpaceWidth = charAtom.bbox.left.asInstanceOf[Int] - prevCharPosition
                    }
                    else {
                        if ((charAtom.bbox.left.asInstanceOf[Int] - prevCharPosition) > 2 * usualSpaceWidth) {
                            separateAuthorNames += separateAuthorName.mkString(NAME_SEPARATOR)
                            separateAuthorName.clear()
                        }
                    }
                    separateAuthorName += currentAuthorNameComponent
                    if (currentAuthorNameComponentIndex + 1 == currentAuthorName.split(NAME_SEPARATOR).length) {
                        separateAuthorNames += separateAuthorName.mkString(NAME_SEPARATOR)
                        separateAuthorName.clear()
                    }
                }

                currentCharAtomIndex += currentAuthorNameComponent.length - 1
                val indices = getNextNameAndComponentIndices(currentAuthorNameComponentIndex, currentAuthorNameIndex, currentAuthorName)
                currentAuthorNameComponentIndex = indices._1
                currentAuthorNameIndex = indices._2

            }
            charAtom = textReflow.charAtoms()(currentCharAtomIndex)
            if (charAtom.bbox.top.asInstanceOf[Int].==(yPosition) || (charAtom.bbox.right.asInstanceOf[Int] < textReflow.charAtoms()(currentCharAtomIndex - 1).bbox.right.asInstanceOf[Int]
                                                                        && textReflow.charAtoms()(currentCharAtomIndex - 1).bbox.top.asInstanceOf[Int].==(yPosition))) {
                prevCharPosition = charAtom.bbox.right.asInstanceOf[Int]
            }
            currentCharAtomIndex += 1

        }


        if (separateAuthorNames.nonEmpty) {
            return separateAuthorNames.filter(_.nonEmpty)
        }
        componentsSeparatedByText
    }

}
