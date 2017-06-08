package edu.umass.cs.iesl.watr.heuristics

import edu.umass.cs.iesl.watr.textreflow.data.TextReflow
import edu.umass.cs.iesl.watr.textreflow.data._

import util.control.Breaks._
import Constants._
import Utils._
import edu.umass.cs.iesl.watr.geometry.CharAtom

import scala.collection.mutable.ListBuffer

object AuthorNameHeuristics {

    def tokenizeAuthorNames(authorTextReflow: TextReflow): ListBuffer[String] = {
        val authorNamesTokens: ListBuffer[String] = ListBuffer[String]()
        val authorName: ListBuffer[Char] = ListBuffer[Char]()

        var prevCharPosition: Int = -1
        var yPosition: Int = -1

        for (charAtom <- authorTextReflow.charAtoms()) {
            breakable{
//                if(charAtom.bbox.left.asInstanceOf[Int] + charAtom.bbox.width.asInstanceOf[Int] < prevCharPosition){
//                    break
//                }
                val currentCharacter = charAtom.char.toCharArray.head
                if (currentCharacter.isLetter && yPosition.==(-1)) {
                    yPosition = charAtom.bbox.top.asInstanceOf[Int]
                    if (prevCharPosition.==(-1)) {
                        authorName += currentCharacter
                    }
                }
                if (prevCharPosition > 0) {
                    val spaceBetweenChars = charAtom.bbox.left.asInstanceOf[Int] - prevCharPosition
                    if (spaceBetweenChars < SPACE_BETWEEN_WORDS_THRESHOLD){
                        if(charAtom.bbox.top.==(yPosition)) {
                            authorName += currentCharacter
                        }
                        else{
                            authorNamesTokens += authorName.mkString
                            authorName.clear()
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
                if(charAtom.bbox.top.asInstanceOf[Int].==(yPosition)){
                    prevCharPosition = charAtom.bbox.left.asInstanceOf[Int] + charAtom.bbox.width.asInstanceOf[Int]
                }
            }
        }

        if (authorName.nonEmpty) {
            authorNamesTokens += authorName.mkString
            authorName.clear()
        }

        mergeConsecutiveNameTokens(authorNamesTokens.filter(_.nonEmpty))
    }

    def getSeparateAuthorNamesByText(authorNamesTokens: ListBuffer[String]): ListBuffer[String] = {

        val separateAuthorNames: ListBuffer[String] = ListBuffer[String]()
        val separateAuthorName: ListBuffer[String] = ListBuffer[String]()

        var checkNextFlag: Boolean = false

        for (authorNameToken <- authorNamesTokens) {
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
                    if (isOfFirstNameInitialFormat(authorNameToken)) {
                        separateAuthorName += authorNameToken.replace(COMMA, BLANK)
                        separateAuthorNames += separateAuthorName.mkString(NAME_SEPARATOR)
                        separateAuthorName.clear()
                    }
                    else {
                        separateAuthorNames += separateAuthorName.mkString(NAME_SEPARATOR)
                        separateAuthorName.clear()
                        separateAuthorName += authorNameToken
                    }
                    checkNextFlag = false
                }
                else {
                    separateAuthorName += authorNameToken
                }

            }

        }

        if (separateAuthorName.nonEmpty) {
            separateAuthorNames += separateAuthorName.mkString(NAME_SEPARATOR)
        }
        separateAuthorNames.filter(_.nonEmpty)
    }

    def getSeparateAuthorNamesByGeometry(authorNamesSeparatedByText: ListBuffer[String], authorTextReflow: TextReflow): ListBuffer[String] = {

        val separateAuthorNames: ListBuffer[String] = ListBuffer[String]()
        val separateAuthorName: ListBuffer[String] = ListBuffer[String]()

        var currentAuthorNameIndex: Int = 0
        var currentAuthorName: String = authorNamesSeparatedByText(currentAuthorNameIndex)
        var currentAuthorNameComponentIndex: Int = 0
        var currentAuthorNameComponent: String = currentAuthorName.split(NAME_SEPARATOR)(currentAuthorNameComponentIndex)
        var prevCharPosition: Int = -1
        var usualSpaceWidth: Int = 0
        var processingComplete: Boolean = false
        var currentCharAtomIndex: Int = 0
        var yPosition: Int = 0

        while (currentCharAtomIndex < authorTextReflow.charAtoms().length && currentAuthorNameIndex < authorNamesSeparatedByText.length) {
            var charAtom: CharAtom = authorTextReflow.charAtoms()(currentCharAtomIndex)
            currentAuthorNameComponent = currentAuthorName.split(NAME_SEPARATOR)(currentAuthorNameComponentIndex)
            currentAuthorName = authorNamesSeparatedByText(currentAuthorNameIndex)
            val currentCharacter = charAtom.char.toCharArray.head
            if (currentCharacter.equals(currentAuthorNameComponent.head)) {
                if(yPosition.==(0)){
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
            charAtom = authorTextReflow.charAtoms()(currentCharAtomIndex)
            currentCharAtomIndex += 1
            if(charAtom.bbox.top.asInstanceOf[Int].==(yPosition)){
                prevCharPosition = charAtom.bbox.left.asInstanceOf[Int] + charAtom.bbox.width.asInstanceOf[Int]
            }
        }


        if (separateAuthorNames.nonEmpty) {
            return separateAuthorNames.filter(_.nonEmpty)
        }
        authorNamesSeparatedByText
    }
}
