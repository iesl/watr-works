package edu.umass.cs.iesl.watr

package heuristics

import edu.umass.cs.iesl.watr.textreflow.data.TextReflow
import edu.umass.cs.iesl.watr.textreflow.data._

import util.control.Breaks._
import Constants._
import Utils._
import edu.umass.cs.iesl.watr.geometry.CharAtom

import scala.collection.mutable.ListBuffer

object AuthorNameHeuristics {

    def tokenizeAuthorNamesFromTextReflow(authorTextReflow: TextReflow): ListBuffer[String] = {
        val authorNamesTokens: ListBuffer[String] = ListBuffer[String]()
        val authorName: ListBuffer[Char] = ListBuffer[Char]()

        var prevCharPosition: Int = -1
        var yPosition: Int = -1

        for (charAtom <- authorTextReflow.charAtoms()) {
            val currentCharacter = charAtom.char.toCharArray.head
            if (currentCharacter.isLetter && yPosition.==(-1)) {
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
                    else{
                        if(charAtom.bbox.right.asInstanceOf[Int] > prevCharPosition){
                            authorNamesTokens += authorName.mkString
                            authorName.clear()
                        }
                        else{
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
            charAtom = authorTextReflow.charAtoms()(currentCharAtomIndex)
            currentCharAtomIndex += 1
            if (charAtom.bbox.top.asInstanceOf[Int].==(yPosition)) {
                prevCharPosition = charAtom.bbox.left.asInstanceOf[Int] + charAtom.bbox.width.asInstanceOf[Int]
            }
        }


        if (separateAuthorNames.nonEmpty) {
            return separateAuthorNames.filter(_.nonEmpty)
        }
        authorNamesSeparatedByText
    }


    def getSeparateAuthorNameComponents(authorName: String): scala.collection.mutable.Map[String, String] = {

        val separateNameComponents = scala.collection.mutable.Map[String, String]()
        val authorNameComponents = authorName.split(NAME_SEPARATOR)
        var lastName = None: Option[String]
        var firstName = None: Option[String]
        var middleName = None: Option[String]

        if(! authorName.contains(COMMA)){
            lastName = Some(authorNameComponents(authorNameComponents.length-1))
            if(authorNameComponents.length.>(1)){
                firstName = Some(authorNameComponents(0))
                if(authorNameComponents.length.>(2)){
                    middleName = Some(authorNameComponents.slice(1, authorNameComponents.length-1).mkString(NAME_SEPARATOR))
                    if(VALID_SURNAME_PARTICLES.contains(authorNameComponents(authorNameComponents.length-2).toLowerCase)){
                        lastName = Some(authorNameComponents.slice(authorNameComponents.length-2, authorNameComponents.length).mkString(NAME_SEPARATOR))
                        middleName = Some(authorNameComponents.slice(1, authorNameComponents.length-2).mkString(NAME_SEPARATOR))
                    }
                }
            }


        }
        else{
            lastName = Some(authorName.toCharArray.slice(0, authorName.indexOf(COMMA)).mkString)
            val remainingNameComponents = authorName.toCharArray.slice(getStartIndexAfterComma(authorName), authorName.length).mkString.split(NAME_SEPARATOR)
            firstName = Some(remainingNameComponents(0))
            if(remainingNameComponents.length.>(1)){
                middleName = Some(remainingNameComponents.slice(1, remainingNameComponents.length).mkString(NAME_SEPARATOR))
            }

        }

        separateNameComponents += (LAST_NAME -> lastName.getOrElse(BLANK))
        separateNameComponents += (FIRST_NAME -> firstName.getOrElse(BLANK))
        separateNameComponents += (MIDDLE_NAME -> middleName.getOrElse(BLANK))

        separateNameComponents
    }
}
