package edu.umass.cs.iesl.watr
package heuristics

import TypeTags._
import geometry.LTBounds
import Constants._
import Utils.{containsPattern, _}
import textreflow.data._
import textreflow.TextReflowF.TextReflow

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._

object AuthorNameHeuristics {

    def getSeparateAuthorNamesByText(tokenizedTextReflow: ListBuffer[String]): ListBuffer[String] = {

        val separateAuthorNames: ListBuffer[String] = ListBuffer[String]()
        val separateAuthorName: ListBuffer[String] = ListBuffer[String]()

        var checkNextFlag: Boolean = false

        for (textReflowToken <- tokenizedTextReflow) {
            breakable {
                if (WORD_SEPARATORS.contains(textReflowToken.toLowerCase)) {
                    separateAuthorNames += separateAuthorName.filter(_.nonEmpty).mkString(SPACE_SEPARATOR)
                    separateAuthorName.clear()
                    if (checkNextFlag) {
                        checkNextFlag = false
                    }
                }
                else if (!checkNextFlag && PUNCTUATION_SEPARATORS.contains(textReflowToken.takeRight(n = 1))) {
                    separateAuthorName += textReflowToken.replace(textReflowToken.takeRight(n = 1), BLANK)
                    checkNextFlag = true
                    break
                }
                else if (checkNextFlag) {
                    if ((isOfNameInitialFormat(textReflowToken) && separateAuthorName.length.==(1))
                        || containsPattern(reflowString = textReflowToken, patternSeq = VALID_HEREDITY_SUFFIXES)
                        || containsPattern(reflowString = textReflowToken, patternSeq = VALID_DEGREES)) {

                        if (PUNCTUATION_SEPARATORS.contains(textReflowToken.takeRight(n = 1))) {
                            separateAuthorName += textReflowToken.replace(textReflowToken.takeRight(n = 1), BLANK)
                        }
                        else{
                            separateAuthorName += textReflowToken
                        }
                    }
                    else {
                        separateAuthorNames += separateAuthorName.filter(_.nonEmpty).mkString(SPACE_SEPARATOR)
                        separateAuthorName.clear()
                        if (textReflowToken.takeRight(1).equals(PERIOD)) {
                            textReflowToken.dropRight(1)
                        }
                        separateAuthorName += textReflowToken
                        checkNextFlag = false
                    }

                }
                else {
                    if (textReflowToken.takeRight(1).equals(PERIOD)) {
                        textReflowToken.dropRight(1)
                    }
                    separateAuthorName += textReflowToken
                }
            }
        }

        if (separateAuthorName.nonEmpty) {
            separateAuthorNames += separateAuthorName.filter(_.nonEmpty).mkString(SPACE_SEPARATOR)
        }
        separateAuthorNames.filter(_.nonEmpty)
    }

    def getSeparateAuthorNameComponents(authorName: String): NameWithBBox = {

        val authorNameComponents = authorName.split(SPACE_SEPARATOR)
        var lastName = None: Option[String]
        var firstName = None: Option[String]
        var middleName = None: Option[String]
        var hereditySuffix = None: Option[String]
        var degree = None: Option[String]

        if (!authorName.contains(COMMA)) {
            lastName = Some(authorNameComponents(authorNameComponents.length - 1))
            if (authorNameComponents.length.>(1)) {
                firstName = Some(authorNameComponents(0))
                if (authorNameComponents.length.>(2)) {
                    var degreeIndex: Int = authorNameComponents.length - 1
                    while (containsPattern(reflowString = authorNameComponents(degreeIndex), VALID_DEGREES)) {
                        degreeIndex -= 1
                    }
                    if (degreeIndex < authorNameComponents.length - 1) {
                        degree = Some(authorNameComponents.slice(degreeIndex + 1, authorNameComponents.length).filter(_.nonEmpty).mkString(SPACE_SEPARATOR))
                    }
                    var hereditySuffixIndex: Int = degreeIndex
                    while (containsPattern(reflowString = authorNameComponents(hereditySuffixIndex), VALID_HEREDITY_SUFFIXES)) {
                        hereditySuffixIndex -= 1
                    }
                    if (hereditySuffixIndex < degreeIndex) {
                        hereditySuffix = Some(authorNameComponents.slice(hereditySuffixIndex + 1, authorNameComponents.length).filter(_.nonEmpty).mkString(SPACE_SEPARATOR))
                    }
                    var lastNameIndex: Int = hereditySuffixIndex
                    while (lastNameIndex > 0 && containsPattern(reflowString = authorNameComponents(lastNameIndex - 1).toLowerCase, VALID_SURNAME_PARTICLES)) {
                        lastNameIndex -= 1
                    }
                    lastName = Some(authorNameComponents.slice(lastNameIndex, hereditySuffixIndex + 1).filter(_.nonEmpty).mkString(SPACE_SEPARATOR))
                    middleName = Some(authorNameComponents.slice(1, lastNameIndex).filter(_.nonEmpty).mkString(SPACE_SEPARATOR))
                }
            }


        }
        else {
            lastName = Some(authorName.toCharArray.slice(0, authorName.indexOf(COMMA)).mkString)
            val remainingNameComponents = authorName.toCharArray.slice(getStartIndexAfterComma(authorName), authorName.length).mkString.split(SPACE_SEPARATOR)
            firstName = Some(remainingNameComponents(0))
            if (remainingNameComponents.length.>(1)) {
                middleName = Some(remainingNameComponents.slice(1, remainingNameComponents.length).filter(_.nonEmpty).mkString(SPACE_SEPARATOR))
            }

        }

        val firstNameRepresentation: ComponentRepresentation = ComponentRepresentation(componentText = firstName.getOrElse(BLANK), componentBBox = LTBounds(FloatRep(-1), FloatRep(-1), FloatRep(-1), FloatRep(-1)))
        val middleNameRepresentation: ComponentRepresentation = ComponentRepresentation(componentText = middleName.getOrElse(BLANK), componentBBox = LTBounds(FloatRep(-1), FloatRep(-1), FloatRep(-1), FloatRep(-1)))
        val lastNameRepresentation: ComponentRepresentation = ComponentRepresentation(componentText = lastName.getOrElse(BLANK), componentBBox = LTBounds(FloatRep(-1), FloatRep(-1), FloatRep(-1), FloatRep(-1)))
        val hereditySuffixRepresentation: ComponentRepresentation = ComponentRepresentation(componentText = hereditySuffix.getOrElse(BLANK), componentBBox = LTBounds(FloatRep(-1), FloatRep(-1), FloatRep(-1), FloatRep(-1)))
        val degreeRepresentation: ComponentRepresentation = ComponentRepresentation(componentText = degree.getOrElse(BLANK), componentBBox = LTBounds(FloatRep(-1), FloatRep(-1), FloatRep(-1), FloatRep(-1)))

        NameWithBBox(firstName = firstNameRepresentation, middleName = middleNameRepresentation, lastName = lastNameRepresentation, hereditySuffix = hereditySuffixRepresentation, degree = degreeRepresentation, bBox = LTBounds(FloatRep(-1), FloatRep(-1), FloatRep(-1), FloatRep(-1)))
    }

    def getBoundingBoxesForAuthorNames(name: NameWithBBox, geometricallySeparatedName: String, textReflow: TextReflow): NameWithBBox = {

        val (nameStartIndex, nameEndIndex) = getIndexesForComponents(component = geometricallySeparatedName.replace(SPACE_SEPARATOR, BLANK), textReflow = textReflow, (0, textReflow.charAtoms().length))
        name.bBox = getBoundingBoxesWithIndexesFromReflow((nameStartIndex, nameEndIndex), textReflow)
        if (name.firstName.componentText.nonEmpty) {
            val (firstNameStartIndex, firstNameEndIndex) = getIndexesForComponents(component = name.firstName.componentText.replace(SPACE_SEPARATOR, BLANK), textReflow = textReflow, (nameStartIndex, nameEndIndex))
            name.firstName.componentBBox = getBoundingBoxesWithIndexesFromReflow((firstNameStartIndex, firstNameEndIndex), textReflow)
        }
        if (name.middleName.componentText.nonEmpty) {
            val (middleNameStartIndex, middleNameEndIndex) = getIndexesForComponents(component = name.middleName.componentText.replace(SPACE_SEPARATOR, BLANK), textReflow = textReflow, (nameStartIndex, nameEndIndex))
            name.middleName.componentBBox = getBoundingBoxesWithIndexesFromReflow((middleNameStartIndex, middleNameEndIndex), textReflow)
        }
        if (name.lastName.componentText.nonEmpty) {
            val (lastNameStartIndex, lastNameEndIndex) = getIndexesForComponents(component = name.lastName.componentText.replace(SPACE_SEPARATOR, BLANK), textReflow = textReflow, (nameStartIndex, nameEndIndex))
            name.lastName.componentBBox = getBoundingBoxesWithIndexesFromReflow((lastNameStartIndex, lastNameEndIndex), textReflow)
        }
        if (name.hereditySuffix.componentText.nonEmpty) {
            val (suffixStartIndex, suffixEndIndex) = getIndexesForComponents(component = name.lastName.componentText.replace(SPACE_SEPARATOR, BLANK), textReflow = textReflow, (nameStartIndex, nameEndIndex))
            name.hereditySuffix.componentBBox = getBoundingBoxesWithIndexesFromReflow((suffixStartIndex, suffixEndIndex), textReflow)
        }
        if (name.degree.componentText.nonEmpty) {
            val (degreeStartIndex, degreeEndIndex) = getIndexesForComponents(component = name.lastName.componentText.replace(SPACE_SEPARATOR, BLANK), textReflow = textReflow, (nameStartIndex, nameEndIndex))
            name.degree.componentBBox = getBoundingBoxesWithIndexesFromReflow((degreeStartIndex, degreeEndIndex), textReflow)
        }
        name
    }


}
