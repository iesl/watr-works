package edu.umass.cs.iesl.watr
package heuristics

import geometry.LTBounds
import Constants._
import Utils.{containsPattern, _}
import textreflow.data._
import textreflow.TextReflowF.TextReflow

import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks._
import utils.ExactFloats._

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


    def getSeparateAuthorNameComponents(authorName: String): ListBuffer[(String, String)] = {

        val authorNameComponents = authorName.split(SPACE_SEPARATOR)
        val lastName = ListBuffer[String]()
        val firstName = ListBuffer[String]()
        val middleName = ListBuffer[String]()
        val hereditySuffix = ListBuffer[String]()
        val degree = ListBuffer[String]()
        val nobilityParticles = ListBuffer[String]()

        val authorNameRepresentation: ListBuffer[(String, String)] = new ListBuffer[(String, String)]()

        if (!authorName.contains(COMMA)) {
            lastName += authorNameComponents(authorNameComponents.length - 1)
            if (authorNameComponents.length.>(1)) {
                firstName += authorNameComponents(0)
                if (authorNameComponents.length.>(2)) {
                    var degreeIndex: Int = authorNameComponents.length - 1
                    while (containsPattern(reflowString = authorNameComponents(degreeIndex), VALID_DEGREES)) {
                        degreeIndex -= 1
                    }
                    if (degreeIndex < authorNameComponents.length - 1) {
                        degree.++=(authorNameComponents.slice(degreeIndex + 1, authorNameComponents.length).filter(_.nonEmpty))
                    }
                    var hereditySuffixIndex: Int = degreeIndex
                    while (containsPattern(reflowString = authorNameComponents(hereditySuffixIndex), VALID_HEREDITY_SUFFIXES)) {
                        hereditySuffixIndex -= 1
                    }
                    if (hereditySuffixIndex < degreeIndex) {
                        hereditySuffix.++=(authorNameComponents.slice(hereditySuffixIndex + 1, degreeIndex + 1).filter(_.nonEmpty))
                    }
                    val lastNameIndex: Int = hereditySuffixIndex
                    var nobilityParticleIndex: Int = lastNameIndex - 1
                    while (nobilityParticleIndex > 0 && containsPattern(reflowString = authorNameComponents(nobilityParticleIndex).toLowerCase, VALID_NOBILITY_PARTICLES)) {
                        nobilityParticleIndex -= 1
                    }
                    lastName.clear()
                    lastName.+=(authorNameComponents(lastNameIndex))
                    nobilityParticles.++=(authorNameComponents.slice(nobilityParticleIndex + 1, lastNameIndex).filter(_.nonEmpty))
                    middleName.++=(authorNameComponents.slice(1, nobilityParticleIndex + 1).filter(_.nonEmpty))
                }
            }
        }
//        else {
//            lastName = authorName.toCharArray.slice(0, authorName.indexOf(COMMA))
//            val remainingNameComponents = authorName.toCharArray.slice(getStartIndexAfterComma(authorName), authorName.length).mkString.split(SPACE_SEPARATOR)
//            firstName = Some(remainingNameComponents(0))
//            if (remainingNameComponents.length.>(1)) {
//                middleName = Some(remainingNameComponents.slice(1, remainingNameComponents.length).filter(_.nonEmpty).mkString(SPACE_SEPARATOR))
//            }
//
//        }

        firstName.foreach{
            nameComponent => authorNameRepresentation.+=((nameComponent, "FIRST-NAME"))
        }
        middleName.foreach{
            nameComponent => authorNameRepresentation.+=((nameComponent, "MIDDLE-NAME"))
        }
        nobilityParticles.foreach{
            nameComponent => authorNameRepresentation.+=((nameComponent, "NOBILITY-PARTICLE"))
        }
        lastName.foreach{
            nameComponent => authorNameRepresentation.+=((nameComponent, "LAST-NAME"))
        }
        hereditySuffix.foreach{
            nameComponent => authorNameRepresentation.+=((nameComponent, "HEREDITY-SUFFIX"))
        }
        degree.foreach{
            nameComponent => authorNameRepresentation.+=((nameComponent, "DEGREE"))
        }

        authorNameRepresentation
    }

    def getBoundingBoxesForAuthorNames(name: Seq[(String, String)], geometricallySeparatedName: String, textReflow: TextReflow): ListBuffer[(String, String, LTBounds)] = {

        val nameWithBBox: ListBuffer[(String, String, LTBounds)] = new ListBuffer[(String, String, LTBounds)]()

        var nameIndices: (Int, Int) = getIndexesForComponents(component = geometricallySeparatedName.replace(SPACE_SEPARATOR, BLANK), textReflow = textReflow, (0, textReflow.charAtoms().length))
        var nameStartIndex: Int = nameIndices._1
        val nameEndIndex: Int = nameIndices._2

        name.foreach{
            nameComponent => {
                nameIndices = getIndexesForComponents(component = nameComponent._1, textReflow = textReflow, indexRange = (nameStartIndex, nameEndIndex))
                nameStartIndex = nameIndices._2
                nameWithBBox.+=((nameComponent._1, nameComponent._2, getBoundingBoxesWithIndexesFromReflow(indexes = nameIndices, textReflow=textReflow)))
            }
        }

        nameWithBBox
    }


}
