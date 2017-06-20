package edu.umass.cs.iesl.watr
package heuristics

import TypeTags._
import geometry.LTBounds
import Constants._
import Utils._

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
                    if (isOfFirstNameInitialFormat(textReflowToken) && separateAuthorName.length.==(1)) {
                        separateAuthorName += textReflowToken.replace(COMMA, BLANK)
                        separateAuthorNames += separateAuthorName.filter(_.nonEmpty).mkString(SPACE_SEPARATOR)
                        separateAuthorName.clear()
                    }
                    else {
                        separateAuthorNames += separateAuthorName.filter(_.nonEmpty).mkString(SPACE_SEPARATOR)
                        separateAuthorName.clear()
                        if(textReflowToken.takeRight(1).equals(PERIOD)){
                            textReflowToken.dropRight(1)
                        }
                        separateAuthorName += textReflowToken
                    }
                    checkNextFlag = false
                }
                else {
                    if(textReflowToken.takeRight(1).equals(PERIOD)){
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

        if (!authorName.contains(COMMA)) {
            lastName = Some(authorNameComponents(authorNameComponents.length - 1))
            if (authorNameComponents.length.>(1)) {
                firstName = Some(authorNameComponents(0))
                if (authorNameComponents.length.>(2)) {
                    middleName = Some(authorNameComponents.slice(1, authorNameComponents.length - 1).filter(_.nonEmpty).mkString(SPACE_SEPARATOR))
                    var lastNameIndex: Int = authorNameComponents.length - 1
                    while(VALID_SURNAME_PARTICLES.contains(authorNameComponents(lastNameIndex-1).toLowerCase)){
                        lastNameIndex -= 1
                    }
                    lastName = Some(authorNameComponents.slice(lastNameIndex, authorNameComponents.length).filter(_.nonEmpty).mkString(SPACE_SEPARATOR))
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

        NameWithBBox(firstName = firstNameRepresentation, middleName = middleNameRepresentation, lastName = lastNameRepresentation, bbox = LTBounds(FloatRep(-1), FloatRep(-1), FloatRep(-1), FloatRep(-1)))
    }

}
