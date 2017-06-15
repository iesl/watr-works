package edu.umass.cs.iesl.watr
package heuristics

import TypeTags._
import geometry.LTBounds
import Constants._
import Utils._

object AuthorNameHeuristics {
    def getSeparateAuthorNameComponents(authorName: String): NameWithBBox = {

        val authorNameComponents = authorName.split(NAME_SEPARATOR)
        var lastName = None: Option[String]
        var firstName = None: Option[String]
        var middleName = None: Option[String]

        if (!authorName.contains(COMMA)) {
            lastName = Some(authorNameComponents(authorNameComponents.length - 1))
            if (authorNameComponents.length.>(1)) {
                firstName = Some(authorNameComponents(0))
                if (authorNameComponents.length.>(2)) {
                    middleName = Some(authorNameComponents.slice(1, authorNameComponents.length - 1).mkString(NAME_SEPARATOR))
                    if (VALID_SURNAME_PARTICLES.contains(authorNameComponents(authorNameComponents.length - 2).toLowerCase)) {
                        lastName = Some(authorNameComponents.slice(authorNameComponents.length - 2, authorNameComponents.length).mkString(NAME_SEPARATOR))
                        middleName = Some(authorNameComponents.slice(1, authorNameComponents.length - 2).mkString(NAME_SEPARATOR))
                    }
                }
            }


        }
        else {
            lastName = Some(authorName.toCharArray.slice(0, authorName.indexOf(COMMA)).mkString)
            val remainingNameComponents = authorName.toCharArray.slice(getStartIndexAfterComma(authorName), authorName.length).mkString.split(NAME_SEPARATOR)
            firstName = Some(remainingNameComponents(0))
            if (remainingNameComponents.length.>(1)) {
                middleName = Some(remainingNameComponents.slice(1, remainingNameComponents.length).mkString(NAME_SEPARATOR))
            }

        }

        val firstNameRepresentation: NameRepresentation = NameRepresentation(nameText = firstName.getOrElse(BLANK), bbox = LTBounds(FloatRep(-1), FloatRep(-1), FloatRep(-1), FloatRep(-1)))
        val middleNameRepresentation: NameRepresentation = NameRepresentation(nameText = middleName.getOrElse(BLANK), bbox = LTBounds(FloatRep(-1), FloatRep(-1), FloatRep(-1), FloatRep(-1)))
        val lastNameRepresentation: NameRepresentation = NameRepresentation(nameText = lastName.getOrElse(BLANK), bbox = LTBounds(FloatRep(-1), FloatRep(-1), FloatRep(-1), FloatRep(-1)))

        NameWithBBox(firstName = firstNameRepresentation, middleName = middleNameRepresentation, lastName = lastNameRepresentation, bbox = LTBounds(FloatRep(-1), FloatRep(-1), FloatRep(-1), FloatRep(-1)))
    }

}
