package edu.umass.cs.iesl.watr
package heuristics

import Constants._
import Utils._
import java.text.Normalizer

import edu.umass.cs.iesl.watr.geometry.LTBounds

import scala.util.control.Breaks._
import scala.collection.mutable.ListBuffer
import textreflow.TextReflowF.TextReflow
import textreflow.data._

object AffiliationsHeuristics {

    def getSeparateAffiliationComponentsByText(tokenizedTextReflow: Seq[String]): ListBuffer[String] = {

        val separateComponents: ListBuffer[String] = ListBuffer[String]()
        val separateComponent: ListBuffer[String] = ListBuffer[String]()

        for (textReflowToken <- tokenizedTextReflow) {
            if (PUNCTUATIONS.contains(textReflowToken.takeRight(n = 1).head) && !textReflowToken.takeRight(n = 1).head.equals(DOT)) {
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

        for (separateComponent <- separateComponents if separateComponent.length.==(0)) {
            separateComponents -= separateComponent
        }

        var currentComponentIndex: Int = 0

//        while (currentComponentIndex < separateComponents.length) {
//            val currentComponent = separateComponents(currentComponentIndex)
//            for (conjunction <- WORD_SEPARATORS if currentComponent.split(SPACE_SEPARATOR).length.>=(3) && currentComponent.split(conjunction).length.==(2)) {
//                separateComponents(currentComponentIndex - 1) = separateComponents.slice(currentComponentIndex - 1, currentComponentIndex + 1).mkString(COMMA.concat(SPACE_SEPARATOR))
//                separateComponents -= currentComponent
//            }
//            currentComponentIndex += 1
//        }
        separateComponents
    }

    def getCategoryForSeparateAffiliationComponents(separatedAffiliationComponents: Seq[String]): ListBuffer[(String, ListBuffer[String])] = {

        val separatedComponentsWithClasses: ListBuffer[(String, ListBuffer[String])] = new ListBuffer[(String, ListBuffer[String])]()

        for (separatedAffiliationComponent <- separatedAffiliationComponents) {
            separatedComponentsWithClasses.+=((separatedAffiliationComponent, getMatchedKeywordsForAffiliationComponent(Normalizer.normalize(separatedAffiliationComponent, Normalizer.Form.NFD).replaceAll("\\p{M}", "").toLowerCase)))
        }

        separatedComponentsWithClasses
    }

    def getUpdatedCategoriesForAffiliationComponents(authorNames: Seq[String], affiliationComponentsWithClasses: ListBuffer[(String, ListBuffer[String])]): ListBuffer[(String, ListBuffer[String])] = {

        val emailStrings: ListBuffer[String] = new ListBuffer[String]()
        var academicKeywordFound: Boolean = false
        var locationKeywordFound: Boolean = true
        var affiliationComponentIndex: Int = affiliationComponentsWithClasses.length - 1

        breakable {
            while (affiliationComponentIndex > 0) {
                if (EMAIL_SUFFIX_PATTERN.findFirstIn(affiliationComponentsWithClasses(affiliationComponentIndex)._1).getOrElse(BLANK).length.==(0)) {
                    affiliationComponentIndex -= 1
                }
                else {
                    emailStrings += affiliationComponentsWithClasses(affiliationComponentIndex)._1
                    affiliationComponentsWithClasses -= affiliationComponentsWithClasses(affiliationComponentIndex)
                    break
                }
            }
        }
        affiliationComponentIndex -= 1
        breakable {
            while (affiliationComponentIndex > 0) {
                if (affiliationComponentsWithClasses(affiliationComponentIndex)._2.isEmpty && isPresentInAuthors(affiliationComponentsWithClasses(affiliationComponentIndex)._1, authorNames)) {
                    emailStrings.prepend(affiliationComponentsWithClasses(affiliationComponentIndex)._1)
                    affiliationComponentsWithClasses -= affiliationComponentsWithClasses(affiliationComponentIndex)
                }
                else {
                    break
                }
                affiliationComponentIndex -= 1
            }

        }

        if (emailStrings.nonEmpty) {
            affiliationComponentsWithClasses.insert(affiliationComponentIndex + 1, (emailStrings.mkString(COMMA), ListBuffer[String](EMAIL_KEYWORD)))
        }

        affiliationComponentsWithClasses.foreach {
            affiliationComponentWithClass => {
                if (!academicKeywordFound && locationKeywordFound && affiliationComponentWithClass._2.isEmpty) {
                    affiliationComponentWithClass._2 += DEPARTMENT_KEYWORD
                }
                else if (academicKeywordFound && !locationKeywordFound && affiliationComponentWithClass._2.isEmpty) {
                    affiliationComponentWithClass._2 += ADDRESS_KEYWORD
                }

                else if (!academicKeywordFound && locationKeywordFound && affiliationComponentWithClass._2.nonEmpty) {
                    for (affiliationClass <- affiliationComponentWithClass._2) {
                        if (ACADEMIA_KEYWORDS.contains(affiliationClass) || COMPANY_KEYWORD.equals(affiliationClass)) {
                            academicKeywordFound = true
                            locationKeywordFound = false
                        }
                    }
                }

                else if (academicKeywordFound && !locationKeywordFound && affiliationComponentWithClass._2.nonEmpty) {
                    for (affiliationClass <- affiliationComponentWithClass._2) {
                        if (LOCATION_KEYWORDS.contains(affiliationClass) || EMAIL_KEYWORD.equals(affiliationClass)) {
                            locationKeywordFound = true
                            academicKeywordFound = false
                        }
                    }
                }

            }
        }


        affiliationComponentsWithClasses
    }

    def getBoundingBoxesForAffiliations(affiliationsWithClasses: Seq[(String, ListBuffer[String])], textReflows: Seq[TextReflow]): ListBuffer[(String, ListBuffer[String], LTBounds)] = {

        val affiliations: ListBuffer[(String, ListBuffer[String], LTBounds)] = new ListBuffer[(String, ListBuffer[String], LTBounds)]

        var affiliationIndex: Int = 0
        var textReflowIndex: Int = 0
        var startIndex: Int = 0

        var componentStartIndex: Int = 0
        var componentEndIndex: Int = textReflows(textReflowIndex).charAtoms().length

        while (affiliationIndex < affiliationsWithClasses.length && textReflowIndex < textReflows.length) {
            val separatedAffiliations = cleanPunctuations(affiliationsWithClasses(affiliationIndex)._1.split(SPACE_SEPARATOR))
            var affiliationComponentIndex: Int = 0
            while (affiliationComponentIndex < separatedAffiliations.length){
                val indices = getIndexesForComponents(component = separatedAffiliations(affiliationComponentIndex), textReflow = textReflows(textReflowIndex), indexRange = (componentStartIndex, componentEndIndex))
                if (indices._1 == -1) {
                    textReflowIndex += 1
                    componentStartIndex = 0
                    componentEndIndex = textReflows(textReflowIndex min (textReflows.length - 1)).charAtoms().length
                    affiliationComponentIndex -= 1
                }
                else {
                    componentStartIndex = indices._2
                    affiliations += ((separatedAffiliations(affiliationComponentIndex), affiliationsWithClasses(affiliationIndex)._2, getBoundingBoxesWithIndexesFromReflow(indexes = indices, textReflow = textReflows(textReflowIndex))))
                }
                affiliationComponentIndex += 1
            }
            affiliationIndex += 1
        }

        affiliations
    }


}
