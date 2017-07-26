package edu.umass.cs.iesl.watr
package heuristics

import geometry.LTBounds
import Constants._
import textreflow.TextReflowF.TextReflow
import textreflow.data._
// import simstring._
import utils.ExactFloats._
import TypeTags._
import geometry.{CharAtom, LTBounds}
import geometry.syntax._
import Constants._
import textreflow.TextReflowF.TextReflow
import textreflow.data._

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.control.Breaks._

object Utils {

    def isOfNameInitialFormat(authorNameComponent: String): Boolean = {

        if (NAME_INITIAL_FORMAT_PATTERN.findFirstIn(authorNameComponent).getOrElse(BLANK).length.==(authorNameComponent.length)) {
            return true
        }
        false
    }

    def getNextComponentIndices(currentSeparateComponentIndex: Int, currentComponentIndex: Int, currentComponent: String): (Int, Int) = {

        if (currentSeparateComponentIndex + 1 == currentComponent.split(SPACE_SEPARATOR).length) {
            return (0, currentComponentIndex + 1)
        }
        (currentSeparateComponentIndex + 1, currentComponentIndex)
    }

    def getStartIndexAfterComma(currentComponent: String): Int = {

        var indexAfterComma: Int = currentComponent.indexOf(COMMA)

        while (!currentComponent.charAt(indexAfterComma).isLetter) {
            indexAfterComma += 1
        }

        indexAfterComma
    }

    def getReflowString(textReflow: TextReflow): String = {
        textReflow.charAtoms().map {
            charAtom => charAtom.char
        }.mkString
    }

    def getIndexesForComponents(component: String, textReflow: TextReflow, indexRange: (Int, Int)): (Int, Int) = {

        var componentIndex: Int = 0
        var textReflowIndex: Int = indexRange._1
        var componentStartIndex: Int = -1
        val yPosition: Int = getYPosition(textReflow = textReflow)

        while (componentIndex < component.length && textReflowIndex < indexRange._2) {
            val currentTextReflowAtom: CharAtom = textReflow.charAtoms()(textReflowIndex)
            var localReflowCharIndex: Int = 0
            breakable {
                while (localReflowCharIndex < currentTextReflowAtom.char.length) {
                    if (component.charAt(componentIndex).==(currentTextReflowAtom.char.charAt(localReflowCharIndex))) {
                        if (componentStartIndex.==(-1)) {
                            componentStartIndex = textReflowIndex
                        }
                        componentIndex += 1
                    }
                    else if (PUNCTUATION_SEPARATORS.contains(currentTextReflowAtom.char.charAt(localReflowCharIndex).toString) || currentTextReflowAtom.bbox.top.asInt().!=(yPosition)){
                        break
                    }
                    else {
                        componentIndex = 0
                        componentStartIndex = -1
                    }
                    localReflowCharIndex += 1
                }
            }
            textReflowIndex += 1
        }

        (componentStartIndex, textReflowIndex)
    }

    def getBoundingBoxesWithIndexesFromReflow(indexes: (Int, Int), textReflow: TextReflow): LTBounds = {
        LTBounds(textReflow.charAtoms()(indexes._1).bbox.left, textReflow.charAtoms()(indexes._1).bbox.top,
            textReflow.charAtoms()(indexes._2 - 1).bbox.right - textReflow.charAtoms()(indexes._1).bbox.left, textReflow.charAtoms()(indexes._1).bbox.height)
    }

    def isLetterString(charSequence: String): Boolean = {
        charSequence.foreach {
            character => {
                if (!character.isLetter) return false
            }
        }
        true
    }

    def isNumberString(charSequence: String): Boolean = {
        charSequence.foreach {
            character => {
                if (!character.isDigit) return false
            }
        }
        true
    }

    def getYPosition(textReflow: TextReflow): Int = {

        val yPositionCounts: scala.collection.mutable.Map[Int, Int] = scala.collection.mutable.Map[Int, Int]()

        for (charAtom <- textReflow.charAtoms()) {
            if (yPositionCounts.get(charAtom.bbox.top.asInt()).isEmpty) {
                yPositionCounts += (charAtom.bbox.top.asInt() -> 0)
            }
            yPositionCounts.update(charAtom.bbox.top.asInt(), yPositionCounts(charAtom.bbox.top.asInt()) + 1)

        }

        val maxRecord: (Int, Int) = yPositionCounts.maxBy(_._2)
        if (maxRecord._2 > 1) {
            return maxRecord._1
        }
        -1
    }

    def getMajorityHeight(textReflow: TextReflow): Int = {
        val heightCounts: scala.collection.mutable.Map[Int, Int] = scala.collection.mutable.Map[Int, Int]()

        for (charAtom <- textReflow.charAtoms()) {
            if (heightCounts.get(charAtom.bbox.height.asInt()).isEmpty) {
                heightCounts += (charAtom.bbox.height.asInt() -> 0)
            }
            heightCounts.update(charAtom.bbox.height.asInt(), heightCounts(charAtom.bbox.height.asInt()) + 1)

        }

        val maxRecord: (Int, Int) = heightCounts.maxBy(_._2)
        if (maxRecord._2 > 1) {
            return maxRecord._1
        }
        -1
    }

    def getMatchedKeywordsForAffiliationComponent(affiliationComponent: String): ListBuffer[String] = {

        val matchedKeywords: ListBuffer[String] = new ListBuffer[String]()

        if (EMAIL_PATTERN.findFirstIn(affiliationComponent).getOrElse(BLANK).length.!=(0)) {
            matchedKeywords += EMAIL_KEYWORD
        }

        if (matchedKeywords.isEmpty) {
            breakable {
                RESOURCE_KEYWORDS.foreach {
                    resource => {
                        for (line <- Source.fromInputStream(getClass.getResourceAsStream(resource._2)).getLines) {
                            if ("\\b".concat(line).concat("\\b").r.findFirstIn(affiliationComponent).isDefined) {
                                matchedKeywords += resource._1
                                break
                            }
                        }
                    }
                }
            }
        }

        matchedKeywords

    }

    def cleanSeparatedComponent(separatedComponent: String): String = {

        var cleanedComponent: String = separatedComponent

        cleanedComponent = separatedComponent.substring(CLEANUP_PATTERN.findFirstIn(separatedComponent).getOrElse(BLANK).length, separatedComponent.length)

        if (separatedComponent.takeRight(1).equals(PERIOD)) {
            cleanedComponent = separatedComponent.dropRight(1)
        }
        if (cleanedComponent.head.==(DOT)) {
            cleanedComponent = separatedComponent.tail
        }

        cleanedComponent.trim

    }

    def getMatchedZipCodePatterns(affiliationComponent: String): ListBuffer[String] = {

        val matchedZipCodePatterns: ListBuffer[String] = new ListBuffer[String]()

        ZIP_CODE_PATTERNS.foreach {
            zipCodePattern => {
                zipCodePattern._2.foreach {
                    zipCodeRegex => {
                        if (zipCodeRegex.findFirstIn(affiliationComponent).getOrElse(BLANK).nonEmpty) {
                            matchedZipCodePatterns += zipCodePattern._1
                        }
                    }
                }
            }
        }

        matchedZipCodePatterns

    }

    def isPresentInAuthors(component: String, authorNames: ListBuffer[NameWithBBox]): Boolean = {

        for (authorName <- authorNames) {
            if (component.contains(authorName.lastName.componentText.toLowerCase)) {
                return true
            }
            if (! isOfNameInitialFormat(authorName.middleName.componentText) && component.contains(authorName.middleName.componentText.toLowerCase)) {
                return true
            }
            if (! isOfNameInitialFormat(authorName.firstName.componentText) && component.contains(authorName.firstName.componentText.toLowerCase)) {
                return true
            }
        }
        false
    }

    def cleanPunctuations(tokens: Seq[String]): ListBuffer[String] = {

        val cleanedTokens: ListBuffer[String] = new ListBuffer[String]()

        tokens.foreach {
            token => {
                token.split(PUNCTUATIONS_PATTERN).foreach {
                    splitToken => {
                        if (PUNCTUATIONS.contains(splitToken.head)) {
                            cleanedTokens += splitToken.head.toString
                            cleanedTokens += splitToken.tail
                        }
                        else {
                            cleanedTokens += splitToken
                        }
                    }
                }
            }
        }
        cleanedTokens.map(cleanedToken => cleanedToken.trim).filter(_.nonEmpty)

    }

    def containsPattern(reflowString: String, patternSeq: Seq[String]): Boolean = {

        for (pattern <- patternSeq) {
            if (reflowString.contains(pattern)) {
                return true
            }
        }

        false
    }

    def getBoundingBoxesForComponents(components: Seq[String], textReflow: TextReflow): ListBuffer[(String, LTBounds)] = {

        var componentStartIndex: Int = 0
        var componentEndIndex: Int = textReflow.charAtoms().length - 1
        var indices: (Int, Int) = (0, textReflow.charAtoms().length - 1)

        val componentsWithBoundingBoxes: ListBuffer[(String, LTBounds)] = new ListBuffer[(String, LTBounds)]()

        for (component <- components) {
            if (!LEXICON_TAGS.contains(component)) {
                if (componentEndIndex.!=(textReflow.charAtoms().length - 1)) {
                    indices = getIndexesForComponents(component = component, textReflow = textReflow, indexRange = (componentEndIndex, textReflow.charAtoms().length))
                }
                else {
                    indices = getIndexesForComponents(component = component, textReflow = textReflow, indexRange = (componentStartIndex, textReflow.charAtoms().length))
                }

                componentStartIndex = indices._1
                componentEndIndex = indices._2
                componentsWithBoundingBoxes.+=((component, getBoundingBoxesWithIndexesFromReflow(indexes = (componentStartIndex, componentEndIndex), textReflow = textReflow)))

            }
        }
        componentsWithBoundingBoxes
    }

    def getBoundingBoxAsString(bBox: LTBounds): String = {
        val bBoxAsString: ListBuffer[String] = new ListBuffer[String]()

        bBoxAsString.+=(bBox.left.asInt().toString)
        bBoxAsString.+=(bBox.top.asInt().toString)
        bBoxAsString.+=(bBox.right.asInt().toString)
        bBoxAsString.+=(bBox.bottom.asInt().toString)

        bBoxAsString.mkString(BOUNDING_BOX_SEPARATOR)
    }

    def getTextReflowLengthFromIndices(textReflow: TextReflow, indexRange: (Int, Int)): Int = {

        var reflowLength: Int = 0
        var reflowAtomIndex: Int = indexRange._1

        while (reflowAtomIndex < indexRange._2) {
            reflowLength += textReflow.charAtoms()(reflowAtomIndex).char.length
            reflowAtomIndex += 1
        }

        reflowLength
    }


}
