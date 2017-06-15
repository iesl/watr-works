package edu.umass.cs.iesl.watr
package heuristics

import edu.umass.cs.iesl.watr.TypeTags._
import edu.umass.cs.iesl.watr.geometry.LTBounds
import edu.umass.cs.iesl.watr.heuristics.Constants._
import edu.umass.cs.iesl.watr.textreflow.TextReflowF.TextReflow
import edu.umass.cs.iesl.watr.textreflow.data._

import scala.util.matching.Regex

object Utils {

    def isOfFirstNameInitialFormat(authorNameComponent: String): Boolean = {

        val nameInitialPattern: Regex = """^[A-Z\.]+$""".r
        val namePatternMatch = nameInitialPattern.findFirstIn(authorNameComponent)
        if(namePatternMatch.isDefined && namePatternMatch.get.length.==(authorNameComponent.length)){
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

        while (componentIndex < component.length && textReflowIndex < indexRange._2) {
            val currentTextReflowChar: String = textReflow.charAtoms()(textReflowIndex).char
            var localReflowCharIndex: Int = 0
            while (localReflowCharIndex < currentTextReflowChar.length) {
                if (component.charAt(componentIndex).==(currentTextReflowChar.charAt(localReflowCharIndex))) {
                    if (componentStartIndex.==(-1)) {
                        componentStartIndex = textReflowIndex
                    }
                    componentIndex += 1
                }
                else {
                    componentIndex = 0
                    componentStartIndex = -1
                }
                localReflowCharIndex += 1
            }
            textReflowIndex += 1
        }

        (componentStartIndex, textReflowIndex)
    }

    def getBoundingBoxesWithIndexesFromReflow(indexes: (Int, Int), textReflow: TextReflow): LTBounds = {
        LTBounds(FloatRep(textReflow.charAtoms()(indexes._1).bbox.left.asInstanceOf[Int]), FloatRep(textReflow.charAtoms()(indexes._1).bbox.top.asInstanceOf[Int]),
            FloatRep(textReflow.charAtoms()(indexes._2 - 1).bbox.right.asInstanceOf[Int] - textReflow.charAtoms()(indexes._1).bbox.left.asInstanceOf[Int]), FloatRep(textReflow.charAtoms()(indexes._1).bbox.height.asInstanceOf[Int]))
    }

    def getBoundingBoxesForComponents(name: NameWithBBox, geometricallySeparatedName: String, textReflow: TextReflow): NameWithBBox = {

        val (nameStartIndex, nameEndIndex) = getIndexesForComponents(component = geometricallySeparatedName.replace(SPACE_SEPARATOR, BLANK), textReflow = textReflow, (0, textReflow.charAtoms().length))
        name.bbox = getBoundingBoxesWithIndexesFromReflow((nameStartIndex, nameEndIndex), textReflow)
        if (name.firstName.nameText.nonEmpty) {
            val (firstNameStartIndex, firstNameEndIndex) = getIndexesForComponents(component = name.firstName.nameText.replace(SPACE_SEPARATOR, BLANK), textReflow = textReflow, (nameStartIndex, nameEndIndex))
            name.firstName.bbox = getBoundingBoxesWithIndexesFromReflow((firstNameStartIndex, firstNameEndIndex), textReflow)
        }
        if (name.middleName.nameText.nonEmpty) {
            val (middleNameStartIndex, middleNameEndIndex) = getIndexesForComponents(component = name.middleName.nameText.replace(SPACE_SEPARATOR, BLANK), textReflow = textReflow, (nameStartIndex, nameEndIndex))
            name.middleName.bbox = getBoundingBoxesWithIndexesFromReflow((middleNameStartIndex, middleNameEndIndex), textReflow)
        }
        if (name.lastName.nameText.nonEmpty) {
            val (lastNameStartIndex, lastNameEndIndex) = getIndexesForComponents(component = name.lastName.nameText.replace(SPACE_SEPARATOR, BLANK), textReflow = textReflow, (nameStartIndex, nameEndIndex))
            name.lastName.bbox = getBoundingBoxesWithIndexesFromReflow((lastNameStartIndex, lastNameEndIndex), textReflow)
        }
        name
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

        for(charAtom <- textReflow.charAtoms()){
            if(yPositionCounts.get(charAtom.bbox.top.asInstanceOf[Int]).isEmpty){
                yPositionCounts += (charAtom.bbox.top.asInstanceOf[Int] -> 0)
            }
            yPositionCounts.update(charAtom.bbox.top.asInstanceOf[Int], yPositionCounts(charAtom.bbox.top.asInstanceOf[Int])+1)

        }

        if(yPositionCounts(yPositionCounts.maxBy(_._2)._1) > 1){
            return yPositionCounts.maxBy(_._2)._1
        }
        -1
    }
}
