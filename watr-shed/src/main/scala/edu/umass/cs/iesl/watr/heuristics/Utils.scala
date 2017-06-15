package edu.umass.cs.iesl.watr
package heuristics

import edu.umass.cs.iesl.watr.TypeTags._
import edu.umass.cs.iesl.watr.geometry.LTBounds
import edu.umass.cs.iesl.watr.heuristics.Constants._
import edu.umass.cs.iesl.watr.textreflow.TextReflowF.TextReflow
import edu.umass.cs.iesl.watr.textreflow.data._

object Utils {

    def isOfFirstNameInitialFormat(authorNameComponent: String): Boolean = {

        if (authorNameComponent.length == 3 && authorNameComponent.head.isUpper && authorNameComponent.charAt(1).==(DOT) && authorNameComponent.charAt(2).==(COMMA_CHARACTER)) {
            return true
        }
        false
    }

    def getNextNameAndComponentIndices(currentAuthorNameComponentIndex: Int, currentAuthorNameIndex: Int, currentAuthorName: String): (Int, Int) = {

        if (currentAuthorNameComponentIndex + 1 == currentAuthorName.split(NAME_SEPARATOR).length) {
            return (0, currentAuthorNameIndex + 1)
        }
        (currentAuthorNameComponentIndex + 1, currentAuthorNameIndex)
    }

    def getStartIndexAfterComma(authorName: String): Int = {

        var indexAfterComma: Int = authorName.indexOf(COMMA)

        while (!authorName.charAt(indexAfterComma).isLetter) {
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

        val (nameStartIndex, nameEndIndex) = getIndexesForComponents(component = geometricallySeparatedName.replace(NAME_SEPARATOR, BLANK), textReflow = textReflow, (0, textReflow.charAtoms().length))
        name.bbox = getBoundingBoxesWithIndexesFromReflow((nameStartIndex, nameEndIndex), textReflow)
        if (name.firstName.nameText.nonEmpty) {
            val (firstNameStartIndex, firstNameEndIndex) = getIndexesForComponents(component = name.firstName.nameText.replace(NAME_SEPARATOR, BLANK), textReflow = textReflow, (nameStartIndex, nameEndIndex))
            name.firstName.bbox = getBoundingBoxesWithIndexesFromReflow((firstNameStartIndex, firstNameEndIndex), textReflow)
        }
        if (name.middleName.nameText.nonEmpty) {
            val (middleNameStartIndex, middleNameEndIndex) = getIndexesForComponents(component = name.middleName.nameText.replace(NAME_SEPARATOR, BLANK), textReflow = textReflow, (nameStartIndex, nameEndIndex))
            name.middleName.bbox = getBoundingBoxesWithIndexesFromReflow((middleNameStartIndex, middleNameEndIndex), textReflow)
        }
        if (name.lastName.nameText.nonEmpty) {
            val (lastNameStartIndex, lastNameEndIndex) = getIndexesForComponents(component = name.lastName.nameText.replace(NAME_SEPARATOR, BLANK), textReflow = textReflow, (nameStartIndex, nameEndIndex))
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
}
