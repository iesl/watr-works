package edu.umass.cs.iesl.watr
package heuristics

import Constants._
import Utils._


import scala.collection.mutable.ListBuffer

object AffiliationsHeuristics {

    def getSeparateAffiliationComponentsByText(tokenizedTextReflow: ListBuffer[String]): ListBuffer[String] = {

        val separateComponents: ListBuffer[String] = ListBuffer[String]()
        val separateComponent: ListBuffer[String] = ListBuffer[String]()

        for (textReflowToken <- tokenizedTextReflow) {
            if( PUNCTUATION_SEPARATORS.contains(textReflowToken.takeRight(n = 1))){
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

        for(separateComponent <- separateComponents if separateComponent.length.==(0)){
            separateComponents -= separateComponent
        }

        var currentComponentIndex: Int = 0

        while(currentComponentIndex < separateComponents.length){
            val currentComponent = separateComponents(currentComponentIndex)
            for(conjunction <- WORD_SEPARATORS if currentComponent.split(SPACE_SEPARATOR).length.==(3) && currentComponent.split(conjunction).length.==(2)){
                separateComponents(currentComponentIndex - 1) = separateComponents.slice(currentComponentIndex-1, currentComponentIndex+1).mkString(COMMA.concat(SPACE_SEPARATOR))
                separateComponents -= currentComponent
            }
            currentComponentIndex += 1
        }
        separateComponents
    }

    def getCategoryForSeparateAffiliationComponents(separatedAffiliationComponents: ListBuffer[String]): ListBuffer[(String, ListBuffer[String])] = {

        val separatedComponentsWithClasses: ListBuffer[(String, ListBuffer[String])] = new ListBuffer[(String, ListBuffer[String])]()

        for(separatedAffiliationComponent <- separatedAffiliationComponents){
            separatedComponentsWithClasses.+=((separatedAffiliationComponent, getMatchedKeywordsForAffiliationComponent(separatedAffiliationComponent.toLowerCase)))
        }

        separatedComponentsWithClasses
    }


}
