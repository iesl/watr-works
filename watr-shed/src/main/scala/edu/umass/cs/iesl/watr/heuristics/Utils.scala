package edu.umass.cs.iesl.watr.heuristics

import Constants._

import scala.collection.mutable.ListBuffer

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

    def mergeConsecutiveNameTokens(authorNameTokens: ListBuffer[String]): ListBuffer[String]= {

        val mergedAuthorNameTokens: ListBuffer[String] = ListBuffer[String]()
        var authorNameTokenIndex: Int = 0

        while(authorNameTokenIndex < authorNameTokens.length-1){
            if(authorNameTokens(authorNameTokenIndex+1).==(PERIOD)){
                mergedAuthorNameTokens += authorNameTokens(authorNameTokenIndex).concat(authorNameTokens(authorNameTokenIndex+1))
                authorNameTokenIndex += 2
            }
            else{
                mergedAuthorNameTokens += authorNameTokens(authorNameTokenIndex)
                authorNameTokenIndex += 1
            }
        }

        if(authorNameTokenIndex == authorNameTokens.length-1){
            mergedAuthorNameTokens += authorNameTokens(authorNameTokenIndex)
        }
        mergedAuthorNameTokens
    }
}
