package edu.umass.cs.iesl.watr.heuristics

import edu.umass.cs.iesl.watr.textreflow.data.TextReflow
import edu.umass.cs.iesl.watr.textreflow.data._
import util.control.Breaks._

import scala.collection.mutable.ListBuffer

object AuthorNameHeuristics {

    def tokenizeAuthorNames(authorTextReflow: TextReflow): ListBuffer[String] ={
        val authorNamesTokens : ListBuffer[String] = ListBuffer[String]()
        val authorName : ListBuffer[Char] = ListBuffer[Char]()

        val spaceThreshold: Int = 200

        var prevCharPosition: Int = -1
        var yPosition: Int = -1

        for (charAtom <- authorTextReflow.charAtoms()){
            val currentCharacter = charAtom.char.toCharArray.head
            if(currentCharacter.isLetter && yPosition.==(-1)){
                yPosition = charAtom.bbox.top.asInstanceOf[Int]
                if(prevCharPosition.==(-1)){
                    authorName += currentCharacter
                }
            }
            if(prevCharPosition > 0){
                val spaceBetweenChars = charAtom.bbox.left.asInstanceOf[Int] - prevCharPosition
                if(spaceBetweenChars < spaceThreshold && charAtom.bbox.top.==(yPosition)){
                    authorName += currentCharacter
                }
                else if(spaceBetweenChars >= spaceThreshold){
                    authorNamesTokens += authorName.mkString
                    authorName.clear()
                    if(charAtom.bbox.top.==(yPosition)){
                        authorName += currentCharacter
                    }
                }
            }
            prevCharPosition = charAtom.bbox.left.asInstanceOf[Int] + charAtom.bbox.width.asInstanceOf[Int]
        }

        if(authorName.nonEmpty){
            authorNamesTokens += authorName.mkString
            authorName.clear()
        }

        authorNamesTokens
    }

    def isOfFirstNameInitialFormat(authorNameComponent: String): Boolean ={

        if(authorNameComponent.head.isUpper && authorNameComponent.charAt(1).==('.')){
            return true
        }
        false
    }

    def getSeparateAuthorNamesByText(authorNamesTokens: ListBuffer[String]): ListBuffer[String] ={

        val separateAuthorNames : ListBuffer[String] = ListBuffer[String]()
        val separateAuthorName : ListBuffer[String] = ListBuffer[String]()

        var checkNextFlag: Boolean = false

        for(authorNameToken <- authorNamesTokens){
            breakable{
                if(authorNameToken.equalsIgnoreCase("and") || authorNameToken.equalsIgnoreCase("&")){
                    separateAuthorNames += separateAuthorName.mkString(" ")
                    separateAuthorName.clear()
                    if(checkNextFlag){
                        checkNextFlag = false
                    }
                }
                else if(!checkNextFlag && authorNameToken.takeRight(n = 1).equals(",")){
                    separateAuthorName += authorNameToken.replace(",", "")
                    checkNextFlag = true
                    break
                }
                else if(checkNextFlag){
                    if(isOfFirstNameInitialFormat(authorNameToken)){
                        separateAuthorName += authorNameToken.replace(",", "")
                        separateAuthorNames += separateAuthorName.mkString(" ")
                        separateAuthorName.clear()
                    }
                    else {
                        separateAuthorNames += separateAuthorName.mkString(" ")
                        separateAuthorName.clear()
                        separateAuthorName += authorNameToken
                    }
                    checkNextFlag = false
                }
                else{
                    separateAuthorName += authorNameToken
                }

            }

        }

        if(separateAuthorName.nonEmpty){
            separateAuthorNames += separateAuthorName.mkString(" ")
        }
        separateAuthorNames
    }

    def getSeparateAuthorNamesByGeometry(authorNamesTokens: ListBuffer[String]): ListBuffer[String] ={

    }
}
