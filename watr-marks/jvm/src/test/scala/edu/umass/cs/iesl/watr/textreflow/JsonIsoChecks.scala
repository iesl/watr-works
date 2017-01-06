package edu.umass.cs.iesl.watr
package textreflow

import org.scalacheck._
import scalaz._, Scalaz._
import org.scalacheck.Prop._

import matryoshka._
import matryoshka.scalacheck.arbitrary._

import geometry._

import ComponentTypeEnrichments._
import TextReflowJsonCodecs._

import TextReflowF._

object JsonIsoChecks extends Properties("JsonIsoChecks") with ArbitraryTextReflows {
  import play.api.libs.json._
  import TextReflowTransforms._


  def showFailed[A: Equal](a1: A, a2: A): Unit = {
    if (a1 =/= a2) {
      println("mismatch, a1: ")
      println(a1)
      println("a2: ")
      println(a2)
    }
  }

  property("json <--> LTBounds") = forAll{ (example: LTBounds) =>
    val jsVal = Json.toJson(example)
    val jsOut = Json.prettyPrint(jsVal)
    jsVal.validate[LTBounds] match   {
      case JsSuccess(ltb, path) =>
        example === ltb
      case _ => false
    }
  }


  property("json <--> TargetRegion") = forAll{ (example: TargetRegion) =>
    val jsVal = Json.toJson(example)
    val jsOut = Json.prettyPrint(jsVal)
    jsVal.validate[TargetRegion] match   {
      case JsSuccess(targetRegion, path) =>
        example === targetRegion
      case _ => false
    }
  }

  property("json <--> CharAtom") = forAll{ (example: CharAtom) =>
    val jsVal = Json.toJson(example)
    val jsOut = Json.prettyPrint(jsVal)
    jsVal.validate[CharAtom] match   {
      case JsSuccess(pageAtom, path) =>
        example === pageAtom
      case _ => false
    }
  }


  property("json <--> textReflow isomorphism") = forAll{ (textReflowEx: TextReflow) =>
    val asJson = textReflowEx.toJson()
    val textReflow = jsonToTextReflow(asJson)
    textReflowEx === textReflow
  }


}
