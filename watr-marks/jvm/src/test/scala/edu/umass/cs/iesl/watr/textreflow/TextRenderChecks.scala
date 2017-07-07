package edu.umass.cs.iesl.watr
package textreflow

import org.scalacheck._
import scalaz._, Scalaz._
import org.scalacheck.Prop._

import matryoshka._
import matryoshka.scalacheck.arbitrary._

import TextReflowF._

object TextRenderChecks extends Properties("TextRenderChecks") with ArbitraryTextReflows {

  new corpora.MemDocZoningApi {

    property("split/join is identity") = forAll{ (textReflowEx: TextReflow) =>
      val asJson = textReflowToJson(textReflowEx)
      val textReflow = jsonToTextReflow(asJson)
      textReflowEx === textReflow
    }


    property("mask/unmask...") = forAll{ (textReflowEx: TextReflow) => true }

  }
}
