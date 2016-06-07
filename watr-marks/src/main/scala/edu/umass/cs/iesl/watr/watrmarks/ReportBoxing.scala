package edu.umass.cs.iesl.watr
package watrmarks

import utils.RelativeDirection

trait ReportBoxing {

  // val fontFullname = font.getFullFontName.map(_.mkString("[", ",", "]")).mkString(", ")
  // .mkString(s"\nPage:${page} file://${pdfArtifact.artifactPath}\n  ", "\n", "\n")

  // val SquareBracketCommaNoSpace


  def reportSeq[T](ts: Seq[T], direction: RelativeDirection, seps: String): TB.Box = {
    ts.mkString("[", ",", "]")

    ???

  }

}

object ReportBoxing extends ReportBoxing



trait ReportStrings {

  // (s"\n  ", "\n", "\n")

  // val SquareBracketCommaNoSpace


  def reportSeq[T](ts: Seq[T], direction: RelativeDirection, seps: String): TB.Box = {
    ts.mkString("[", ",", "]")

    ???

  }

}

object ReportStrings extends ReportStrings

