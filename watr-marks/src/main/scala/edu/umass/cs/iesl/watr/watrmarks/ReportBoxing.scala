package edu.umass.cs.iesl.watr
package watrmarks




trait ReportBoxing {
  import textboxing.TextBoxing
  import TextBoxing._

  // val fontFullname = font.getFullFontName.map(_.mkString("[", ",", "]")).mkString(", ")
  // .mkString(s"\nPage:${page} file://${pdfArtifact.artifactPath}\n  ", "\n", "\n")

  // val SquareBracketCommaNoSpace


  def reportSeq[T](ts: Seq[T], direction: RelativeDirection, seps: String): Box = {
    ts.mkString("[", ",", "]")

    ???

  }

}

object ReportBoxing extends ReportBoxing



trait ReportStrings {
  import textboxing.TextBoxing
  import TextBoxing._

  // (s"\n  ", "\n", "\n")

  // val SquareBracketCommaNoSpace


  def reportSeq[T](ts: Seq[T], direction: RelativeDirection, seps: String): Box = {
    ts.mkString("[", ",", "]")

    ???

  }

}

object ReportStrings extends ReportStrings

