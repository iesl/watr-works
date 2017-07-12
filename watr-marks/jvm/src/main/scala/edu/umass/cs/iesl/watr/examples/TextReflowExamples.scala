package edu.umass.cs.iesl.watr
package examples

object TextReflowExamples extends ScalatagsDefs {
  import texttags._

  val corpus = new SampleTextCorpus()

  corpus.loadSampleDoc(2)


  val sc = sourcecode.Text {
    corpus.reportDocument(corpus.stableId)
  }

  val src = sc.source

  val result = <.pre(sc.value.toString())

}
