package edu.umass.cs.iesl.watr
package table  //;import acyclic.file


case class ImageArtifacts(
  artifactGroup: CorpusArtifactGroup
) {
  def rootPath = artifactGroup.rootPath

}


trait ImageArtifactEnrichments extends ImageManipulation {

  implicit class RicherImageArtifacts(val theImageArtifacts: ImageArtifacts)  {
    def getPages(): Seq[CorpusArtifact] = {
      theImageArtifacts.artifactGroup.getArtifacts
    }

    def getPage(p: Int): Option[CorpusArtifact] = {
      val pps = getPages()
      if (p < pps.length) Option(pps(p))
      else                None
    }
  }

}
