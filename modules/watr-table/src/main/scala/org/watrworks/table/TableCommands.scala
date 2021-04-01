package org.watrworks
package table

object ShellCommands extends  DocumentZoningApiEnrichments {

  // private[this] val log = org.log4s.getLogger

  // import apps.ProcessPipelineSteps._

  // def textExtractAll(n: Int=Int.MaxValue, skip: Int=0)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
  //   val conf = TextWorksConfig.Config(IOConfig(
  //     inputMode = Some(InputMode.CorpusFile(corpusAccessApi.corpus.corpusRoot.toNIO)),
  //     outputPath= None,
  //     overwrite = false
  //   ))

  //   val processStream = createInputStream[IO](conf.ioConfig)
  //     .drop(skip.toLong).take(n.toLong)
  //     .through(initMarkedInput())
  //     .through(cleanFileArtifacts(conf))
  //     .through(markUnextractedProcessables(conf))
  //     .through(runSegmentation(conf))
  //     .through(writeExtractedTextFile(conf))

  //   processStream.compile.drain
  //     .unsafeRunSync()

  // }

  // def segmentAll(n: Int=Int.MaxValue, skip: Int=0)(implicit corpusAccessApi: CorpusAccessApi): Unit = {
  //   val conf = TextWorksConfig.Config(IOConfig(
  //     inputMode = Some(InputMode.CorpusFile(corpusAccessApi.corpus.corpusRoot.toNIO)),
  //     outputPath= None,
  //     overwrite = true
  //   ))

  //   val processStream = createInputStream[IO](conf.ioConfig)
  //     .drop(skip.toLong).take(n.toLong)
  //     .through(initMarkedInput())
  //     .through(cleanDBArtifacts(conf))
  //     .through(cleanFileArtifacts(conf))
  //     .through(markUnextractedProcessables(conf))
  //     .through(runSegmentation(conf))
  //     .through(writeExtractedTextFile(conf))
  //     .through(importPaperIntoDB())

  //   processStream.compile.drain
  //     .unsafeRunSync()
  // }

}
