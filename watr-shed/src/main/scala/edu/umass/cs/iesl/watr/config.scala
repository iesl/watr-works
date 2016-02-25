package edu.umass.cs.iesl.watr


import com.typesafe.config.{Config, ConfigFactory}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.Ficus.toFicusConfig
import net.ceedubs.ficus.readers.ValueReader

case class PdfCorpusConfig(
  rootDirectory: String
)

import better.files._, Cmds._
object configuration {

  def getPdfCorpusConfig(): PdfCorpusConfig = {
    println(s"cwd = ${cwd}")

    val conf = ConfigFactory.parseFile(File("conf/application.conf").toJava)

    conf.as[PdfCorpusConfig]("pdfCorpus")
  }


}
