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

  def getPdfCorpusConfig(appRoot: String): PdfCorpusConfig = {
    println(s"config init cwd = ${cwd}")

    val root = File(appRoot)

    val conf = ConfigFactory.parseFile(File(appRoot, "conf/application.conf").toJava)

    val config = conf.as[PdfCorpusConfig]("pdfCorpus")
    config.copy(
      rootDirectory = (root / config.rootDirectory).toString()
    )
  }


}
