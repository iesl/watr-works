package edu.umass.cs.iesl.watr


import java.io.{ InputStreamReader, Reader }
import better.files._, Cmds._
import scala.util.{Try, Failure, Success}
import org.jdom2


case class PdfCorpusConfig(
  rootDirectory: String
)


object configuration {
  import com.typesafe.config.ConfigFactory
  import net.ceedubs.ficus.Ficus._
  import net.ceedubs.ficus.readers.ArbitraryTypeReader._
  import net.ceedubs.ficus.Ficus.toFicusConfig
  // import net.ceedubs.ficus.readers.ValueReader

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

object Corpus {
  def apply(config: PdfCorpusConfig): Corpus = {
    new Corpus(config)
  }
}

class Corpus(
  val config: PdfCorpusConfig
) {

  // e.g., 3245.pdf, or sha1:afe23s...
  def entry(entryDescriptor: String): CorpusEntry = {
    new CorpusEntry(entryDescriptor, this)
  }

}

class CorpusEntry(
  entryDescriptor: String,
  corpus: Corpus
) {
  val artifactsRoot = File(corpus.config.rootDirectory) / s"${entryDescriptor}.d/"

  // e.g., cermine-zones.xml
  def getArtifact(artifactDescriptor: String): CorpusArtifact = {
    new CorpusArtifact(artifactDescriptor, this)
  }

}


class CorpusArtifact(
  descriptor: String,
  entry: CorpusEntry
) {

  def artifactPath = entry.artifactsRoot / descriptor
  def asReader: Try[Reader] = {
    val res: Option[Try[Reader]] = try {
      artifactPath
        .inputStream
        .map{is => Success(new InputStreamReader(is))}
        .headOption
    } catch {
      case t: Exception => Option(Failure[Reader](t))
    }

    res.getOrElse(sys.error(s"error getting corpus artifact ${artifactPath}"))

  }

  def asXml: Try[jdom2.Document] = {
    val res: Option[Try[jdom2.Document]] = try {
       artifactPath
        .inputStream
        .map({is => Success(new jdom2.input.SAXBuilder().build(is))})
        .headOption
    } catch {
      case t: Exception => Some(Failure(t))
    }

    res.getOrElse(sys.error(s"error getting corpus artifact ${artifactPath}"))
  }
}
