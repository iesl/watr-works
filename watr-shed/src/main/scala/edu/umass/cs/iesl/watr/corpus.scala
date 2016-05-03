package edu.umass.cs.iesl.watr


import java.io.{ FileInputStream, InputStream, InputStreamReader, Reader }
import better.files._, Cmds._
import play.api.libs.json
import scala.util.{Try, Failure, Success}
import org.jdom2
// import scalaz.{Tag, @@}


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

  def entries(): Seq[CorpusEntry] = {
    val root = File(config.rootDirectory)
    val es = root.glob("**/*.d").toList
    val sorted = es.sortBy(_.name)

    val artifacts = sorted.map{e =>
      new CorpusEntry(e.name, this)
    }

    artifacts.filterNot(_.getArtifacts.isEmpty)
  }

}

sealed trait ArtifactDescriptor

class CorpusEntry(
  entryDescriptor: String,
  corpus: Corpus
) {
  val artifactsRoot = File(corpus.config.rootDirectory) / entryDescriptor
  val entryDescriptorRoot = entryDescriptor.dropRight(2)

  def getArtifacts(): Seq[String] = {
    val allFiles = artifactsRoot.glob("*").toSeq

    allFiles.map(_.name)
  }

  // e.g., cermine-zones.xml
  def getArtifact(artifactDescriptor: String): CorpusArtifact = {
    new CorpusArtifact(artifactDescriptor, this)
  }

  def getSvgArtifact(): CorpusArtifact = {
    new CorpusArtifact(s"${entryDescriptorRoot}.svg", this)
  }

}


class CorpusArtifact(
  descriptor: String,
  entry: CorpusEntry
) {

  def artifactPath = entry.artifactsRoot / descriptor

  def asFile: Try[File] = Success(artifactPath)

  def asInputStream: Try[InputStream] = {
    val fis = new FileInputStream(artifactPath.toJava)
    Success(fis)
  }

  def asReader: Try[Reader] = {
    val fis = new FileInputStream(artifactPath.toJava)
    val fisr = new InputStreamReader(fis)

    Success(fisr)
  }

  def asJson: Try[json.JsValue] = {
    val res: Try[json.JsValue] = try {
      asInputStream
        .map(json.Json.parse(_))
    } catch {
      case t: Exception => Failure(t)
    }
    res
  }

  def asXml: Try[jdom2.Document] = {
    val res: Try[jdom2.Document] = try {
      asInputStream
        .map(is => new jdom2.input.SAXBuilder().build(is))
    } catch {
      case t: Exception => Failure(t)
    }

    res
  }
}
