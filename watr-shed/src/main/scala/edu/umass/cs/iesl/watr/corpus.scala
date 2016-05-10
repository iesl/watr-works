package edu.umass.cs.iesl.watr

import java.io.{ InputStream }
import java.io.InputStreamReader
import java.io.Reader
import java.nio.{file => nio}
import play.api.libs.json
import scala.util.{Try, Failure, Success}
import org.jdom2
import scalaz.{Tag, @@}
import ammonite.ops._


sealed trait SHA1String
case class HeaderInfo(
  title: String,
  authors: String
)

case class CorpusEntryMetadata(
  pdfSha1: String@@SHA1String,
  filenames: Seq[String],
  urls: Seq[String],
  headers: Option[HeaderInfo]
)

trait CorpusJsonFormats {
  import play.api.libs.json
  import json._

  implicit def FormatSHA1String  = Format(
    __.read[String].map(i => Tag.of[SHA1String](i)),
    Writes[String@@SHA1String](i => JsString(SHA1String.unwrap(i)))
  )

  implicit def HeaderInfoFormat = Json.format[HeaderInfo]
  implicit def CorpusEntryMetadataFormat = Json.format[CorpusEntryMetadata]
}

object corpusFormats extends CorpusJsonFormats


object Corpus {
  // import com.typesafe.config.ConfigFactory
  // import net.ceedubs.ficus.Ficus._
  // import net.ceedubs.ficus.readers.ArbitraryTypeReader._
  // import net.ceedubs.ficus.Ficus.toFicusConfig

  // def apply(config: PdfCorpusConfig): Corpus = {
  //   new Corpus(config)
  // }

  def apply(appRoot: nio.Path): Corpus = {
    new Corpus(Path(appRoot))
  }

  def apply(appRoot: Path): Corpus = {
    new Corpus(appRoot)
  }


  // def getPdfCorpusConfig(appRoot: String): PdfCorpusConfig = {
  //   println(s"config init cwd = ${cwd}")

  //   val root = File(appRoot)

  //   val conf = ConfigFactory.parseFile(File(appRoot, "conf/application.conf").toJava)

  //   val config = conf.as[PdfCorpusConfig]("pdfCorpus")
  //   config.copy(
  //     rootDirectory = (root / config.rootDirectory).toString()
  //   )
  // }

}


class Corpus(
  val corpusRoot: Path
) {

  override val toString = {
    s"corpus:${corpusRoot}"
  }

  // e.g., 3245.pdf, or sha1:afe23s...
  def entry(entryDescriptor: String): CorpusEntry = {
    new CorpusEntry(entryDescriptor, this)
  }

  def entries(): Seq[CorpusEntry] = {
    val artifacts = ls.!(corpusRoot)
      .map { _.name }
      .sorted
      .map{ new CorpusEntry(_, this) }

    artifacts.filterNot(_.getArtifacts.isEmpty)
  }

}

sealed trait ArtifactDescriptor

// object CorpusEntry  {}

class CorpusEntry(
  val entryDescriptor: String,
  val corpus: Corpus
) extends CorpusJsonFormats {

  override val toString = {
    s"${corpus}/${entryDescriptor}"
  }

  val artifactsRoot = corpus.corpusRoot / entryDescriptor
  val entryDescriptorRoot = entryDescriptor.dropRight(2)

  def getArtifacts(): Seq[String] = {
    val allFiles = ls! artifactsRoot map{ pdf =>
      pdf
    }

    allFiles.map(_.name)
  }

  def putArtifact(artifactDescriptor: String, content: String): CorpusArtifact = {
    val outputPath = artifactsRoot/artifactDescriptor
    write(outputPath, content)
    new CorpusArtifact(artifactDescriptor, this)
  }

  // e.g., cermine-zones.xml
  def getArtifact(artifactDescriptor: String): CorpusArtifact = {
    new CorpusArtifact(artifactDescriptor, this)
  }
  def deleteArtifact(artifactDescriptor: String): Unit = {
    new CorpusArtifact(artifactDescriptor, this).delete
  }

  def hasArtifact(artifactDescriptor: String): Boolean ={
    getArtifact(artifactDescriptor).exists


  }

  def getPdfArtifact(): CorpusArtifact =
    new CorpusArtifact(s"${entryDescriptorRoot}", this)

  def getSvgArtifact(): CorpusArtifact = {
    new CorpusArtifact(s"${entryDescriptorRoot}.svg", this)
  }

  def updateMetadata(m: Option[CorpusEntryMetadata] = None): CorpusEntryMetadata = {
    new CorpusArtifact("metadata.inf", this).asJson.map {
      _.validate[CorpusEntryMetadata]
    }

    ???
  }

}


class CorpusArtifact(
  val artifactDescriptor: String,
  val entry: CorpusEntry
) {
  override val toString = {
    s"${entry}/${artifactDescriptor}"
  }

  def exists: Boolean = {
    ammonite.ops.exists! artifactPath
  }

  def delete: Unit = {
    ammonite.ops.rm! artifactPath
  }

  def artifactPath = entry.artifactsRoot / artifactDescriptor

  def asPath: Try[Path] = Success(artifactPath)

  def asInputStream: Try[InputStream] = {
    val fis = nio.Files.newInputStream(artifactPath.toNIO)
    Success(fis)
  }

  def asReader: Try[Reader] = {
    asInputStream.map(new InputStreamReader(_))
  }

  def asJson: Try[json.JsValue] = try {
    asInputStream.map(json.Json.parse(_))
  } catch {
    case t: Exception => Failure(t)
  }

  def asXml: Try[jdom2.Document] = try {
    asInputStream.map(is => new jdom2.input.SAXBuilder().build(is))
  } catch {
    case t: Exception => Failure(t)
  }

}
