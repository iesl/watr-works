package edu.umass.cs.iesl.watr


import java.io.{ InputStream }
import java.io.InputStreamReader
import java.io.Reader
import java.nio.{file => nio}
import play.api.libs.json
import scala.util.{Try, Failure, Success}
import org.jdom2
import scalaz.@@
import ammonite.ops._

// import TypeTags._


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

trait CorpusJsonFormats extends TypeTagFormats {
  import play.api.libs.json
  import json._


  implicit def HeaderInfoFormat = Json.format[HeaderInfo]
  implicit def CorpusEntryMetadataFormat = Json.format[CorpusEntryMetadata]
}

object corpusFormats extends CorpusJsonFormats


object Corpus {

  def apply(appRoot: nio.Path): Corpus = {
    new Corpus(Path(appRoot))
  }

  def apply(appRoot: Path): Corpus = {
    new Corpus(appRoot)
  }


}


class Corpus(
  val corpusRoot: Path
) {

  override val toString = {
    s"corpus:${corpusRoot}"
  }

  lazy val corpusSentinel =  corpusRoot / ".corpus-root"

  def touchSentinel(): Unit = {
    if (!sentinelExists()) {
      write(corpusSentinel, "")
    }
  }

  def sentinelExists(): Boolean = {
    exists(corpusSentinel)
  }

  def hasEntry(entryDescriptor: String): Boolean = {
    (entryDescriptor.endsWith(".d") &&
      exists(corpusRoot / entryDescriptor) &&
      stat(corpusRoot / entryDescriptor).isDir)
  }


  // def artifactExists(entryDescriptor: String, artifactDescriptor: String): Boolean = {
  //   val artifactPath = corpusRoot / RelPath(entryDescriptor) / artifactDescriptor
  //   entryExists(corpusRoot, entryDescriptor) && ammonite.ops.exists(artifactPath)
  // }

  // e.g., 3245.pdf, or sha1:afe23s...
  def entry(entryDescriptor: String): Option[CorpusEntry]= {
    Option(new CorpusEntry(entryDescriptor, this))
  }

  def entries(): Seq[CorpusEntry] = {
    val artifacts = (ls! corpusRoot)
      .filter(f => f.ext == "d" && f.isDir)
      .map { _.name }
      .sorted
      .map{ new CorpusEntry(_, this) }

    artifacts.filterNot(_.getArtifacts.isEmpty)
  }

}

sealed trait ArtifactDescriptor

class CorpusEntry(
  val entryDescriptor: String,
  val corpus: Corpus
) extends CorpusJsonFormats {

  override val toString = {
    s"${corpus}/./${entryDescriptor}"
  }


  val artifactsRoot = corpus.corpusRoot / RelPath(entryDescriptor)

  val entryDescriptorRoot = {
    // if (entryDescriptor)
    entryDescriptor.dropRight(2)
  }

  def getArtifacts(): Seq[String] = {
    val allFiles = ls! artifactsRoot //  map{ pdf => pdf }

    allFiles.map(_.name)
  }

  def putArtifact(artifactDescriptor: String, content: String): CorpusArtifact = {
    val outputPath = artifactsRoot/artifactDescriptor
    write(outputPath, content)
    new CorpusArtifact(artifactDescriptor, this)
  }

  def getArtifact(artifactDescriptor: String): Option[CorpusArtifact] = {
    if (hasArtifact(artifactDescriptor)) {
      (new CorpusArtifact(artifactDescriptor, this)).some
    } else None
  }

  def deleteArtifact(artifactDescriptor: String): Unit = {
    new CorpusArtifact(artifactDescriptor, this).delete
  }

  def hasArtifact(artifactDescriptor: String): Boolean ={
    exists(artifactsRoot / artifactDescriptor)
  }

  def getPdfArtifact(): Option[CorpusArtifact] = {
    // TODO fixme
    Some(
      new CorpusArtifact(s"${entryDescriptorRoot}", this)
    )

  }

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
    s"${entry}/./${artifactDescriptor}"
  }

  def exists(): Boolean = {
    ammonite.ops.exists! artifactPath
  }

  def delete(): Unit = {
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
