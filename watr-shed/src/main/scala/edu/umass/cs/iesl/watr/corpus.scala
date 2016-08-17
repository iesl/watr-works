package edu.umass.cs.iesl.watr


import java.io.{ InputStream }
import java.io.InputStreamReader
import java.io.Reader
import java.nio.{file => nio}
import play.api.libs.json
import scala.util.{Try, Failure, Success}
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


  def putArtifact(artifactDescriptor: String, content: String, groupDescriptor: String = "."): CorpusArtifact = {
    val outputPath = artifactsRoot / RelPath(groupDescriptor) / RelPath(artifactDescriptor)
    write(outputPath, content)
    val group = new CorpusArtifactGroup(groupDescriptor, this)
    new CorpusArtifact(artifactDescriptor, group)
  }

  def getArtifact(artifactDescriptor: String, groupDescriptor: String = "."): Option[CorpusArtifact] = {
    if (hasArtifact(artifactDescriptor, groupDescriptor)) {
      val group = new CorpusArtifactGroup(groupDescriptor, this)
      (new CorpusArtifact(artifactDescriptor, group)).some
    } else None
  }

  def getArtifactGroup(groupDescriptor: String): Option[CorpusArtifactGroup] = {
    if (hasArtifactGroup(groupDescriptor)) {
      new CorpusArtifactGroup(groupDescriptor, this).some
    } else None
  }

  def deleteArtifact(artifactDescriptor: String, groupDescriptor: String = "."): Unit = {
    val artifact = new CorpusArtifact(artifactDescriptor,
      new CorpusArtifactGroup(groupDescriptor, this)
    )

    artifact.delete
  }
  def hasArtifactGroup(groupDescriptor: String): Boolean ={
    exists(artifactsRoot / RelPath(groupDescriptor))
  }

  def hasArtifact(artifactDescriptor: String, groupDescriptor: String = "."): Boolean ={
    exists(artifactsRoot / RelPath(artifactDescriptor))
  }

  def getPdfArtifact(): Option[CorpusArtifact] = {
    val artifact = new CorpusArtifact(s"${entryDescriptorRoot}",
      new CorpusArtifactGroup(".", this)
    )

    artifact.some
  }

  def getSvgArtifact(): CorpusArtifact = {
    new CorpusArtifact(s"${entryDescriptorRoot}.svg",
        new CorpusArtifactGroup(".", this)
    )
  }

}


class CorpusArtifactGroup(
  val groupDescriptor: String,
  val entry: CorpusEntry
) {
  lazy val rootPath = entry.artifactsRoot / RelPath(groupDescriptor)

  def descriptor = s"""${entry.entryDescriptor}/${groupDescriptor}"""

  override val toString = {
    s"${entry}/${groupDescriptor}"
  }

  def getArtifacts(): Seq[CorpusArtifact] = {
    ls(rootPath)
      .sortBy(_.name)
      .map(path => new CorpusArtifact(path.name, this))
  }

}

class CorpusArtifact(
  val artifactDescriptor: String,
  val group: CorpusArtifactGroup
  // val entry: CorpusEntry
) {
  import ammonite.{ops => fs}

  override val toString = {
    s"${group}/${artifactDescriptor}"
  }


  def exists(): Boolean = {
    fs.exists(artifactPath)
  }

  def delete(): Unit = {
    fs.rm(artifactPath)
  }

  def descriptor = s"""${group.descriptor}/${artifactDescriptor}"""

  def artifactPath = group.rootPath / artifactDescriptor

  def asPath: Try[Path] = Success(artifactPath)

  def asDirectory: Try[Path] = {
    asPath.filter({p => fs.stat(p).isDir})
  }

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

}
