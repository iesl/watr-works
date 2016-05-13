package edu.umass.cs.iesl.watr
package ext

import ammonite.ops._

import java.nio.{file => nio}

object Works extends App {

  import java.io.{File => JFile}

  case class AppConfig(
    entry: Option[JFile] = None,
    corpusRoot: Option[JFile] = None,
    action: Option[String] = None,
    force: Boolean = false,
    exec: Option[(AppConfig) => Unit] = None
  )

  def corpusRootOrDie(ac: AppConfig): Path = ac.corpusRoot.map(
   croot => cwd/RelPath(croot)
  ).getOrElse(sys.error("no corpus root specified"))

  def setAction(conf: AppConfig, action: (AppConfig) => Unit): AppConfig = {
    conf.copy(exec=Option(action))
  }

  val parser = new scopt.OptionParser[AppConfig]("scopt") {
    head("itext zone extractor", "0.1")

    note("Run svg text extraction and analysis")

    opt[Unit]('x', "overwrite") action { (v, conf) =>
      conf.copy(force = true) } text("force overwrite of existing files")

    opt[JFile]('a', "artifact") action { (v, conf) =>
      conf.copy(entry = Option(v))
    } text("artifact id (same as *.d directory name)")


    opt[JFile]('c', "corpus") action { (v, conf) =>
      conf.copy(corpusRoot = Option(v))
    } text ("root path of the corpus")

    cmd("init") action { (_, conf) =>
      setAction(conf, {(ac: AppConfig) =>
        normalizeCorpusEntry(ac)
      })
    } text ("init (or re-init) a corpus directory structure") // children()

    cmd("cerm") action { (v, conf) =>
      setAction(conf, {(ac: AppConfig) =>
        extractCermineZones(ac)
      })
    } text ("run Cermine zone extractor") // children()
  }


  val config = parser.parse(args, AppConfig()).getOrElse{
    sys.error(parser.usage)
  }


  val croot = config
    .corpusRoot.map{ r => cwd/RelPath(r) }
    .filter { _.isDir }
    .getOrElse { sys.error(s"corpus root is not dir ('${config.corpusRoot}')") }


  config.exec.foreach { _.apply(config) }

  def getProcessList(conf: AppConfig): Seq[CorpusEntry] = {
    val corpus = Corpus(corpusRootOrDie(conf))

    val toProcess = conf.entry match {
      case Some(entry) => Seq(corpus.entry(entry.getName))
      case None => corpus.entries()
    }
    toProcess
  }

  def extractCermineZones(conf: AppConfig): Unit = {
    getProcessList(conf).foreach { entry =>
      def process(): Unit = {
        val pdfArtifact = entry.getPdfArtifact()

        val sha1 = pdfArtifact.asPath.map{p =>
          val bytes = nio.Files.readAllBytes(p.toNIO)
          DigestUtils.shaHex(bytes)
        }.getOrElse { sys.error(s"could not read byte array for ${pdfArtifact}") }

        entry.putArtifact("cermine-zones.json",
          pdfArtifact.asInputStream.map{is =>
            CermineExtractor.cermineZonesToJson(is)
          }.getOrElse { sys.error(s"could not extract cermine data for ${pdfArtifact}") }
        )
      }

      println(s"extractCermineZones(${entry.corpus}, ${entry}")
      if (entry.hasArtifact("cermine-zones.json")) {
        if (conf.force){
          entry.deleteArtifact("cermine-zones.json")
          process()
        } else {
          println(s"skipping existing ${entry}, use -x to force reprocessing")
        }
      } else {
        process()
      }
    }
  }

  def normalizeCorpusEntry(conf: AppConfig): Unit = {
    val corpus = Corpus(corpusRootOrDie(conf))

    println(s"normalizing corpus at ${corpus.corpusRoot}")
    ls! corpus.corpusRoot foreach { pdf =>
      val artifactPath = corpus.corpusRoot / s"${pdf.name}.d"
      if (pdf.isFile && !(exists! artifactPath)) {
        println(s" processing ${pdf}")
        mkdir! artifactPath
        mv.into(pdf, artifactPath)
      }
    }
  }


}
