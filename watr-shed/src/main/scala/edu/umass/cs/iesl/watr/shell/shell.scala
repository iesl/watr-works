package edu.umass.cs.iesl.watr
package shell

import java.io.FileInputStream
import java.io.InputStream
import java.io.{File => JFile}
import pl.edu.icm.cermine.structure.model.BxChunk
import pl.edu.icm.cermine.structure.model.BxWord

import org.jdom.output.Format
import org.jdom2.Document
import org.jdom2.input.SAXBuilder
import pl.edu.icm.cermine.ComponentConfiguration
import pl.edu.icm.cermine.content.model.ContentStructure
import pl.edu.icm.cermine.structure.model.BxDocument

import scala.language.implicitConversions

import scala.sys.process._
import better.files._



object Works extends App {


  case class AppConfig(
    // foo: Int = -1,
    // out: File = new File("."),
    force: Boolean = false
  )

  val parser = new scopt.OptionParser[AppConfig]("scopt") {
    head("watr-works", "wip")

    note("some notes.\n")

    help("help") text("prints this usage text")

    opt[Unit]('f', "force") action { (v, opts) =>
      opts.copy(force = true) } text("force overwrite of existing files")

    // opt[File]('o', "out") required() valueName("<file>") action { (x, c) =>
    //   c.copy(out = x) } text("out is a required file property")
    // opt[(String, Int)]("max") action { case ((k, v), c) =>
    //   c.copy(libName = k, maxCount = v) } validate { x =>
    //   if (x._2 > 0) success else failure("Value <max> must be >0")
    // } keyValueName("<libname>", "<max>") text("maximum count for <libname>")
    // opt[Seq[File]]('j', "jars") valueName("<jar1>,<jar2>...") action { (x,c) =>
    //   c.copy(jars = x) } text("jars to include")
    // opt[Map[String,String]]("kwargs") valueName("k1=v1,k2=v2...") action { (x, c) =>
    //   c.copy(kwargs = x) } text("other arguments")
    // opt[Unit]("verbose") action { (_, c) =>
    //   c.copy(verbose = true) } text("verbose is a flag")
    // opt[Unit]("debug") hidden() action { (_, c) =>
    //   c.copy(debug = true) } text("this option is hidden in the usage text")
    // note("some notes.\n")
    // help("help") text("prints this usage text")
    // arg[File]("<file>...") unbounded() optional() action { (x, c) =>
    //   c.copy(files = c.files :+ x) } text("optional unbounded args")
    // cmd("update") action { (_, c) =>
    //   c.copy(mode = "update") } text("update is a command.") children(
    //   opt[Unit]("not-keepalive") abbr("nk") action { (_, c) =>
    //     c.copy(keepalive = false) } text("disable keepalive"),
    //     opt[Boolean]("xyz") action { (x, c) =>
    //       c.copy(xyz = x) } text("xyz is a boolean property"),
    //     checkConfig { c =>
    //       if (c.keepalive && c.xyz) failure("xyz cannot keep alive") else success }
    // )

  }

  // parser.parse returns Option[C]
  val config = parser.parse(args, AppConfig()).getOrElse{
    sys.error(parser.usage)
  }


  parser.parse(args, AppConfig()) match {
    case Some(config) =>

      // do stuff

    case None =>
      // arguments are bad, error message will have been displayed
  }

  val conf = configuration.getPdfCorpusConfig(".")


  val corpusRoot = File(conf.rootDirectory)

  // run iesl pdf -> svg over corpus
  if (corpusRoot.isDirectory) {
    println(s"processing dir $corpusRoot")
    val m = corpusRoot.glob("**/*.pdf")

    m.foreach { f =>
      val artifactPath = s"${f.path}.d".toFile
      if (!artifactPath.isDirectory) {
        artifactPath.createDirectory()
      }

      val output = s"${artifactPath}/${f.name}.svg".toFile
      if (!output.isReadable || config.force) {
        ops.pdfToSVG(f, output)
      } else {
        println(s"skipping $f")
      }

    }
  } else {
    sys.error("please specify a file or directory")
  }
}


object ops {
  import pl.edu.icm.cermine.ExtractionUtils

  // val pdfToSVGPath = cwd/up/"iesl-pdf-to-text"
  // val pdfToSVGExe = pdfToSVGPath/"bin"/"run.sh"
  val pdf2svg = "ext/iesl-pdf-to-text/bin/run.sh"

  def pdfToSVG(pdfInput: File, outputPath: File): Unit = {
    println("running: " + pdf2svg)

    // "ls" #| "grep .scala" #&& Seq("sh", "-c", "scalac *.scala") #|| "echo nothing found" lines
    println(s"running: ${pdf2svg} on ${pdfInput} -> ${outputPath}")

    val result = List(pdf2svg, "-i", pdfInput.toString, "-o", outputPath.toString()).!

  }



  def cerminePDF(pdf: JFile): (BxDocument, ContentStructure) = {
    val in = new FileInputStream(pdf);
    val conf = new ComponentConfiguration()
    val doc = ExtractionUtils.extractStructure(conf, in);
    val contentStructure = ExtractionUtils.extractText(conf, doc);



    // println(s"result: doc=${doc.toText()}")

    // val fmt = Format.getPrettyFormat()
    // val fmt = Format.getCompactFormat()
    // val xmlout = new org.jdom.output.XMLOutputter(fmt)
    // val strout = xmlout.outputString(result)
    // println(strout)

    (doc, contentStructure)
  }












}
