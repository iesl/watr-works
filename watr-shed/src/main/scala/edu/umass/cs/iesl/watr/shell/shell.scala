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

  def argsToMap(args: Array[String]): Map[String, List[String]] = {
    import scala.collection.mutable.ListMap
    val argmap = ListMap[String, List[String]]()
    args.foldLeft(argmap){ (m, k: String) =>
        val ss: Seq[Char] = k
        ss match {
          case Seq('-', '-', opt @ _*) => m.put(opt.toString, List[String]())
          case Seq('-', opt @ _*) => m.put(opt.toString, List[String]())
          case opt @ _ => m.put(m.head._1, m.head._2 ++ List[String](opt.toString))
        }
        m
    }
    Map[String, List[String]](argmap.toList.reverse: _*)
  }

  val argMap = argsToMap(args)

  val conf = configuration.getPdfCorpusConfig("conf/application.conf")

  // onCommand("init-corpus") {}


  val corpusRoot = File(conf.rootDirectory)
  // ensure proper corpus directory structure

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
      if (!output.isReadable) {
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
