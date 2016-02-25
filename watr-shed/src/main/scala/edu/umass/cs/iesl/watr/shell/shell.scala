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

  val conf = configuration.getPdfCorpusConfig

  // onCommand("init-corpus") {}


  val fin = File(conf.rootDirectory)
  if (fin.isDirectory) {
    println(s"processing dir $fin")
    val m = fin.glob("*.pdf")

    m.foreach { f =>
      val output = s"${f.path}.svg".toFile
      println(s"$f -> $output")


      f.inputStream.map { is =>
        // val wdom = itextUtil.itextPdfToSvg(is)
        // output < wdom.toSvg()
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

  def pdfToSVG(pdf: JFile): Document = {
    println("running: " + pdf2svg)

    // "ls" #| "grep .scala" #&& Seq("sh", "-c", "scalac *.scala") #|| "echo nothing found" lines
    val inputPath = pdf.toPath().toString()
    val outputPath = inputPath+".svg"
    println(s"running: ${pdf2svg} on ${inputPath} -> ${outputPath}")

    val result = List(pdf2svg, "-i", pdf.toPath().toString(), "-o", outputPath).!

    val jdomBuilder = new SAXBuilder();
    val svgJDom = jdomBuilder.build(new FileInputStream(outputPath))
    svgJDom
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
