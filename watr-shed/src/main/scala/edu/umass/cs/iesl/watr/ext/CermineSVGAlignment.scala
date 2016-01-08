package edu.umass.cs.iesl.watr
package ext

import java.io.InputStreamReader
import pl.edu.icm.cermine.ComponentConfiguration
import pl.edu.icm.cermine.ExtractionUtils
import pl.edu.icm.cermine.structure.model._
import pl.edu.icm.cermine.structure.tools.BxBoundsBuilder
import scala.collection.JavaConversions._

import better.files._

import watrmarks._
import watrmarks.dom._

object cermineSvgAlignment {


  /**
   * Returns the longest common substring of two strings 'a' and 'b'.
   *
   * http://www.geeksforgeeks.org/longest-common-substring/
   *
   * Time - O(mn)
   * Space - O(mn)
   */
  def longestCommonSubstring(a: String, b: String) : String = {
    def loop(m: Map[(Int, Int), Int], bestIndices: List[Int], i: Int, j: Int) : String = {
      if (i > a.length) {
        b.substring(bestIndices(1) - m((bestIndices(0),bestIndices(1))), bestIndices(1))
      } else if (i == 0 || j == 0) {
        loop(m + ((i,j) -> 0), bestIndices, if(j == b.length) i + 1 else i, if(j == b.length) 0 else j + 1)
      } else if (a(i-1) == b(j-1) && math.max(m((bestIndices(0),bestIndices(1))), m((i-1,j-1)) + 1) == (m((i-1,j-1)) + 1)) {
        loop(
          m + ((i,j) -> (m((i-1,j-1)) + 1)),
          List(i, j),
          if(j == b.length) i + 1 else i,
          if(j == b.length) 0 else j + 1
        )
      } else {
        loop(m + ((i,j) -> 0), bestIndices, if(j == b.length) i + 1 else i, if(j == b.length) 0 else j + 1)
      }
    }
    loop(Map[(Int, Int), Int](), List(0, 0), 0, 0)
  }


  def extractViaCermine(pdfFile: File): BxDocument = {

    val conf = new ComponentConfiguration()

    pdfFile.inputStream.map { is =>

      val structuredDoc = ExtractionUtils.extractStructure(conf, is)

      val filtered = ExtractionUtils.filterContent(conf, structuredDoc)

      ExtractionUtils.classifyMetadata(conf, filtered);
    }.head
  }

  def svgToString(dom: WatrDom): String = {
    val cur = dom.toCursor(CharLabel).get

    cur.unfoldLabels.foldLeft("") {
      case (acc, c) =>
        // println(acc)
        acc + c.toText
    }
  }

  def bxDocToString(doc: BxDocument): String = {
    doc.toText()
  }

  def alignSvgToCermine(pdfFile: File, svgFile: File, bioDict: BioLabelDictionary): Unit = {
    import scalaz.{Show, TreeLoc, Tree}
    import scala.collection.mutable

    val docWithMetadata = extractViaCermine(pdfFile: File)

    val svg = svgFile.fileReader.map{r =>
      watrmarks.dom.readWatrDom(r, bioDict)
    } head


    // val str1 = svgToString(svg)
    // val str2 = bxDocToString(docWithMetadata)

    // println(str1)

    // println("\n"*20)
    // println(str2)

    var ccursor = svg.toCursor(CharLabel)
    val charBuffer = mutable.ArrayBuffer[Either[(Char, Char), Char]]()

    docWithMetadata.asChunks().foreach{ chunk =>
      chunk.iterator().toList.foreach { c =>
        // eat space
        while (ccursor.isDefined && ccursor.get.toText==" ") {
          ccursor = ccursor.get.next
        }

        ccursor.foreach {ccur =>
          if (c.toString == ccur.toText) {
            charBuffer.append(Right(c))
          } else {
            charBuffer.append(Left((c.toChar -> ccur.toText.head)))
          }
          ccursor = ccur.next
        }
      }
    }
    charBuffer.foreach {
      _ match {
        case Left((c1, c2)) =>
          println(s"\nmismatch ${c1} ${c2}")
        case Right(c) =>
          print(s"${c}")
      } }

    // val substring = longestCommonSubstring(str1, str2)
    // println(substring)




    // val _ = docWithMetadata.asPages().toList.zipWithIndex.foreach{ case (page, i) =>


    //   page.iterator.toList.zipWithIndex.foreach{case (zone, ii) =>

    //     zone.iterator().toList.foreach{ line =>

    //       line.iterator().toList.foreach{ word =>

    //         word.iterator().toList.foreach{ chunk =>
    //           chunk.iterator().toList.foreach { c =>

    //             // eat space
    //             while (ccursor.isDefined && ccursor.get.toText==" ") {
    //               ccursor = ccursor.get.next
    //             }

    //             ccursor.foreach {ccur =>
    //               if (c.toString == ccur.toText) {
    //                 charBuffer.append(Right(c))
    //               } else {
    //                 charBuffer.append(Left((c.toChar -> ccur.toText.head)))
    //               }
    //               ccursor = ccur.next
    //             }
    //           }
    //         }
    //       }
    //     }
    //   }
    // }




    // WatrDom(accum.toTree)
  }


  def main(args: Array[String]) = {

    val pdfFilename = args(0)
    val svgFilename = args(1)

    alignSvgToCermine(pdfFilename.toFile, svgFilename.toFile, StandardLabels.bioDict)
  }


}
