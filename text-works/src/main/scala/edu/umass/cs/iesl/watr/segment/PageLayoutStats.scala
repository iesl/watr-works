package edu.umass.cs.iesl.watr
package segment

import scala.collection.mutable

import geometry._
import geometry.syntax._

import utils.ExactFloats._

import org.dianahep.{histogrammar => HST}
// import org.dianahep.histogrammar.ascii._


class LayoutStats {
  import HST._

  val trapezoidHeights = HST.SparselyBin.ing(1.0, {t: Trapezoid => t.height().asDouble})
  val leftAcuteBaseAngles = HST.SparselyBin.ing(0.1, {t: Trapezoid => if (t.leftBaseAngleType() == AngleType.Acute) { t.leftBaseAngle() } else 0})
  val leftObtuseBaseAngles = HST.SparselyBin.ing(0.1, {t: Trapezoid => if (t.leftBaseAngleType() == AngleType.Obtuse) { t.leftBaseAngle() } else 0})

}

class PageLayoutStats extends LayoutStats {}

class DocumentLayoutStats extends LayoutStats {
  // val pageStats: mutable.HashMap[Int@@PageNum, PageLayoutStats] = mutable.HashMap()

  // def addPage(pageNum: Int@@PageNum): PageLayoutStats = {
  //   pageStats.put(pageNum, new PageLayoutStats())
  //   pageStats(pageNum)
  // }

  // def getPage(pageNum: Int@@PageNum): PageLayoutStats = {
  //   pageStats(pageNum)
  // }

}

