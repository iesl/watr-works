package edu.umass.cs.iesl.watr
package segment

import geometry._
import geometry.syntax._

import utils.ExactFloats._
import utils._

import utils.DoOrDieHandlers._
import org.dianahep.{histogrammar => HST}
// import org.dianahep.histogrammar.ascii._
import scala.collection.mutable


class LayoutStats {
  import HST._

  val trapezoidHeights = HST.SparselyBin.ing(1.0, {t: Trapezoid => t.height().asDouble})
  val leftAcuteBaseAngles = HST.SparselyBin.ing(0.1, {t: Trapezoid => if (t.leftBaseAngleType() == AngleType.Acute) { t.leftBaseAngle() } else 0})
  val leftObtuseBaseAngles = HST.SparselyBin.ing(0.1, {t: Trapezoid => if (t.leftBaseAngleType() == AngleType.Obtuse) { t.leftBaseAngle() } else 0})


  val namedTables = mutable.HashMap[String, TabularData[_, _, _]]()

  def initTable[R: Ordering, C: Ordering, A](tableName: String): TabularData[R, C, A] = {
    assume(namedTables.get(tableName).isEmpty)
    namedTables.put(tableName, GuavaHelpers.initTable[R, C, A]())
    getTable(tableName)
  }

  def getTable[R, C, A](tableName: String): TabularData[R, C, A] = {
    namedTables.get(tableName)
      .map(_.asInstanceOf[TabularData[R, C, A]])
      .orDie(s"LayoutStats.getTable(${tableName}) Error: No such table exists")
  }
}

class PageLayoutStats extends LayoutStats {}

class DocumentLayoutStats extends LayoutStats {}

