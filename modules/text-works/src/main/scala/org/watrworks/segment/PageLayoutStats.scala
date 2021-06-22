package org.watrworks
package segment

import geometry._
import TypeTags._
import utils.DoOrDieHandlers._
import utils.ExactFloats._
import utils._

import org.dianahep.{histogrammar => HST}
import scala.collection.mutable

class LayoutStats {
  import HST._

  val trapezoidHeights = HST.SparselyBin.ing(1.0, { t: Trapezoid => t.height().asDouble() })
  val leftAcuteBaseAngles = HST.SparselyBin.ing(
    0.1,
    { t: Trapezoid =>
      if (t.leftBaseAngleType() == AngleType.Acute) { t.leftBaseAngle() }
      else 0
    }
  )
  val leftObtuseBaseAngles = HST.SparselyBin.ing(
    0.1,
    { t: Trapezoid =>
      if (t.leftBaseAngleType() == AngleType.Obtuse) { t.leftBaseAngle() }
      else 0
    }
  )

  def accumShapeStats(shape: GeometricFigure): Unit = {
    shape match {
      case t: Trapezoid =>
        trapezoidHeights.fill(t)
        leftAcuteBaseAngles.fill(t)
        leftObtuseBaseAngles.fill(t)
      case _ =>
    }
  }

  val namedTables = mutable.HashMap[String, TabularData[_, _, _, _, _]]()

  def initTable[R: Ordering, C: Ordering, A](
    tableName: String
  ): TabularData[R, C, A, Unit, Unit] = {
    assume(namedTables.get(tableName).isEmpty)
    namedTables.put(tableName, GuavaHelpers.initTable[R, C, A]())
    getTable(tableName)
  }

  def getTable[R, C, A](tableName: String): TabularData[R, C, A, Unit, Unit] = {
    namedTables
      .get(tableName)
      .map(_.asInstanceOf[TabularData[R, C, A, Unit, Unit]])
      .orDie(s"LayoutStats.getTable(${tableName}) Error: No such table exists")
  }
}

import com.google.{common => guava}
import guava.{collect => gcol}

class PageLayoutStats extends LayoutStats {
  import scalax.collection.Graph
  import scalax.collection.{ mutable => mutgraph }
  import scalax.collection.edge.LDiEdge
  import scalax.collection.edge.Implicits._


  object charrunGraph {
    val graph = mutgraph.Graph()

  }


  object monofontVJump {
    type RowT   = String @@ ScaledFontID
    type ValueT = Int @@ FloatRep
    type TableT = mutable.Map[RowT, List[ValueT]]

    val table: TableT = mutable.Map()

    def add(
      fontId: RowT,
      y1: Int @@ FloatRep,
      y2: Int @@ FloatRep
    ): Unit = {
      val vdist = FloatRep(math.abs(y2.unwrap - y1.unwrap))
      table.updateWith(fontId)((v: Option[List[ValueT]]) =>
        v.map(vdist :: _).orElse(Some(List(vdist)))
      )
    }
  }

  object fontVJump {
    type RowT   = String @@ ScaledFontID
    type ColT   = String @@ ScaledFontID
    type ValueT = gcol.TreeMultiset[Int @@ FloatRep]
    type TableT = TabularData.Init[RowT, ColT, ValueT]

    private def initValue(): ValueT = gcol.TreeMultiset.create(null)

    val table: TableT = GuavaHelpers.initTable()

    def add(
      f1: RowT,
      y1: Int @@ FloatRep,
      f2: ColT,
      y2: Int @@ FloatRep
    ): Unit = {
      val vdist = FloatRep(math.abs(y2.unwrap - y1.unwrap))

      table.useOrSet(f1, f2, _.add(vdist), initValue())
    }
  }

  object fontBackslashAngle {
    type RowT = String @@ ScaledFontID
    type ColT = String @@ ScaledFontID
    // type ValueT = gcol.TreeMultiset[Double]
    type ValueT = gcol.TreeMultiset[Int @@ FloatRep]
    type TableT = TabularData.Init[RowT, ColT, ValueT]

    private def initValue(): ValueT = gcol.TreeMultiset.create(null)

    val table: TableT = GuavaHelpers.initTable()

    def add(f1: RowT, f2: ColT, angle: Int @@ FloatRep): Unit =
      table.useOrSet(f1, f2, _.add(angle), initValue())

  }

}

class DocumentLayoutStats extends LayoutStats {

  object fontVJumpByPage {
    type RowT   = Int @@ PageNum
    type ColT   = String @@ ScaledFontID
    type ValueT = gcol.TreeMultiset[Int @@ FloatRep]
    type TableT = TabularData.Init[RowT, ColT, ValueT]

    private def initValue(): ValueT = gcol.TreeMultiset.create(null)

    val table: TableT = GuavaHelpers.initTable()

    def add(
      pageNum: RowT,
      fontId: ColT,
      y1: Int @@ FloatRep,
      y2: Int @@ FloatRep
    ): Unit = {
      val vdist = FloatRep(math.abs(y2.unwrap - y1.unwrap))

      table.useOrSet(pageNum, fontId, _.add(vdist), initValue())
    }
  }
}
