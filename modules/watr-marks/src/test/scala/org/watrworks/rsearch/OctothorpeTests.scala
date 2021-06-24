package org.watrworks
package rsearch

import utils.{M3x3Position => M3}

import geometry._
import textgrid.TextGridConstruction
import TypeTags._
import watrmarks.Label

class OctothorpeTest extends WatrSpec with TextGridConstruction {
  val B = Octothorpe.Bounds

  type RectShape = TestShape[Rect]

  it should "smokescreen" in {

    val text = """|abcde
                  |01234
                  |fghij
                  |""".stripMargin

    val documentId = DocumentID("foo")

    val textGrid = stringToPageTextGrid(documentId, text, PageNum(1), None)

    val rtreeIndex = RTreeIndex.empty[GeometricFigure, RectShape]()

    textGrid
      .indexedCells()
      .zipWithIndex
      .foreach({ case ((cell, row @ _, col @ _), cellIndex) =>
        val shape = TestShape[Rect](
          cell.pageRegion.bbox,
          ShapeID(cellIndex),
          Set(Label(cell.char.toString()))
        )
        rtreeIndex.add(shape)
      })

    val Bounds5x3   = getRegionBounds(0, 0, 5, 3)
    val BoundsAt1_1 = getRegionBounds(1, 1, 1, 1)

    val plusShaped = Octothorpe.withSearchRegions(
      Octothorpe.cell(M3.Top),
      Octothorpe.cell(M3.Bottom),
      Octothorpe.cellspan(M3.Left, M3.Right)
    )

    val doSearch = (r: Rect) => rtreeIndex.search(r, _ => true)

    val focused = plusShaped
      .withHorizon(Bounds5x3)
      .centeredOn(BoundsAt1_1)

    val found       = focused.runSearch(doSearch)
    val foundLabels = found.map(_.labels.head).mkString(", ")
    println(s"found: ${foundLabels}")
  }

  import _root_.io.circe, circe.syntax._

  it should "serialize" in {

    val text = """|abcde
                  |01234
                  |fghij
                  |""".stripMargin

    val documentId = DocumentID("foo")

    val textGrid = stringToPageTextGrid(documentId, text, PageNum(1), None)

    val rtreeIndex = RTreeIndex.empty[GeometricFigure, RectShape]()

    textGrid
      .indexedCells()
      .zipWithIndex
      .foreach({ case ((cell, row @ _, col @ _), cellIndex) =>
        val shape = TestShape[Rect](
          cell.pageRegion.bbox,
          ShapeID(cellIndex),
          Set(Label(cell.char.toString()))
        )
        rtreeIndex.add(shape)
      })

    val Bounds5x3   = getRegionBounds(0, 0, 5, 3)
    val BoundsAt1_1 = getRegionBounds(1, 1, 1, 1)

    val plusShaped = Octothorpe.withSearchRegions(
      Octothorpe.cell(M3.Top),
      Octothorpe.cell(M3.Bottom),
      Octothorpe.cellspan(M3.Left, M3.Right)
    )

    val doSearch = (r: Rect) => rtreeIndex.search(r, _ => true)

    val focused = plusShaped
      .withHorizon(Bounds5x3)
      .centeredOn(BoundsAt1_1)

    // val found       = focused.runSearch(doSearch)

    val jsonVal = focused.asJson
    println("ocothorpe")
    println(jsonVal)

  }

}
