package edu.umass.cs.iesl.watr
package textgrid

import scala.scalajs.js
import scala.scalajs.js.annotation._
import js.JSConverters._

import TypeTags._
import watrmarks._
import geometry._
import TextGridLabelWidget._
import utils.ExactFloats._

@JSExportTopLevel("watr.textgrid.TextGridInterop")
object TextGridInterop {

  @JSExportTopLevel("watr.textgrid.TextGridInterop.labelSchemas")
  object labelSchemas {

    @JSExport
    def abbrevFor(ls: LabelSchemas, label: String): String  = {
      ls.abbrevFor(Label(label))
    }

    @JSExport
    def allLabels(ls: LabelSchemas): js.Array[String]  = {
      ls.allLabels.toJSArray
    }

    @JSExport
    def childLabelsFor(ls: LabelSchemas, label: String): js.Array[String]  = {
      if (label=="") {
        ls.topLabels().toJSArray
      } else {
        ls.childLabelsFor(Label(label)).toJSArray
      }
    }

  }

  @JSExportTopLevel("watr.textgrid.TextGridInterop.gridRegions")
  object gridRegions {

    @JSExport
    def labels(gr: GridRegion): js.Array[String]  = {
      gr.classes().toJSArray
    }
  }

  @JSExportTopLevel("watr.textgrid.TextGridInterop.widgetDisplayGridProps")
  object widgetDisplayGridProps {

    @JSExport
    def gridRegions(wd: WidgetDisplayGridProps): js.Array[GridRegion]  = {
      wd.gridRegions.toJSArray
    }
  }
}

@JSExportTopLevel("watr.textgrid.TextGridConstructor_Companion")
object TextGridConstructor {
  @JSExport
  def create(): TextGridConstructor =
    new TextGridConstructor()
}


@JSExportTopLevel("watr.textgrid.TextGridConstructor")
class TextGridConstructor() extends TextGridConstruction {



  def makeTextGrid(stableId: String, pageNum: Int, pageStr: String): TextGrid = {
    stringToPageTextGrid(DocumentID(stableId), pageStr, PageNum(pageNum), None)
  }

  val Authors = Label.auto
  val Author = Label.auto
  val FirstName = Label.auto
  val MiddleName = Label.auto
  val LastName = Label.auto
  val Journal = Label.auto
  val RefMarker = Label.auto
  val RefNumber = Label.auto

  val stableId = DocumentID("docXX")

  @JSExport
  def getTestTextGridLarge(): TextGrid = {
    val loremIpsum = """Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur? """
    val labelSpans = List(
      ((0, 100),   RefMarker),
      ((0, 50),   RefNumber),
      ((50, 60),   RefNumber),
      ((100, 120),  Authors),
      ((100, 110),  Author),
      ((120, 130),  Authors),
      ((120, 125),  Author),
      ((130, 190),  Authors),
      ((200, 400),  Authors),
      ((200, 300),  Author),
      ((300, 400),  Author),
      ((400, 600),  Authors),
      ((600, 820),  Authors)
    )
    val ls2 = labelSpans.map{case ((b, e), l) => ((b+820, e+820), l) }

    val ls = labelSpans //  ++ ls2
    var textGrid = stringToPageTextGrid(stableId, loremIpsum,  PageNum(1), None)
    val labeledRow = addLabelsToGridRow(textGrid.rows.head, ls)
    textGrid = TextGrid.fromRows(stableId, Seq(labeledRow))
    textGrid = textGrid.splitOneLeafLabelPerLine()
    textGrid
  }

  @JSExport
  def getTestTextGrid(): TextGrid = {
    val labelSpans = List(
      ((0, 1),   RefMarker),
      ((0, 0),   RefNumber),
      ((3, 33),  Authors),
      ((3, 17),  Author),
      ((3, 14),  LastName),
      ((17, 17), FirstName),
      ((24, 33), Author),
      ((36, 48), Journal)
    )
    val unlabeledText = {
      //"0         1         2         3         4         5
      // 012345678901234567890123456789012345678901234567899 """
      """1. Bishop-Clark, C  and Wheeler, D; S.Eng. P-Hall"""

    }
    var textGrid = stringToPageTextGrid(stableId, unlabeledText,  PageNum(1), None)
    val labeledRow = addLabelsToGridRow(textGrid.rows.head, labelSpans)
    textGrid = TextGrid.fromRows(stableId, Seq(labeledRow))
    textGrid = textGrid.splitOneLeafLabelPerLine()
    textGrid = textGrid.split(9, 7).get

    // val allRows = (0 to 10).flatMap{_ => textGrid.rows }
    // TextGrid.fromRows(stableId, allRows)

    textGrid
  }

  @JSExport
  def getSampleTextGrid1(): TextGrid = {
    val labelSpans = List(
      ((0, 1),   RefMarker),
      ((0, 0),   RefNumber),
      // ((3, 33),  Authors),
      // ((3, 17),  Author),
      // ((3, 14),  LastName),
      // ((17, 17), FirstName),
      // ((24, 33), Author),
      // ((36, 48), Journal)
    )
    val unlabeledText = {
      //"0         1         2         3         4         5
      // 012345678901234567890123456789012345678901234567899 """
      """1. """

    }
    var textGrid = stringToPageTextGrid(stableId, unlabeledText,  PageNum(1), None)
    val labeledRow = addLabelsToGridRow(textGrid.rows.head, labelSpans)
    textGrid = TextGrid.fromRows(stableId, Seq(labeledRow))
    textGrid = textGrid.splitOneLeafLabelPerLine()

    textGrid
  }

  @JSExport
  def getTestLabelSchema(): LabelSchemas = {

    val authorNameSchema = LabelSchema(
      Author, Some(('a', 'u')), List(
        LabelSchema(FirstName),
        LabelSchema(MiddleName),
        LabelSchema(LastName))
    )

    val authorListSchema = LabelSchema(
      Authors, Some(('a', 's')), List(
        authorNameSchema)
    )

    val refMarkerSchema = LabelSchema(
      RefMarker, None, List(
        LabelSchema(RefNumber))
    )
    val journalSchema = LabelSchema(
      Journal, None
    )

    LabelSchemas(
      List(
        authorListSchema,
        refMarkerSchema, journalSchema
      )
    )
  }


  @JSExport
  def textGridToWidgetGrid(
    textGrid: TextGrid,
    labelSchemas: LabelSchemas,
    originX: Int,
    originY: Int
  ): WidgetDisplayGridProps = {

    val labelTree = time("textGridToLabelTree"){ textGridToLabelTree(textGrid)}
    val gridRegions = time("labelTreeToGridRegions"){  labelTreeToGridRegions(labelTree, labelSchemas, originX, originY) }
    new  WidgetDisplayGridProps(
      labelTree,
      gridRegions
    )
  }

}

@JSExportTopLevel("watr.textgrid.WidgetDisplayGridProps")
class WidgetDisplayGridProps(
  val labelTree: scalaz.Tree[TreeNode],
  val gridRegions: Seq[GridRegion]
) {
  lazy val regionExtents = gridRegions.map{r =>
    (r.bounds.getRight.toInt, r.bounds.getBottom.toInt)
  }

  @JSExport
  def getGridRowCount(): Int = {
    regionExtents.map(_._2).max
  }

  @JSExport
  def getGridColCount(): Int = {
    regionExtents.map(_._1).max
  }

}

@JSExportTopLevel("watr.textgrid.RTreeRect")
@JSExportAll
class RTreeRect(
  val region: GridRegion,
) {

  val bounds = {
    val LTBounds.Ints(l, t, w, h) = region.bounds
    LTBounds.Ints(l*4, t*4, w*4, h*4)
  }

  val left:Int   = bounds.left.asInt
  val top:Int    = bounds.top.asInt
  val width:Int  = bounds.width.asInt
  val height:Int = bounds.height.asInt

  val minX = left
  val minY = top
  val maxX = left + width
  val maxY = top + height

  val x = left
  val y = top

  val x1 = left
  val x2 = left + width
  val y1 = top
  val y2 = top + height

  val bottom = top + height
  val right  = left + width

  // def topLeft() = new PointPolyfill{
  //   override val x = self.left
  //   override val y = self.top
  // }
}

@js.native
trait RTreeApi extends js.Object {

  def loadData(data: js.Array[RTreeRect]): Unit = js.native

}
