package edu.umass.cs.iesl.watr
package display

// import utils.{CompassDirection => CDir}

import geometry._
// import textreflow._

// import GeometryImplicits._
// import PageComponentImplicits._

object LabelWidgetRendering {
  // import LabelWidgetF._

  case class LwAccum(
    regions: List[(TargetRegion, LTBounds)]
  )

  def renderLabelWidget(lwidget: LabelWidget): LwAccum = {
    // import matryoshka._
    // import matryoshka.data._
    // import matryoshka.implicits._

    import LabelWidgetF._

    def visit(t: LabelWidgetF[(LabelWidget, LwAccum)]): LwAccum = t match {
      case Target(tr, emboss, sels)  =>
        // makeImageForTargetRegion(tr)
        //   .map({img =>
        //     val trPositionVec = tr.bbox.toPoint(CDir.NW)

        //     val adjustedSelects = sels.map({preselect:LTBounds =>
        //       val selectPositionVec = preselect.toPoint(CDir.NW)
        //       val adjustedPos = selectPositionVec - trPositionVec
        //       preselect.moveTo(adjustedPos.x, adjustedPos.y)
        //     })

        //     LwAccum(
        //       img,
        //       List(
        //         (tr, tr.bbox.moveToOrigin)
        //       )
        //     )
        //   })
        ???


      // case Col(attrs) =>
      //   val lls: List[LwRenderingAttrs] = attrs.map(_._2).sequenceU

      //   lls.map(vjoinAttrs(_))

      // case Panel((content, attr))  =>
      //   attr.map({ fobj =>
      //     addBorder(4.0, fobj)
      //   })

      // case MouseOverlay((bkplane, fattr)) =>
      //   // createShape(tr.bounds, "black", "yellow", 1f)
      //   fattr.map({ attr =>
      //     // addBorder(4.0, fobj)
      //   })

      //   ???

      case _ => sys.error("echoLabeler: TODO")
    }

    // lwidget
    //   .cata(attributePara(visit))
    //   .toPair._1.map(_.fobj)
    ???
  }
}
