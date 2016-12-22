package edu.umass.cs.iesl.watr
package watrcolors

import geometry._
import GeometricFigure._
// import EnrichGeometricFigures._


trait ShapeDataTypePicklers {
  // import scala.reflect._
  import boopickle.DefaultBasic._
  import TypeTags._
  import watrmarks._

  implicit val pGeometricFigure = compositePickler[GeometricFigure]
  implicit val pLTBounds = PicklerGenerator.generatePickler[LTBounds]
  implicit val pLBBounds = PicklerGenerator.generatePickler[LBBounds]
  implicit val pPoint= PicklerGenerator.generatePickler[Point]
  implicit val pLine = PicklerGenerator.generatePickler[Line]

  pGeometricFigure
    .addConcreteType[LTBounds]
    .addConcreteType[LBBounds]
    .addConcreteType[Point]
    .addConcreteType[Line]

  // implicit val pSharedJs = compositePickler[SharedJs]
  implicit val pPageGeometry = PicklerGenerator.generatePickler[PageGeometry]
  implicit val pTargetRegion = PicklerGenerator.generatePickler[TargetRegion]
  implicit val pTargetFigure = PicklerGenerator.generatePickler[TargetFigure]
  implicit val pLabel = PicklerGenerator.generatePickler[Label]
  implicit val pZone = PicklerGenerator.generatePickler[Zone]
  // implicit val pComponent = PicklerGenerator.generatePickler[Component]

  // pSharedJs
  //   .addConcreteType[PageGeometry]
  //   .addConcreteType[TargetRegion]
  //   .addConcreteType[TargetFigure]
  //   .addConcreteType[Label]
  //   .addConcreteType[Zone]
  //   .addConcreteType[Component]

  // import tracing._
  // import TraceLog._
  // implicit val pDSL = compositePickler[TraceLog]

  // implicit val pSetPageGeometry = PicklerGenerator.generatePickler[SetPageGeometries]
  // implicit val pShow            = PicklerGenerator.generatePickler[Show]
  // implicit val pShowZone        = PicklerGenerator.generatePickler[ShowZone]
  // // implicit val pShowComponent   = PicklerGenerator.generatePickler[ShowComponent]
  // // implicit val pShowLabel       = PicklerGenerator.generatePickler[ShowLabel]
  // implicit val pFocusOn         = PicklerGenerator.generatePickler[FocusOn]
  // implicit val pIndicate        = PicklerGenerator.generatePickler[Indicate]
  // // implicit val pMessage         = PicklerGenerator.generatePickler[Message]
  // implicit val pAll             = PicklerGenerator.generatePickler[All]
  // implicit val pLink            = PicklerGenerator.generatePickler[Link]
  // implicit val pGroup           = PicklerGenerator.generatePickler[Group]
  // implicit val pGroupEnd        = PicklerGenerator.generatePickler[GroupEnd]

  // pDSL
  //   .addConcreteType[SetPageGeometries]
  //   .addConcreteType[Show]
  //   .addConcreteType[ShowZone]
  //   .addConcreteType[FocusOn]
  //   .addConcreteType[Indicate]
  //   .addConcreteType[All]
  //   .addConcreteType[Link]
  //   .addConcreteType[Group]
  //   .addConcreteType[GroupEnd]
    // .addConcreteType[Message]
  // .addConcreteType[ShowComponent]
  // .addConcreteType[ShowLabel]

}
