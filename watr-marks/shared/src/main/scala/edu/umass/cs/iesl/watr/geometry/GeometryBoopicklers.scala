package edu.umass.cs.iesl.watr
package geometry

import boopickle._
import boopickle.DefaultBasic._

trait GeometryBoopicklers extends PicklerHelper {
  import PicklerGenerator._

  import watrmarks._
  import geometry._


  implicit val pGeometricFigure = compositePickler[GeometricFigure]
  implicit val pLTBounds        = generatePickler[LTBounds]
  implicit val pLBBounds        = generatePickler[LBBounds]
  implicit val pPoint           = generatePickler[Point]
  implicit val pLine            = generatePickler[Line]

  pGeometricFigure
    .addConcreteType[LTBounds]
    .addConcreteType[LBBounds]
    .addConcreteType[Point]
    .addConcreteType[Line]

  implicit val pPageGeometry = generatePickler[PageGeometry]
  implicit val pTargetRegion = generatePickler[TargetRegion]
  implicit val pTargetFigure = generatePickler[TargetFigure]
  implicit val pLabel        = generatePickler[Label]
  implicit val pZone         = generatePickler[Zone]


  implicit val p02 = transformPickler[CharAtom, (TargetRegion, String, Int)](
    t => new CharAtom(t._1, t._2, if(t._3==0) None else Some(t._3)))(
    t => (t.targetRegion, t.char, t.wonkyCharCode.getOrElse(0)))

}
