package edu.umass.cs.iesl.watr
package watrcolors
package server


// object SharedTypeConversions {
//   import geometry._

//   import GeometricFigure._
//   import TypeTags._


//   implicit class RicherPoint(val in: spindex.GeometricFigure.Point) extends AnyVal {
//     def convert() = Point(in.x, in.y)
//   }
//   implicit class RicherLTBounds(val in: spindex.GeometricFigure.LTBounds) extends AnyVal {
//     def convert() = LTBounds(in.left, in.top, in.width, in.height)
//   }
//   implicit class RicherLBBounds(val in: spindex.GeometricFigure.LBBounds) extends AnyVal {
//     def convert() = LBBounds(in.left, in.bottom, in.width, in.height)
//   }

//   implicit class RicherLine(val in: spindex.GeometricFigure.Line) extends AnyVal {
//     def convert(): Line = Line(in.p1.convert(), in.p2.convert())
//   }

//   def convertFigure(in: spindex.GeometricFigure): GeometricFigure = convert(in)

//   def convert(in: spindex.GeometricFigure): GeometricFigure = {
//     in match {
//       case v: spindex.GeometricFigure.LTBounds => v.convert()
//       case v: spindex.GeometricFigure.LBBounds => v.convert()
//       case v: spindex.GeometricFigure.Point    => v.convert()
//       case v: spindex.GeometricFigure.Line     => v.convert()
//     }
//   }


//   implicit class RicherPageGeometry(val in: spindex.PageGeometry) extends AnyVal {
//     def convert(): PageGeometry = PageGeometry(
//       in.id.unwrap,
//       in.bounds.convert
//     )
//   }
//   implicit class RicherTargetFigure(val in: spindex.TargetFigure) extends AnyVal {
//     def convert(): TargetFigure = TargetFigure(
//       in.id.unwrap,
//       in.page.unwrap,
//       convertFigure(in.figure)
//     )
//   }

//   implicit class RicherTargetRegion(val in: spindex.TargetRegion) extends AnyVal {
//     def convert(): TargetRegion = TargetRegion(
//       in.id.unwrap,
//       in.target.unwrap,
//       in.bbox.convert()
//     )
//   }
//   implicit class RicherZone(val in: spindex.Zone) extends AnyVal {
//     def convert(): Zone = Zone(
//       in.id.unwrap,
//       in.regions.map(_.convert())
//     )
//   }

//   import watrmarks.{StandardLabels => LB}

//   implicit class RicherComponent(val in: spindex.Component) extends AnyVal {

//     def convert(): Component = {
//       // val content: Option[String] = if (in.getLabels.contains(LB.TokenizedLine)) {
//       //   Some(in.toText)
//       // } else {
//       //   None
//       // }

//       Component(
//         in.id.unwrap,
//         in.targetRegion.convert,
//         None // content
//       )
//     }
//   }


//   implicit class RicherLabel(val in: watrmarks.Label) extends AnyVal {
//     def convert(): watrcolors.Label = watrcolors.Label(in.ns, in.key, in.value)
//   }
// }
