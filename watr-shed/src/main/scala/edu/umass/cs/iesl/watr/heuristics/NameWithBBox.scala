package edu.umass.cs.iesl.watr
package heuristics

import geometry.LTBounds

case class NameWithBBox(
                           firstName: ComponentRepresentation,
                           middleName: ComponentRepresentation,
                           lastName: ComponentRepresentation,
                           hereditySuffix: ComponentRepresentation,
                           degree: ComponentRepresentation,
                           var bBox: LTBounds)
