package edu.umass.cs.iesl.watr
package heuristics

import geometry.LTBounds

case class NameRepresentation(var nameText: String, var bbox: LTBounds)

case class NameWithBBox(firstName: NameRepresentation, middleName: NameRepresentation, lastName: NameRepresentation, var bbox: LTBounds)
