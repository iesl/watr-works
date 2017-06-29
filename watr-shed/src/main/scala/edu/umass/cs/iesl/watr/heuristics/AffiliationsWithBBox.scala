package edu.umass.cs.iesl.watr
package heuristics

import geometry.LTBounds

import scala.collection.mutable.ListBuffer

case class AffiliationsWithBBox(
                                   var departments: ListBuffer[ComponentRepresentation],
                                   var institutions: ListBuffer[ComponentRepresentation],
                                   var university: ComponentRepresentation,
                                   var company: ComponentRepresentation,
                                   var addresses: ListBuffer[ComponentRepresentation],
                                   var city: ComponentRepresentation,
                                   var regions: ListBuffer[ComponentRepresentation],
                                   var country: ComponentRepresentation,
                                   var zipCode: ComponentRepresentation,
                                   var bBoxes: ListBuffer[LTBounds]
                               )
