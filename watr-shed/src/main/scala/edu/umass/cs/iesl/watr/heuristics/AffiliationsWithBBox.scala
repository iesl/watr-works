package edu.umass.cs.iesl.watr
package heuristics

import geometry.LTBounds

case class AffiliationsWithBBox(labName: ComponentRepresentation, departmentName: ComponentRepresentation, collegeName: ComponentRepresentation, institutionName: ComponentRepresentation,
                                institutionAddress: ComponentRepresentation, cityName: ComponentRepresentation, stateName: ComponentRepresentation, countryName: ComponentRepresentation,
                                zipcode: ComponentRepresentation, var bbox: LTBounds)
