
package edu.umass.cs.iesl.xml_annotator

/** Constructors for labels
  *
  * They are used by Annotator instances for annotating characters of text
  */
sealed trait Label
case class B(c: Char) extends Label
case object I extends Label
case object O extends Label
case object L extends Label
case class U(c: Char) extends Label


sealed trait Constraint

/** Constructor for char constraints
  *
  * It is used by Annotator instances to constrain labels of annotation types
  * to the the primitive unit of characters
  */
case object CharCon extends Constraint

/** Constructor for segment constraints
  *
  * It is used by Annotator instances to constrain labels of annotation types
  * to the index pairs containing the B or U labels of other annotation types
  */
case class SegmentCon(annotationTypeName: String) extends Constraint

sealed trait ConstraintRange


/** Constructor to create a constraint range
  *
  * It is used by Annotator instances to hold an annotation type string
  * and a constraint such that the annotation type is constrained by the constraint
  * or is constrained by a SegmentCon whose annotation type has the qualities
  * of the previously mentioned annotation type string
  */
case class Range(annoTypeName: String, con: Constraint) extends ConstraintRange


/** Constructor to make a constraint range consisting of just a single constraint **/
case class Single(constraint: Constraint) extends ConstraintRange
