package edu.umass.cs.iesl.watr.watrmarks


sealed trait BioPin {
  def label: BioLabel
  def pinChar: Char
  override def toString = s"<${pinChar}::${label.namespace}/${label.name}>"
}

case class BPin(label: BioLabel, override val pinChar:Char='B') extends BioPin
case class IPin(label: BioLabel, override val pinChar:Char='I') extends BioPin
case class OPin(label: BioLabel, override val pinChar:Char='O') extends BioPin
case class LPin(label: BioLabel, override val pinChar:Char='L') extends BioPin
case class UPin(label: BioLabel, override val pinChar:Char='U') extends BioPin

class BioLabel(val namespace: String, val name: String, val c: Char, val constraint: Constraint) {
  lazy val B = BPin(this)
  lazy val I = IPin(this)
  lazy val O = OPin(this)
  lazy val L = LPin(this)
  lazy val U = UPin(this)
}


trait BioLabelDictionary {
  def apply(c: Char): BioLabel
  def get(s: Char): Option[BioLabel]

  def apply(s: String): BioLabel
  def get(s: String): Option[BioLabel]
}

case class BioDictionary(
  byName: Map[String, BioLabel],
  byChar: Map[Char, BioLabel]
) extends BioLabelDictionary {

  def apply(s: String) = byName(s)
  def get(s: String): Option[BioLabel] = byName.get(s)

  def apply(s: Char) = byChar(s)
  def get(s: Char): Option[BioLabel] = byChar.get(s)
}


object BioLabel {

  def apply(ns: String, name: String, c: Char, constraint: BioLabel) =
    new BioLabel(ns, name, c, BioConstraint(constraint))

  def apply(ns: String, name: String) =
    new BioLabel(ns, name, name(0), CharConstraint)

  def apply(name: String) =
    new BioLabel(name, name, name(0), CharConstraint)

}

case class FontDictionary(
  dict: Map[String, FontInfo] = Map()
)

case class FontInfo(
  // fontName: String,
  fontFamily: String,
  fontSize: String
)

case class TextBounds(
  left: Double,
  bottom: Double,
  width: Double,
  height: Double
)

case class LabeledColumn(
  labels: Set[BioPin] = Set(),
  char: Char,
  font: Option[FontInfo],
  bounds: Option[TextBounds]
)

case class LabeledSpan(
  columns: List[LabeledColumn] = List()
)


case class LabeledLocation(
  current: List[LabeledColumn],
  prevs: List[LabeledColumn] = List(),
  nexts: List[LabeledColumn] = List()
) {
  def next(label: BioLabel): LabeledLocation = {
    LabeledLocation(current, prevs, nexts)
  }

  def addLabel(label: BioLabel): LabeledLocation = {
    this
  }

  def toLabeledSpan = LabeledSpan(
    prevs.reverse ++ current ++ nexts
  )

}


object biolu {

  def parseBioLine(bioLine: String, continueChar: Option[Char], dict: BioLabelDictionary): Map[Int, BioPin] = {
    var currChar = continueChar.getOrElse(' ')
    def currLabel = dict(currChar.toLower)

    (bioLine
      .toIndexedSeq.zipWithIndex
      .filter(p => p._1 != ' ').map {
      case (c, i) =>
        i -> (c match {
          case '-' => currLabel.O
          case '~' => currLabel.I
          case '$' => currLabel.L
          case t if t.isLower => currChar = t; currLabel.B
          case t if t.isUpper => currChar = t; currLabel.U
          case x  => sys.error(s"no mapping for char '${x}'")

        })
    }).toMap
  }

  def parseBioTypes(typeString: String): List[(String, Char)] = {
    typeString.split(", ").map(pairString => {
      val Array(str, c) = pairString.split(": ")
      (str, c.toCharArray()(0))
    }).toList
  }

  def parseBioConstraints(constraintString: String): ConstraintRange = {
    val typeList = constraintString.split('.')
    val size = typeList.size
    val lastType = typeList(size - 1)
    val con = if (lastType == "char") {
      CharCon
    } else {
      SegmentCon(lastType)
    }

    if (size == 1) {
      Single(con)
    } else {
      Range(typeList(0), con)
    }
  }


  def parseBioBlock(
    blockString: String,
    dict: BioLabelDictionary,
    maybeText: Option[String],
    maybeBounds: Option[List[TextBounds]] = None,
    maybeFontInfo: Option[List[FontInfo]] = None
  ): LabeledSpan = {

    val bioBlock = bioParsers.parseBioBlock(blockString)
      .left.map(err => sys.error(s"error parsing ${blockString}: $err"))
      .right.get

    val p = bioBlock.pinRows
      .map{ pinrow => parseBioLine(pinrow.biostr, None, dict) }

    val allpins = p.toList
      .flatten
      .groupBy(_._1)
      .map{ case (k, v) => k -> v.map(_._2).toSet }
      .toMap

    val text = (bioBlock.text.map(_.text) orElse maybeText)
      .getOrElse(sys.error("no text provided or found in bio block"))
      .toCharArray

    val bounds  = maybeBounds
      .map(_.map(Some(_)))
      .getOrElse { List.fill(text.length)(None:Option[TextBounds]) }

    val fontInfos  = maybeFontInfo
      .map(_.map(Some(_)))
      .getOrElse { List.fill(text.length)(None:Option[FontInfo]) }

    val cols = text
      .zip(bounds)
      .zip(fontInfos)
      .zipWithIndex
      .map{ case(((c, bound), fontInfo), i) =>
        LabeledColumn(
          allpins.getOrElse(i, Set()),
          c, fontInfo, bound
        )
    }


    LabeledSpan(cols.toList)

    //   val labelMap = parseBioBricks(labelStr)
    //   val typePairList = parseBioTypes(defDict)


    // fullPattern.r.findAllIn(blockString).toList.reverse.map(spanString => {
    //   println("here!")
    //   spanString match {
    //     case fullPattern.r(labelString, typeString, _, constraintString, _) =>
    //       println("here(full)!")

    //       val labelMap = parseBioBricks(labelString)
    //       val typePairList = parseBioTypes(typeString)
    //       val constraintRange = parseBioConstraints(constraintString)
    //       (labelMap, typePairList, constraintRange)
    //     case _ =>
    //       println("here _!")
    //       sys.error("")
    //   }

    // })

  }

}




/** Constructors for labels
  * They are used by Annotator instances for annotating characters of text
  */
sealed trait Label
case class B(c: Char) extends Label
case object I extends Label
case object O extends Label
case object L extends Label
case class U(c: Char) extends Label


sealed trait Constraint

// /** Constructor for char constraints
//   *
//   * It is used by Annotator instances to constrain labels of annotation types
//   * to the the primitive unit of characters
//   */
case object CharCon extends Constraint
case object CharConstraint extends Constraint

// /** Constructor for segment constraints
//   *
//   * It is used by Annotator instances to constrain labels of annotation types
//   * to the index pairs containing the B or U labels of other annotation types
//   */
case class SegmentCon(annotationTypeName: String) extends Constraint
case class BioConstraint(label: BioLabel) extends Constraint


// @deprecated("","")
sealed trait ConstraintRange


/** Constructor to create a constraint range
  *
  * It is used by Annotator instances to hold an annotation type string
  * and a constraint such that the annotation type is constrained by the constraint
  * or is constrained by a SegmentCon whose annotation type has the qualities
  * of the previously mentioned annotation type string
  */
@deprecated("","")
case class Range(annoTypeName: String, con: Constraint) extends ConstraintRange


/** Constructor to make a constraint range consisting of just a single constraint **/
@deprecated("","")
case class Single(constraint: Constraint) extends ConstraintRange
