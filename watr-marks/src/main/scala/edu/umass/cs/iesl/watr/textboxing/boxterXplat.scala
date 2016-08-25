package edu.umass.cs.iesl.watr
package textboxing


object Boxter {


  /*
   To replace ConnectedComponents, needs to implement:
   - ID per box
   - Char-level granularity at leaf, with, perhaps,
     parameterized leaf components (PageAtoms, images, whitespace, anything other than chars/strings)
   - extend alignment to include super/sub/stacked etc. formatting hints
   - label boxes? or else just have IDs, and labeling is external
   - map/fold/etc
   */

  case class Box(rows:Int, cols:Int, content: Content)

  sealed trait Alignment

  case object AlignFirst extends Alignment
  case object AlignLast extends Alignment
  case object AlignCenter1 extends Alignment
  case object AlignCenter2 extends Alignment

  sealed trait Content

  case object Blank extends Content
  case class Text(s:String) extends Content
  case class Row(bs:Seq[Box]) extends Content
  case class Col(bs:Seq[Box]) extends Content
  case class SubBox(a1: Alignment, a2: Alignment, b:Box) extends Content
  case class AnnotatedBox(props:Map[String, String], b:Box) extends Content

  case class Layers(b: Box) extends Content

}


object BoxExample extends App {

  /*

   Use case sketches:

   // text layout
   box(
     box("abc"),
     box("def") beside "jkl"
   )


   // Html page layout templates:

   // Labeled CCs reflowed/formatted




   // Connected component ADT
   val B = Boxter[PageAtom]; import B._
   // a"" = atom("")

   // Represent a tokenized formula

   // Unconnected chars:
   'H', '2', 'S', 'O', '4'

   layers(
     text('2'),
     label("sub")
   )

   // gathered into row:
   row('H', '2', 'S', 'O', '4')

   // Super/sub (as cols/rows)
   row(
   'H', '2', 'S', 'O', '4')

   // Tokenized as formula
   row(:formula:
     tok(:mol:
       tok(:elem: 'H'),
       col(:ss: row(), row('2')),
       tok('S'), tok('O'),
       col(:ss: row(), row('4'))
     )
   )


   */
}
