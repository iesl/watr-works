package edu.umass.cs.iesl.watr
package textboxing



object Boxter {

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
}


object BoxExample extends App {

  /*

   Use case sketches:

   // text layout
   box(
     box("abc"),
     box("def") beside "jkl"
   )




   // Connected component ADT
   val B = Boxter[PageAtom]; import B._
   // a"" = atom("")

   a"H", a"2", a"S", a"O" a"4"

   layers(

   )

   */
}
