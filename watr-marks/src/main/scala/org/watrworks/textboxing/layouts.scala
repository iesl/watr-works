package org.watrworks
package textboxing


import textboxing.{TextBoxing => TB}, TB._

trait ShowBox[F]  { self =>
  def show(f: F): TB.Box = shows(f)
  def shows(f: F): TB.Box = show(f)
}
object ShowBox {
  @inline def apply[F](implicit F: ShowBox[F]): ShowBox[F] = F


  def showFromToString[A]: ShowBox[A] = new ShowBox[A] {
    override def shows(f: A): TB.Box = f.toString.box
  }

  def show[A](f: A => Box): ShowBox[A] = new ShowBox[A] {
    override def show(a: A): TB.Box = f(a)
  }

  def shows[A](f: A => String): ShowBox[A] = new ShowBox[A] {
    override def shows(a: A): TB.Box = f(a).box
  }

}

object TextBoxingLayouts {

  def hangingIndent(heading: String, items: Seq[Box]): Box = {
    heading.atop(indent(4,
      vjoins(items)
    ))
  }

  import scalaz._

  def boxedMap[K, V](m: Map[K, V])(implicit
    ShowBoxK: Need[ShowBox[K]],
    ShowBoxV: Need[ShowBox[V]],
  ): TB.Box = {
    val kvCols = m.map{ case (k, v) =>
      (ShowBoxK.value.show(k), ShowBoxV.value.show(v))
    }
    hjoin(
      vjoins(kvCols.map(_._1).toList),
      hspace(2),
      vjoins(kvCols.map(_._2).toList)
    )

  }
}
