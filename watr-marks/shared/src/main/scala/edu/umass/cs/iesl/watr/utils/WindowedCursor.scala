package edu.umass.cs.iesl.watr
package utils

import scalaz.{@@ => _, _} , Scalaz._

import scala.annotation.tailrec
object Cursors {



  def groupByWindow[A](f: (Seq[A], A) => Boolean, as: Seq[A]): Seq[Seq[A]] = {

    @tailrec
    def loop(maybeCursor: Option[Cursor[A]], acc: List[Seq[A]]): Seq[Seq[A]] = {
      maybeCursor match {
        case Some(cursor) =>
          val window = cursor.toWindow
          val group = window.slurpRight(f)
          val acc1 = group.cells :: acc
          println(s"loop = C: ${cursor.debugString()}")
          println(s"       W: ${group.debugString()}")
          println(s"     Acc: ${acc1}")
          loop(group.nextCursor(), acc1 )

        case None =>
          acc
      }

    }
    loop(Cursor.init(as), List()).reverse

  }
}

object Cursor {
  def init[A](z: Zipper[A]) = new Cursor[A] {
    def zipper: Zipper[A] = z
  }

  def init[A](optZ: Option[Zipper[A]]): Option[Cursor[A]] =
    optZ.map{ z =>
      new Cursor[A] {
        def zipper: Zipper[A] = z
      }
    }

  def init[A](cells: Seq[A]): Option[Cursor[A]] =
    init[A](cells.toList.toZipper)

}

trait Cursor[A] { self =>
  def zipper: Zipper[A]

  def focus: A = zipper.focus

  def next: Option[Cursor[A]] =
    Cursor.init[A]{ zipper.next }

  def prev: Option[Cursor[A]] =
    Cursor.init[A]{ zipper.previous }

  def insertLeft(g: A): Cursor[A] =
    Cursor.init[A]{ zipper.insertLeft(g)}

  def insertRight(g: A): Cursor[A] =
    Cursor.init[A]{ zipper.insertRight(g)}


  def atStart: Boolean = zipper.atStart
  def atEnd: Boolean = zipper.atEnd

  def start: Cursor[A] = Cursor.init[A]{ zipper.start }
  def end: Cursor[A] = Cursor.init[A]{ zipper.end }

  def toWindow(): Window[A] = {
    Window(Seq(focus), zipper.lefts, zipper.focus, zipper.rights)
  }

  def move(n: Int): Option[Cursor[A]] =
    Cursor.init[A]{ zipper.move(n) }

  def slurpRight(p: A => Boolean): Window[A] = {
    val (winTail, tail) = zipper.rights.span(p)
    val window = zipper.focus +: winTail
    Window(window, zipper.lefts, zipper.focus, tail)
  }

  def findNext(p: A => Boolean): Option[Cursor[A]] = {
    Cursor.init[A]{ zipper.findNext(p) }
  }

  def findPrevious(p: A => Boolean): Option[Cursor[A]] = {
    Cursor.init[A]{ zipper.findPrevious(p) }
  }

  def debugString(): String = {
    val ls = zipper.lefts.toList.reverse.mkString(", ")
    val rs = zipper.rights.toList.mkString(", ")
    val f = zipper.focus
    s"""Cursor($ls [${f}] $rs)"""
  }
}




object Window {
  def apply[A](window: Seq[A], lefts: Stream[A], focus: A, rights:Stream[A]): Window[A] = {
    new Window[A] {
      def cells: Seq[A] = window

      def winStart: Zipper[A] =
        Zipper.zipper(lefts, focus, rights)
    }
  }
}



sealed trait Window[A] { self =>
  def winStart: Zipper[A]
  def cells: Seq[A]

  def atStart: Boolean = winStart.atStart
  def atEnd: Boolean = winStart.atEnd

  def debugString(): String = {
    val rs = winStart.rights.map(_.toString()).mkString(", ")
    val ls = winStart.lefts.map(_.toString()).mkString(", ").reverse
    val ws = cells.map(_.toString()).mkString(", ")
    s"""Window($ls [${ws}] $rs)"""
  }

  def closeWindow(): Cursor[A] = {
    val newLefts = cells.init.reverse.toStream ++ winStart.lefts
    val newFocus = cells.last

    Cursor.init[A]{
      Zipper.zipper(newLefts, newFocus, winStart.rights)
    }
  }

  def nextCursor(): Option[Cursor[A]] = {
    Cursor.init[A]{
      winStart.next.map{znext =>
        Zipper.zipper(
          cells.reverse.toStream ++ znext.lefts,
          znext.focus,
          znext.rights
        )
      }
    }
  }

  def widen(n: Int): Window[A] = {
    val (as, bs) = winStart.rights.splitAt(n)

    Window(cells++as, winStart.lefts, winStart.focus, bs)
  }

  private def rinits(as: Stream[A]): Seq[Seq[A]]= {
    (1 to as.length) map { as.slice(0, _) }
  }

  def slurpRight(p: (Seq[A], A) => Boolean): Window[A] = {

    val slurped = rinits(winStart.rights)
      .map{ init =>
        val nextWin = cells ++ init
        (nextWin.init, nextWin.last)
      }
      .takeWhile { case (h, t) => p(h, t) }

    val slcells = slurped.map{case (ws, t) => ws :+ t}.toList
      .lastOption
      .getOrElse { cells }

    Window(slcells, winStart.lefts, winStart.focus, winStart.rights.drop(slurped.length))

  }

  def extendRight[B](f: A => A): Window[A] = {
    val ins = f(winStart.focus)
    Window(
      cells :+ ins,
      winStart.lefts, winStart.focus, winStart.rights
    )

  }
}
