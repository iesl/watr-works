package org.watrworks
package utils

import scalaz.{@@ => _, _}, Scalaz._

// import scala.{collection => sc}
// import sc.Seq

import scala.annotation.tailrec
import scala.annotation.nowarn

/**
  *
  */
object Cursors {

  def groupByWindow[A](f: (Seq[A], A) => Boolean, as: Seq[A]): Seq[Seq[A]] = {

    @tailrec
    def loop(maybeCursor: Option[Cursor[A]], acc: List[Seq[A]]): Seq[Seq[A]] = {
      maybeCursor match {
        case Some(cursor) =>
          val window = cursor.toWindow()
          val group = window.slurpRight(f)
          val acc1 = group.cells :: acc
          // println(s"loop = C: ${cursor.debugString()}")
          // println(s"       W: ${group.debugString()}")
          // println(s"     Acc: ${acc1}")
          loop(group.nextCursor(), acc1)

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
  def init[A](optZ: Maybe[Zipper[A]]): Option[Cursor[A]] =
    init(optZ.toOption)

  def init[A](optZ: Option[Zipper[A]]): Option[Cursor[A]] =
    optZ.map(init(_))

  def init[A](cells: Seq[A]): Option[Cursor[A]] = {
    init[A](cells.toList.toZipper)
  }

}

trait Cursor[A] { self =>
  protected def zipper: Zipper[A]

  def focus: A = zipper.focus

  def next: Option[Cursor[A]] =
    Cursor.init[A] { zipper.next }

  def prev: Option[Cursor[A]] =
    Cursor.init[A] { zipper.previous }

  def insertLeft(g: A): Cursor[A] =
    Cursor.init[A] { zipper.insertLeft(g) }

  def insertRight(g: A): Cursor[A] =
    Cursor.init[A] { zipper.insertRight(g) }

  def toList(): List[A] = zipper.toList

  def atStart: Boolean = zipper.atStart
  def atEnd: Boolean = zipper.atEnd

  def start: Cursor[A] = Cursor.init[A] { zipper.start }
  def end: Cursor[A] = Cursor.init[A] { zipper.end }

  def toWindow(): Window[A] = {
    Window(
      Seq(focus),
      zipper.lefts.to(LazyList),
      zipper.focus,
      zipper.rights.to(LazyList)
    )
  }

  def move(n: Int): Option[Cursor[A]] =
    Cursor.init[A] { zipper.move(n) }

  def slurpRight(p: A => Boolean): Window[A] = {
    val (winTail, tail) = zipper.rights.to(LazyList).span(p)
    val window = zipper.focus +: winTail
    Window(window, zipper.lefts.to(LazyList), zipper.focus, tail)
  }

  def findNext(p: A => Boolean): Option[Cursor[A]] = {
    Cursor.init[A] { zipper.findNext(p) }
  }

  def findPrevious(p: A => Boolean): Option[Cursor[A]] = {
    Cursor.init[A] { zipper.findPrevious(p) }
  }

  @tailrec
  final def unfold(f: Cursor[A] => Option[Cursor[A]]): Cursor[A] = {
    f(self) match {
      case Some(cmod) =>
        cmod.next match {
          case Some(cnext) => cnext.unfold(f)
          case None        => cmod

        }
      case None => self
    }
  }

  def debugString(): String = {
    val ls = zipper.lefts.toList.reverse.mkString(", ")
    val rs = zipper.rights.to(LazyList).toList.mkString(", ")
    val f = zipper.focus
    s"""Cursor([$ls] [[ ${f} ]] [$rs])"""
  }
}



object Window {
  def apply[A](
    window: Seq[A],
    lefts: LazyList[A],
    focus: A,
    rights: LazyList[A]
  ): Window[A] = {
    new Window[A] {
      def cells: Seq[A] = window

      @nowarn
      def winStart: Zipper[A] =
        Zipper.zipper(lefts.to(Stream), focus, rights.to(Stream))
    }
  }
}

sealed trait Window[A] { self =>
  def winStart: Zipper[A]
  def cells: Seq[A]

  def atStart: Boolean = winStart.atStart
  def atEnd: Boolean = winStart.atEnd

  def foreach(f: A => A): Unit = {
    cells.foreach(f)
  }

  def debugString(): String = {
    val rs = winStart.rights.map(_.toString()).mkString(", ")
    val ls = winStart.lefts.map(_.toString()).mkString(", ").reverse
    val ws = cells.map(_.toString()).mkString(", ")
    s"""Window([$ls] [[ ${ws} ]] [$rs])"""
  }

  def closeWindow(): Cursor[A] = {
    @nowarn
    val newLefts = cells.init.reverse.to(Stream) ++ winStart.lefts
    val newFocus = cells.last

    Cursor.init[A] {
      Zipper.zipper(newLefts, newFocus, winStart.rights)
    }
  }

  def nextCursor(): Option[Cursor[A]] = {
    Cursor.init[A] {
      winStart.next.map { znext => {
        @nowarn
        val prevs = cells.reverse.to(Stream) ++ znext.lefts

        Zipper.zipper(
          prevs,
          znext.focus,
          znext.rights
        )
      }}
    }
  }

  def widen(n: Int): Option[Window[A]] = {
    if (winStart.rights.length < n) {
      val split = winStart.rights.splitAt(n)
      val as = split._1
      val bs = split._2
      Some(Window(cells ++ as, winStart.lefts.to(LazyList), winStart.focus, bs.to(LazyList)))
    } else None
  }

  private def rinits(as: LazyList[A]): Seq[Seq[A]] = {
    (1 to as.length) map { as.slice(0, _) }
  }


  def slurpRight(p: (Seq[A], A) => Boolean): Window[A] = {
    val slurped = rinits(winStart.rights.to(LazyList))
      .map { init =>
        val nextWin = cells ++ init
        (nextWin.init, nextWin.last)
      }
      .takeWhile {
        case (init, last) =>
          p(init, last)
      }

    val slcells =
      slurped.map { case (ws, t) => ws :+ t }.toList.lastOption.getOrElse {
        cells
      }

    Window(
      slcells,
      winStart.lefts.to(LazyList),
      winStart.focus,
      winStart.rights.drop(slurped.length).to(LazyList)
    )
  }

  def extendRight(a: => A): Window[A] = {
    Window(
      cells :+ a,
      winStart.lefts.to(LazyList),
      winStart.focus,
      winStart.rights.to(LazyList)
    )

  }
}
