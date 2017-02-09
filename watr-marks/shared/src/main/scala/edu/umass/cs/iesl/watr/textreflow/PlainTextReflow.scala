package edu.umass.cs.iesl.watr
package textreflow

// TODO plaintext reflow started as testing util, and still has a mix of testing and production code

import watrmarks.{StandardLabels => LB}
import TypeTags._

trait PlainTextReflow extends TextReflowSharedFunctions {
  import scala.collection.mutable
  import scalaz.std.string._
  import scalaz.Tree
  import utils.ScalazTreeImplicits._
  import matryoshka._
  import geometry._

  import GeometryImplicits._
  import PageComponentImplicits._
  import TextReflowF._

  def docStore: ReflowDocstore

  val xscale = 10.0d
  val yscale = 10.0d

  def lines(str: String): Seq[String] = {
    str.split("\n")
      .map(_.trim)
  }

  val ffi = 0xFB03.toChar
  val charSubs = Map(
    ffi -> "ffi",
    'ﬂ' -> "fl",
    'ﬆ' -> "st",
    'æ' -> "ae",
    'Æ' -> "AE"
  )


  def mkTargetRegion(docId: String@@DocumentID, pageNum: Int@@PageNum, x: Int, y: Int, w: Int, h: Int) = {
    // bbox areas (for non-empty bounding boxes) are a bit smaller than full 1x1 area
    val width = if (w>0) {
      w*xscale - 0.1
    } else 0
    val height = if (h>0) {
      h*yscale - 0.1
    } else 0

    val bbox = LTBounds(
      left=x*xscale, top=y*yscale,
      width, height
    )
    docStore.addTargetRegion(docId, pageNum, bbox)
  }

  def textReflowsToAtoms(
    docId: String@@DocumentID, pages: Seq[TextReflow]
  ): Seq[(Seq[PageAtom], PageGeometry)] = for {
    page <- pages
  } yield {
    val TargetRegion(id, docId, pageNum, bbox@ LTBounds(l, t, w, h) ) =
      page.targetRegions.reduce(_ union _)

    (page.charAtoms(), PageGeometry(pageNum, bbox))
  }

  def stringsToTextReflows(docId: String@@DocumentID, pages:Seq[String]): Seq[TextReflow] = {
    for {
      (page, n) <- pages.zipWithIndex
    } yield {
      stringToTextReflow(page)(docId, PageNum(n))
    }
  }

  def stringToTextReflow(multiLines: String)(
    docId: String@@DocumentID,
    pageNum: Int@@PageNum
  ): TextReflow = {
    val isMultiline = multiLines.contains("\n")
    val doc = docStore.addDocument(docId)
    val page = docStore.addPage(docId, pageNum)

    var tloc = if (isMultiline) {
      val t: Tree[TextReflowF[Int]] =
        Tree.Node(Labeled(Set(LB.PageLines), 0), Stream(
          Tree.Node(Flow(List()), Stream(
            Tree.Node(Labeled(Set(LB.VisualLine), 0), Stream(
              Tree.Leaf(Flow(List()))
            ))
          ))
        ))

      t.loc.lastChild.get.lastChild.get.lastChild.get
    } else {
      val t: Tree[TextReflowF[Int]] =
        Tree.Leaf(Flow(List()))
      t.loc
    }


    var linenum:Int = 0
    var chnum = 0
    var lineCharAtoms = mutable.ArrayBuffer[CharAtom]()


    def insertRight(tr: TextReflowF[Int]): Unit    = { tloc = tloc.insertRight(Tree.Leaf(tr)) }
    def insertLeft(tr: TextReflowF[Int]): Unit     = { tloc = tloc.insertLeft(Tree.Leaf(tr)) }
    def insertDownLast(tr: TextReflowF[Int]): Unit = { tloc = tloc.insertDownLast(Tree.Node(tr, Stream())) }
    def pop(): Unit = { tloc = tloc.parent.get }
    //
    def debug(): Unit = { println(tloc.toTree.map(_.toString).drawBox) }

    def createUriString(): String = {
      val z = mkTargetRegion(
        docId, pageNum,
        0, linenum, 0, 0
      )

      val accumLineTargetRegion = lineCharAtoms
        .map(_.targetRegion)
        .foldLeft(z)(_ union _)

      lineCharAtoms.clear()
      accumLineTargetRegion.uriString

    }

    for (ch <- lines(multiLines).mkString("\n")) {
      ch match {
        case '\n' =>

          val uriStr = createUriString()

          linenum += 1
          chnum = 0
          // update the VisualLine w/exact dimensions
          pop()

          tloc = tloc.modifyLabel(_ => Labeled(Set(LB.VisualLine(uriStr)), 0))

          pop()
          insertDownLast(Labeled(Set(LB.VisualLine), 0))
          insertDownLast(Flow(List()))


        case '^' => insertDownLast(Labeled(Set(LB.Sup), 0))
        case '_' => insertDownLast(Labeled(Set(LB.Sub), 0))
        case '{' => insertDownLast(Flow(List()))
        case '}' => pop(); pop()
        case ' ' =>
          insertDownLast(Insert(" "))
          pop()
          chnum += 1

        case chx if charSubs.contains(chx) =>
          insertDownLast(Rewrite(0, charSubs(chx)))
          val charAtom = CharAtom(
            mkTargetRegion(docId, pageNum,
              chnum, linenum, 1, 1),
            ch.toString
          )
          lineCharAtoms += charAtom
          insertDownLast(Atom(charAtom))
          pop()
          pop()
          chnum += 1

        case _ =>
          val charAtom = CharAtom(
            mkTargetRegion(docId, pageNum, chnum, linenum, 1, 1),
            ch.toString
          )
          lineCharAtoms += charAtom
          insertDownLast(Atom(charAtom))
          pop()
          chnum += 1
      }
    }

    if (!tloc.isRoot) {
      pop()
      val uriStr = createUriString()
      tloc = tloc.modifyLabel(_ => Labeled(Set(LB.VisualLine(uriStr)), 0))
    }

    // Now construct the Fix[] version of the tree:
    val ftree = tloc.toTree
    val res = ftree.scanr ((reflowNode: TextReflowF[Int], childs: Stream[Tree[TextReflow]]) => fixf {
      reflowNode match {
        case t@ Atom(c)                 => Atom(c)
        case t@ Insert(value)           => Insert(value)
        case t@ Rewrite(from, to)       => Rewrite(childs.head.rootLabel, to)
        case t@ Bracket(pre, post, a)   => Bracket(pre, post, childs.head.rootLabel)
        case t@ Flow(atoms)             => Flow(childs.toList.map(_.rootLabel))
        case t@ Labeled(ls, _)          => Labeled(ls, childs.head.rootLabel)
      }}
    )

    val finalTextReflow = res.rootLabel
    val bbox = finalTextReflow.targetRegion().bbox

    docStore.updatePageGeometry(docId, pageNum, bbox)
    finalTextReflow
  }



  def stringToPageAtoms(str: String, pageNum: Int, docId: String@@DocumentID): (Seq[PageAtom], PageGeometry) = {
    for {
      (line, linenum) <- lines(str).zipWithIndex
      (ch, chnum)     <- line.zipWithIndex
    } yield {
      val bbox = LTBounds(
        left=chnum*xscale, top=linenum*yscale,
        width=xscale, height=yscale
      )
      val tr = docStore.addTargetRegion(docId, PageNum(pageNum), bbox)
      CharAtom(tr, ch.toString)

    }

    val atoms = lines(str).zipWithIndex
      .map({ case (line, linenum) =>
        line.zipWithIndex
          .filterNot(_._1 == ' ')
          .map({ case (ch, chnum) =>
            val bbox = LTBounds(
              left=chnum*xscale, top=linenum*yscale,
              width=xscale, height=yscale
            )
            val tr = docStore.addTargetRegion(docId, PageNum(pageNum), bbox)
            CharAtom(tr, ch.toString)
          })
      })
      .flatten.toSeq

    val maxX = atoms.map(_.targetRegion.bbox.right).max
    val maxY = atoms.map(_.targetRegion.bbox.bottom).max


    val pageGeom = PageGeometry(
      PageNum(pageNum), LTBounds(
        left=0, top=0,
        width=maxX, height=maxY
      )
    )

    (atoms, pageGeom)
  }

  def stringsToMultiPageAtoms(docId: String@@DocumentID, strs: String*): Seq[(Seq[PageAtom], PageGeometry)] = {
    for {
      (pstr, pagenum) <- strs.zipWithIndex
    } yield {
      stringToPageAtoms(pstr, pagenum, docId)
    }
  }

}
