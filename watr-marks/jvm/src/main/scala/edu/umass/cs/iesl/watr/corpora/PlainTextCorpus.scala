package edu.umass.cs.iesl.watr
package corpora

// TODO plaintext corpus is primarily a testing util, but has a mix of testing and production code,
//   which need to separate

import textreflow._
import watrmarks.{StandardLabels => LB}
import TypeTags._

trait PlainTextCorpus extends TextReflowSharedFunctions {
  import scala.collection.mutable
  import scalaz.Tree
  import scalaz.TreeLoc
  import matryoshka._
  import geometry._

  import GeometryImplicits._
  import TextReflowF._

  def docStore: DocumentCorpus

  val xscale = 10.0d
  val yscale = 10.0d

  def lines(str: String): Seq[String] = {
    str.split("\n")
      .map(_.trim)
  }
  def linesWithPad(str: String): Seq[(Int, String)] = {
    str.split("\n")
      .map({s =>
             val pre = s.takeWhile(_ == ' ').length
             val line = s.trim
             (pre, line)
           })
  }

  val ffi = 0xFB03.toChar
  val charSubs = Map(
    ffi -> "ffi",
    'ﬂ' -> "fl",
    'ﬆ' -> "st",
    'æ' -> "ae",
    'Æ' -> "AE"
  )

  class TextReflowBuilder {
    import utils.ScalazTreeImplicits._
    type TextReflowV = TextReflowF[Unit]

    val reflowStack = mutable.Stack[TreeLoc[TextReflowV]]()
    val completed = mutable.Queue[TextReflow]()

    def reset(): Unit = {
      while (reflowStack.nonEmpty) reflowStack.pop()
    }

    def totalBounds(): LTBounds = {
      completed
        .map(_.targetRegion.bbox)
        .reduce { _ union _ }
    }


    val empty: Tree[TextReflowV] = Tree.Leaf(Flow(List()))
    def mod(f: TreeLoc[TextReflowV] => TreeLoc[TextReflowV]): Unit = {
      if (reflowStack.isEmpty) {
        reflowStack.push(empty.loc)
      }
      val top = reflowStack.pop

      reflowStack.push(f(top))
    }

    import scalaz.std.string._
    def debug(treeLoc: TreeLoc[TextReflowV]): Unit = {
      println(treeLoc.toTree.map(_.toString).drawBox)
    }

    def insertDownLast(tr: TextReflowV): Unit = mod {  _.insertDownLast(Tree.Node(tr, Stream())) }
    def pop(): Unit = mod { _.parent.get }

    def newline(): Unit = {
      reflowStack
        .headOption
        .map({tloc =>
          // Construct the Fix(..) version of the tree:
          val ftree = tloc.toTree
          val res = ftree.scanr ((reflowNode: TextReflowV, childs: Stream[Tree[TextReflow]]) =>
            fixf {
              reflowNode match {
                case t@ Atom(c)                 => Atom(c)
                case t@ Insert(value)           => Insert(value)
                case t@ Rewrite(from, to)       => Rewrite(childs.head.rootLabel, to)
                case t@ Bracket(pre, post, a)   => Bracket(pre, post, childs.head.rootLabel)
                case t@ Flow(atoms)             => Flow(childs.toList.map(_.rootLabel))
                case t@ Labeled(ls, _)          => Labeled(ls, childs.head.rootLabel)
              }}
          )
          val treflow = res.rootLabel
          completed.enqueue(treflow)
          reset()
        })

    }
  }


  def mkTargetRegion(pageId: Int@@PageID, x: Int, y: Int, w: Int, h: Int): TargetRegion = {
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

    val regionId = docStore.addTargetRegion(pageId, bbox)
    docStore.getTargetRegion(regionId)
  }

  def addDocument(stableId: String@@DocumentID, pages:Seq[String]): Unit  = {
    for {
      (page, n) <- pages.zipWithIndex
    } yield {
      loadPageFromString(stableId, PageNum(n), page)
    }
  }

  def loadPageFromString(
    stableId: String@@DocumentID,
    pageNum: Int@@PageNum,
    pageBlock: String
  ): Unit = {
    val docId = docStore.addDocument(stableId)
    val pageId = docStore.addPage(docId, pageNum)

    var linenum:Int = -1
    var chnum = 0
    var reflowBuilder = new TextReflowBuilder

    val pageLines = linesWithPad(pageBlock)

    for {
      (pad, line) <- pageLines
    } {
      linenum += 1
      chnum = pad
      reflowBuilder.newline()

      for {
        chpair <- (line+" ").sliding(2)
      } {

        chpair match {
          case "^{" =>
            reflowBuilder.insertDownLast(Labeled(Set(LB.Sup), ()))
            reflowBuilder.insertDownLast(Flow(List()))

          case "_{" =>
            reflowBuilder.insertDownLast(Labeled(Set(LB.Sub), ()))
            reflowBuilder.insertDownLast(Flow(List()))

          case chs if chs.nonEmpty =>
            chnum += 1

            chs(0) match {
              case '{' =>
                chnum -= 1

              case '}' =>
                chnum -= 1
                reflowBuilder.pop()
                reflowBuilder.pop()

              case ' ' =>
                reflowBuilder.insertDownLast(Insert(" "))
                reflowBuilder.pop()

              case ch  =>
                if (charSubs.contains(ch)) {
                  reflowBuilder.insertDownLast(Rewrite((), charSubs(ch)))
                }
                val charAtom = CharAtom(
                  mkTargetRegion(pageId, x=chnum, y=linenum, w=1, h=1),
                  ch.toString
                )
                reflowBuilder.insertDownLast(Atom(charAtom))
                docStore.addCharAtom(pageId, charAtom)
                reflowBuilder.pop()
            }
            case x => println(s"error: ${x}")
        }
      }
    }

    reflowBuilder.newline()
    reflowBuilder.completed.foreach { reflow =>
      val tt = reflow.toText
      println(s"adding textreflow: ${tt}")
      reflow.charAtoms.foreach{ca =>
        println(s"    ${ca}")
      }

      val lineRegion = reflow.targetRegion()
      val lineZone = docStore.createZone(docId)
      val regionId = docStore.addTargetRegion(pageId, lineRegion.bbox)
      val tr = docStore.getTargetRegion(regionId)
      docStore.setZoneTargetRegions(lineZone, Seq(tr))
      docStore.setTextReflowForZone(lineZone, reflow)
      docStore.addZoneLabel(lineZone, LB.VisualLine)
    }

    docStore.setPageGeometry(pageId, reflowBuilder.totalBounds())
  }





  def stringToPageAtoms(str: String, pageNum: Int, stableId: String@@DocumentID): (Seq[PageAtom], PageGeometry) = {
    val docId = docStore.addDocument(stableId)
    val pageId = docStore.addPage(docId, PageNum(pageNum))

    for {
      (line, linenum) <- lines(str).zipWithIndex
      (ch, chnum)     <- line.zipWithIndex
    } yield {
      val bbox = LTBounds(
        left=chnum*xscale, top=linenum*yscale,
        width=xscale, height=yscale
      )

      val regionId = docStore.addTargetRegion(pageId, bbox)
      val tr = docStore.getTargetRegion(regionId)
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

            val regionId = docStore.addTargetRegion(pageId, bbox)
            val tr = docStore.getTargetRegion(regionId)
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

  def stringsToMultiPageAtoms(stableId: String@@DocumentID, strs: String*): Seq[(Seq[PageAtom], PageGeometry)] = {
    for {
      (pstr, pagenum) <- strs.zipWithIndex
    } yield {
      stringToPageAtoms(pstr, pagenum, stableId)
    }
  }

}
