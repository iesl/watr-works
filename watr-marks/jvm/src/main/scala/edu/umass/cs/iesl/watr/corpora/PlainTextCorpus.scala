package edu.umass.cs.iesl.watr
package corpora


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

  val charIds = utils.IdGenerator[CharID]()
  val xscale = 10.0d
  val yscale = 10.0d

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
        .map(_.bounds)
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
          val txt = treflow.toText().trim
          if (txt.length>0) {
            completed.enqueue(treflow)
          }
          reset()
        })

    }
  }

  def getRegionBounds(x: Int, y: Int, w: Int, h: Int): LTBounds = {
    // val width = if (w>0) {
    //   w*xscale - 0.1
    // } else 0
    // val height = if (h>0) {
    //   h*yscale - 0.1
    // } else 0

    val width = w * xscale
    val height = h*yscale

    LTBounds(
      left=x*xscale, top=y*yscale,
      width, height
    )
  }

  def mkTargetRegion(pageId: Int@@PageID, x: Int, y: Int, w: Int, h: Int): TargetRegion = {
    // bbox areas (for non-empty bounding boxes) are a bit smaller than full 1x1 area
    val bbox = getRegionBounds(x, y, w, h)

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
      chnum = pad - 1
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
                  charIds.nextId,
                  mkTargetRegion(pageId, x=chnum, y=linenum, w=1, h=1),
                  ch.toString
                )

                reflowBuilder.insertDownLast(Atom(charAtom))
                reflowBuilder.pop()
            }
            case x => println(s"error: ${x}")
        }
      }
    }

    reflowBuilder.newline()
    reflowBuilder.completed.foreach { reflow =>
      val tt = reflow.toText
      val lineBounds = reflow.bounds()
      val lineZone = docStore.createZone(docId)
      val regionId = docStore.addTargetRegion(pageId, lineBounds)
      val tr = docStore.getTargetRegion(regionId)
      docStore.setZoneTargetRegions(lineZone, Seq(tr))
      docStore.setTextReflowForZone(lineZone, reflow)
      docStore.addZoneLabel(lineZone, LB.VisualLine)
    }

    docStore.setPageGeometry(pageId, reflowBuilder.totalBounds())
  }
}
