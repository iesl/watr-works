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

  import utils.EnrichNumerics._
  import geometry.syntax._
  import TextReflowF._

  def docStore: DocumentZoningApi

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

  def getRegionBoundsDbl(x: Double, y: Double, w: Double, h: Double): LTBounds = {
    val left   = x * xscale
    val top    = y * yscale
    val width  = w * xscale
    val height = h * yscale

    LTBounds.Doubles(left, top, width, height)
  }

  // // bbox areas (for non-empty bounding boxes) are a bit smaller than full 1x1 area
  def getRegionBounds(x: Int, y: Int, w: Int, h: Int): LTBounds = {
    getRegionBoundsDbl(x.toDouble, y.toDouble, w.toDouble, h.toDouble)
  }

  def mkTargetRegionDbl(pageId: Int@@PageID, x: Double, y: Double, w: Double, h: Double): TargetRegion = {
    val bbox  = getRegionBoundsDbl(x, y, w, h)
    val regionId = docStore.addTargetRegion(pageId, bbox)
    docStore.getTargetRegion(regionId)
  }

  def mkPageRegion(pageId: Int@@PageID, x: Int, y: Int, w: Int, h: Int): PageRegion = {
    val bbox = getRegionBounds(x, y, w, h)
    val recPageId = docStore.getPageIdentifier(pageId)
    PageRegion(recPageId, bbox)
  }

  def mkTargetRegion(pageId: Int@@PageID, x: Int, y: Int, w: Int, h: Int): TargetRegion = {
    val pageRegion = mkPageRegion(pageId, x, y, w, h)
    val regionId = docStore.addTargetRegion(pageId, pageRegion.bbox)
    docStore.getTargetRegion(regionId)
  }

  def addDocument(stableId: String@@DocumentID, pages:Seq[String]): Unit  = {
    docStore.addDocument(stableId)
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
    val docId = docStore.getDocument(stableId).get
    val pageId = docStore.addPage(docId, pageNum)

    var linenum:Int = -1
    var chnum = 0
    val reflowBuilder = new TextReflowBuilder

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

                val pageRegion = mkPageRegion(pageId, x=chnum, y=linenum, w=1, h=1)
                val adjustedBbox = pageRegion.bbox.scale(-2.percent).translate(0.1, 0.1)
                val adjRegion = pageRegion.copy(bbox = adjustedBbox)
                val charAtom = CharAtom(
                  charIds.nextId,
                  adjRegion,
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
    val labelId = docStore.ensureLabel(LB.VisualLine)

    reflowBuilder.completed.foreach { reflow =>
      val lineBounds = reflow.bounds()
      val regionId = docStore.addTargetRegion(pageId, lineBounds)

      val lineZoneId = docStore.createZone(regionId, labelId)

      docStore.setTextReflowForZone(lineZoneId, reflow)
    }

    docStore.setPageGeometry(pageId, reflowBuilder.totalBounds())
  }
}
