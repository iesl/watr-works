package edu.umass.cs.iesl.watr
package spindex

import org.scalatest._

trait ConnectedComponentTestUtil extends FlatSpec with Matchers {
  import scalaz._, Scalaz._
  import utils.ScalazTreeImplicits._
  import utils.IdGenerator
  import TypeTags._
  import GeometricFigure._
  import EnrichGeometricFigures._
  import watrmarks.{StandardLabels => LB}
  import edu.umass.cs.iesl.watr.watrmarks.Label
  import matryoshka._
  import matryoshka.implicits._
  import java.net.URI
  def dummyUri = URI.create("/")

  import textreflow._
  import TextReflowF._

  val regionIDs = IdGenerator[RegionID]()
  // val pageIDs = IdGenerator[PageID]()

  val page0 = PageID(0)
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


  def textReflowToComponentReflow(textReflow: TextReflow, zoneIndex: ZoneIndexer): TextReflow = {
    val regions = textReflow.collect({
      case  t@ Embed(Atom(c, ops)) => c.asInstanceOf[PageAtom].targetRegion.bbox
    })
    val totalPageGeometry = regions.reduce(_ union _)
    val geom = PageGeometry(PageID(0), totalPageGeometry)

    zoneIndex.addPage(geom)
    textReflow.mapT({
      case Atom(c, ops) =>
        val atomicComponent = zoneIndex.addPageAtom(c.asInstanceOf[PageAtom])
        Atom(atomicComponent, ops)
      case t => t
    })
  }


  def stringToTextReflow(multiLines: String): TextReflow = {
    val t: Tree[TextReflowF[Int]] =
      Tree.Node(Flow(List()),
        Stream(
          Tree.Node(Labeled(Set(LB.VisualLine), 0),
            Stream(Tree.Leaf(Flow(List()))))))

    var tloc = t.loc.lastChild.get.lastChild.get
    var linenum = 0
    var chnum = 0

    def insertRight(tr: TextReflowF[Int]): Unit    = { tloc = tloc.insertRight(Tree.Leaf(tr)) }
    def insertLeft(tr: TextReflowF[Int]): Unit     = { tloc = tloc.insertLeft(Tree.Leaf(tr)) }
    def insertDownLast(tr: TextReflowF[Int]): Unit = { tloc = tloc.insertDownLast(Tree.Node(tr, Stream())) }
    def pop(): Unit = { tloc = tloc.parent.get }
    //
    def debug(): Unit = { println(tloc.toTree.map(_.toString).drawBox) }

    for (ch <- lines(multiLines).mkString) {
      ch match {
        case '\n' =>
          linenum += 1
          chnum = 0
          pop(); pop()
          insertDownLast(Labeled(Set(LB.VisualLine), 0))
          insertDownLast(Flow(List()))

        case '^' => insertDownLast(Labeled(Set(LB.Sup), 0))
        case '_' => insertDownLast(Labeled(Set(LB.Sub), 0))
        case '{' => insertDownLast(Flow(List()))
        case '}' => pop(); pop()
        case ' ' => insertDownLast(Insert(" ")); pop()
        case _ =>
          val charAtom = CharAtom(
            TargetRegion(regionIDs.nextId, page0,
              LTBounds(
                left=chnum*xscale, top=linenum*yscale,
                width=xscale, height=yscale
              )
            ),
            ch.toString
          )
          val ops = new textreflow.TextReflowAtomOps(Seq(ch))
          insertDownLast(Atom(charAtom, ops))
          pop()
      }

    }

    // Now construct the Fix[] version of the tree:
    val ftree = tloc.toTree
    val res = ftree.scanr ((reflowNode: TextReflowF[Int], childs: Stream[Tree[TextReflow]]) => {
      reflowNode match {
          case t@ Atom(c, ops)               => fixf(Atom(c, ops))
          case t@ Insert(value)              => fixf(Insert(value))
          case t@ Rewrite(from, to)          => fixf(Rewrite(childs.head.rootLabel, to))
          case t@ Bracket(pre, post, a)      => fixf(Bracket(pre, post, childs.head.rootLabel))
          case t@ Flow(atoms)                => fixf(Flow(childs.toList.map(_.rootLabel)))
          case t@ Labeled(ls, _)             => fixf(Labeled(ls, childs.head.rootLabel))
        }
      }
    )

    res.rootLabel
  }


  def stringToPageAtoms(str: String): (Seq[PageAtom], PageGeometry) = {
    for {
      (line, linenum) <- lines(str).zipWithIndex
      (ch, chnum)     <- line.zipWithIndex
    } yield {
      CharAtom(
        TargetRegion(regionIDs.nextId, page0,
          LTBounds(
            left=chnum*xscale, top=linenum*yscale,
            width=xscale, height=yscale
          )
        ),
        ch.toString
      )

    }

    val atoms = lines(str).zipWithIndex
      .map({ case (line, linenum) =>
        line.zipWithIndex
          .filterNot(_._1 == ' ')
          .map({ case (ch, chnum) =>
            CharAtom(
              TargetRegion(regionIDs.nextId, page0,
                LTBounds(
                  left=chnum*xscale, top=linenum*yscale,
                  width=xscale, height=yscale
                )
              ),
              ch.toString
            )
          })
      })
      .flatten.toSeq

    val maxX = atoms.map(_.targetRegion.bbox.right).max
    val maxY = atoms.map(_.targetRegion.bbox.bottom).max


    val pageGeom = PageGeometry(
      PageID(0), LTBounds(
        left=0, top=0,
        width=maxX, height=maxY
      )
    )


    (atoms, pageGeom)
  }

  def queryComponents(pageInfo: PageIndex, x: Int, y: Int, w: Int, h: Int): Seq[Component] = {
    val q = LTBounds(x*xscale, y*yscale, w*xscale, h*xscale)
    pageInfo.componentIndex.queryForContained(q)
  }


  def labelRow(zoneIndex: ZoneIndexer, row: Int, l: Label): Option[RegionComponent] = {
    val pageInfo = zoneIndex.getPageIndex(page0)
    val q = LTBounds(0, row*yscale, Int.MaxValue, yscale)
    val charAtoms = pageInfo.componentIndex.queryForContained(q)
    val reg = zoneIndex.labelRegion(charAtoms, l)
    // assert each region can select its contained page atoms
    reg.foreach { rc =>
      val patoms = rc.queryInside(LB.PageAtom)
      assertResult(charAtoms.toSet){
        patoms.toSet
      }
    }
    reg
  }

  import java.net.URI



  def createZoneIndexer(str: String): ZoneIndexer = {
    val (atoms, geom) = stringToPageAtoms(str)
    val dummyUri = URI.create("/")
    ZoneIndexer.loadSpatialIndices(dummyUri, Seq((atoms -> geom)))
  }

  def createZoneIndexerAndLines(str: String): (ZoneIndexer, Seq[String]) = {
    val (atoms, geom) = stringToPageAtoms(str)
    val dummyUri = URI.create("/")
    (ZoneIndexer.loadSpatialIndices(dummyUri, Seq((atoms -> geom))),
      lines(str))
  }

  def tokenizeStringToZoneIndex(str: String): (ZoneIndexer, Seq[RegionComponent]) = {
    val (zoneIndex, lines) = createZoneIndexerAndLines(str)
    val visualLines = for {
      i <- 0 until lines.length
      visualLine <- labelRow(zoneIndex, i, LB.VisualLine)
    } yield {
      // visualLine.addLabel(LB.Tokenized)
      val textSpanRegion = visualLine.cloneAs(LB.TextSpan)
      textSpanRegion.addLabel(LB.Tokenized)
      visualLine.setChildren(LB.TextSpan, Seq(textSpanRegion))

      textSpanRegion.groupAtomsIf({(atom1, atom2, pairIndex) =>
        val dist = atom2.bounds.right - atom1.bounds.right
        dist <= xscale
      }, {(region, regionIndex) =>
        region.addLabel(LB.TextSpan)
        region.addLabel(LB.Token)
        textSpanRegion.addChild(LB.TextSpan, region)
      })

      visualLine
    }
    (zoneIndex, visualLines)
  }

}
