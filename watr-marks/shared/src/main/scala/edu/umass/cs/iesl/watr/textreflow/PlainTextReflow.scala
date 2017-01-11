package edu.umass.cs.iesl.watr
package textreflow //;import acyclic.file

// TODO plaintext reflow started as testing util, and still has a mix of testing and production code
trait PlainTextReflow {
  import scalaz.std.string._
  import scalaz.Tree
  import utils.ScalazTreeImplicits._
  import utils.IdGenerator
  import matryoshka._
  import java.net.URI
  import geometry._

  import EnrichGeometricFigures._
  import ComponentTypeEnrichments._
  import TextReflowF._

  def dummyUri = URI.create("/")

  val regionIDs = IdGenerator[RegionID]()

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


  def mkTargetRegion(docId: String@@DocumentID, pageId: Int@@PageID, x: Int, y: Int, w: Int, h: Int) = {
    // bbox areas (for non-empty bounding boxes) are a bit smaller than full 1x1 area
    val width = if (w>0) {
      w*xscale - 0.1
    } else 0
    val height = if (h>0) {
      h*yscale - 0.1
    } else 0

    TargetRegion(
      RegionID(0),
      docId, pageId,
      LTBounds(
        left=x*xscale, top=y*yscale,
        width, height
      )
    )
  }


  def stringToTextReflow(multiLines: String): TextReflow = {
    val isMultiline = multiLines.contains("\n")

    var tloc = if (isMultiline) {
      val t: Tree[TextReflowF[Int]] =
        Tree.Node(Flow(List()),
          Stream(
            Tree.Node(Labeled(Set(LB.VisualLine), 0),
              Stream(Tree.Leaf(Flow(List()))))))
      t.loc.lastChild.get.lastChild.get
    } else {
      val t: Tree[TextReflowF[Int]] =
        Tree.Leaf(Flow(List()))
      t.loc
    }

    import scala.collection.mutable

    var docId = DocumentID("doc-0")
    var pageId = PageID(0)
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
        docId, pageId,
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
            mkTargetRegion(docId, pageId,
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
            mkTargetRegion(docId, pageId, chnum, linenum, 1, 1),
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

    res.rootLabel
  }



  def stringToPageAtoms(str: String, pageId: Int, docId: String@@DocumentID): (Seq[PageAtom], PageGeometry) = {
    for {
      (line, linenum) <- lines(str).zipWithIndex
      (ch, chnum)     <- line.zipWithIndex
    } yield {
      CharAtom(
        TargetRegion(regionIDs.nextId,
          docId,
          page0,
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
              TargetRegion(regionIDs.nextId,
                docId,
                page0,
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
      PageID(pageId), LTBounds(
        left=0, top=0,
        width=maxX, height=maxY
      )
    )

    (atoms, pageGeom)
  }

  import com.sksamuel.scrimage._

  def textToImage(text: String): Unit = {

    import java.awt.Color
    import java.awt.Font
    import java.awt.FontMetrics
    import java.awt.Graphics2D
    import java.awt.RenderingHints
    import java.awt.image.BufferedImage

    /*
     Because font metrics is based on a graphics context, we need to create
     a small, temporary image so we can ascertain the width and height
     of the final image
     */
    val imgtmp: BufferedImage =  new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB)
    val g2dtmp: Graphics2D  = imgtmp.createGraphics()
    val font: Font  = new Font("Arial", Font.PLAIN, 48)
    g2dtmp.setFont(font)
    val fmetrics: FontMetrics  = g2dtmp.getFontMetrics()
    val width = fmetrics.stringWidth(text)
    val height = fmetrics.getHeight()
    g2dtmp.dispose()

    val img = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val g2d = img.createGraphics()
    g2d.setRenderingHint(RenderingHints.KEY_ALPHA_INTERPOLATION, RenderingHints.VALUE_ALPHA_INTERPOLATION_QUALITY)
    g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g2d.setRenderingHint(RenderingHints.KEY_COLOR_RENDERING, RenderingHints.VALUE_COLOR_RENDER_QUALITY)
    g2d.setRenderingHint(RenderingHints.KEY_DITHERING, RenderingHints.VALUE_DITHER_ENABLE)
    g2d.setRenderingHint(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON)
    g2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR)
    g2d.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
    g2d.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE)
    g2d.setFont(font)
    val fm = g2d.getFontMetrics()
    g2d.setColor(Color.BLACK)
    g2d.drawString(text, 0, fm.getAscent())
    g2d.dispose()
    Image.wrapAwt(img)
  }

}
