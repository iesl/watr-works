package edu.umass.cs.iesl.watr
package watrmarks

import java.io.Reader
import javax.xml.stream.XMLInputFactory
import javax.xml.stream.events._;
// import scala.collection.JavaConversions._

package object dom {

  import javax.xml.namespace.QName
  import scalaz.{Show, TreeLoc, Tree}

  implicit val showElementTree = Show.shows[WatrElement](_.toString())

  // def document(): WatrElement = new Document()
  // def svg(): WatrElement      = new Svg()
  // def desc(): WatrElement     = new Desc()
  // def grp(): WatrElement      = new Grp()
  // def defs(): WatrElement     = new Defs()
  // def text(): WatrElement     = new Text()
  // def path(): WatrElement     = new Path()

  def maybeAttrValue(e: StartElement, s:String): Option[String] = {
    // println(s"maybe attrValue: $e: $s")
    val maybeAttr = e.getAttributeByName(QName.valueOf(s))
    if (maybeAttr!=null) Some(maybeAttr.getValue())
    else None
  }

  def attrValue(e: StartElement, s:String): String = {
    // println(s"attrValue: $e: $s")
    e.getAttributeByName(QName.valueOf(s)).getValue()
  }

  def getTransforms(e: StartElement): List[Transform] = {
    transformParser.parse(maybeAttrValue(e, "transform").getOrElse(""))
      .left.map(err => sys.error(s"parsing transform='${attrValue(e, "transform")}': ${err}"))
      .right.get
  }

  def digits(s: String): Double = {
    val ds = """(\d+)(px)?""".r
    val ds(d, _) = s
    d.toDouble
  }

  def getXs(e: StartElement): Option[List[Double]] = { maybeAttrValue(e, "x").map(_.split(" ").map(_.toDouble).toList) }
  def getEndX(e: StartElement): Option[Double]     = { maybeAttrValue(e, "endX").map(_.toDouble) }
  def getY(e: StartElement): Double                = { attrValue(e, "y").toDouble }
  def getFontSize(e: StartElement): String         = { maybeAttrValue(e, "font-size").getOrElse("0") }
  def getFontFamily(e: StartElement): String       = { maybeAttrValue(e, "font-family").getOrElse("") }
  def getBioBrick(e: StartElement): Option[String] = { maybeAttrValue(e, "bio") }
  def getWidth(e: StartElement): Double            = { digits(attrValue(e, "width")) }
  def getHeight(e: StartElement): Double           = { digits(attrValue(e, "height")) }
  def getViewBox(e: StartElement): ViewBox = {
    val vs = attrValue(e, "viewBox")
      .split(" ")
      .map(digits(_))
    ViewBox(vs(0), vs(1), vs(2), vs(3))
  }


  def readWatrDom(ins: Reader, bioDict: BioLabelDictionary): WatrDom = {
    val factory = XMLInputFactory.newInstance();
    val reader = factory.createXMLEventReader(ins);

    var accum: TreeLoc[WatrElement] = null

    import scala.collection.mutable

    var currentPageList = mutable.ListBuffer[TSpan]()
    var currentPageNumber = 1

    val pageSpans = mutable.ListMap[Int, mutable.ListBuffer[TSpan]](
      currentPageNumber -> currentPageList
    )



    while (reader.hasNext()) {
      val event = reader.nextEvent();

      event match {
        case elem: Namespace =>
        // println(s"Namespace: ${elem}")
        case attribute: Attribute =>
          println(s"Attribute: ${attribute}")
          val name = attribute.getName();
          val value = attribute.getValue();
          // println("Attribute name/value: " + name + "/" + value);
        case elem: StartDocument =>
          val n: WatrElement = Document(bioDict)

          accum = Tree.Leaf(n).loc
          // println(s"StartDocument: ${elem}")

        case elem: EndDocument =>
          // println(s"EndDocument: ${elem}")

        case elem: StartElement =>
          // if (accum.path.length == 2) {
          //   println(s"StartElement: ${elem.getName()} stack size = ${accum.path.length}")
          // }
          // // println(s"Start Element: ${elem.getName}, prefix: ${elem.getName().getPrefix()}, local: ${elem.getName().getLocalPart()}")

          elem.getName.getLocalPart.toLowerCase match {
            case "svg"   =>
              val n = Svg(
                width=getWidth(elem),
                height=getHeight(elem),
                viewBox=getViewBox(elem),
                getTransforms(elem)
              )

              accum = accum.insertDownLast(Tree.Leaf(n))

            case "g"     =>
              val labelAttr = elem.getAttributeByName(new QName("labels"))
              // debug(labelAttr)

              val labels:List[BioPin] = if (labelAttr != null) {
                // debug(labelAttr)
                val labels = labelAttr.getValue
                // TODO un-hardcode this page label addition
                List(PageLabel.fencePost)
              } else {
                List()
              }

              // firstPage.childNodes[0].setAttributeNS(null, 'labels', 'xy:page');
              val n = Grp(
                getTransforms(elem),
                labels
              )
              accum = accum.insertDownLast(Tree.Leaf(n))

            case "defs"  =>
              val n = Defs()
              accum = accum.insertDownLast(Tree.Leaf(n))

            case "text"  =>
              val n =  Text(getTransforms(elem))
              accum = accum.insertDownLast(Tree.Leaf(n))

            case "path"  =>
              val n =  Path(getTransforms(elem))
              accum = accum.insertDownLast(Tree.Leaf(n))

            case "tspan" =>
              import scalaz._, Scalaz._

              // val _xs = getXs(elem)
              // val _endx = getEndX(elem)
              val _y = getY(elem)

              val offs = ^(getXs(elem), getEndX(elem))(
                (_xs, _endx) => TextXYOffsets(
                  xs=_xs,
                  endX=_endx,
                  ys=List(_y)
              ))

              val n = TSpanInit(
                "",
                getTransforms(elem),
                offs,
                getFontSize(elem),
                getFontFamily(elem),
                getBioBrick(elem)
              )
              accum = accum.insertDownLast(Tree.Leaf(n))

            case "style"
               | "clippath" =>
              accum = accum.insertDownLast(Tree.Leaf(NullElement))
            case _ =>
              sys.error(s"no case match for StartElement: ${elem}")

          }


        case elem: EndElement =>
          elem.getName.getLocalPart.toLowerCase match {
            case "tspan"   =>
              val init = accum.getLabel.asInstanceOf[TSpanInit]
              val rootDocument = accum.root.getLabel.asInstanceOf[Document]


              def bounds: Option[List[TextBounds]] =
                init.textXYOffsets.map {
                  xyoffs => xyoffs.xs.map{x => TextBounds(
                    left   = x,
                    bottom = xyoffs.ys.head,
                    width  = 10, // TODO FIXME width/height
                    height = 10
                  )}
                }

              lazy val fontInfo =  FontInfo(init.fontFamily, init.fontSize)

              def fonts: List[FontInfo] = List.fill(init.text.length)(fontInfo)

              def emptyBioBrick: BrickColumns = BrickColumns(
                (init.text zip bounds.getOrElse(List()))
                  .toList
                  .map({ case (char, bnd) =>
                    BrickColumn(Set(), char, Some(fontInfo), Some(bnd))
                  }))

              val bioBrick: BrickColumns = init.bioBrickStr.map{str =>
                biolu.parseBioBrick(
                  str,
                  rootDocument.labelDictionary,
                  Some(init.text),
                  Some(bounds.getOrElse(List())),
                  Some(fonts)
                )
              } getOrElse emptyBioBrick


              val tspan = TSpan(
                init.text,
                init.transforms,
                init.textXYOffsets,
                init.fontSize,
                init.fontFamily,
                bioBrick,
                rootDocument
              )

              currentPageList.append(tspan)

              accum = accum.modifyLabel { _ => tspan }


            case _   =>
          }
          // println(s"EndElement: ${elem}, accum.parents=${accum.parents}")
          accum = accum.parent.get

        case elem: EntityReference =>
        // println(s"EntityReference: ${elem}")
        case elem: Characters =>
          // println(s"Characters: '${elem.getData().trim()}'")

              accum.getLabel match {
                case t: TSpanInit =>
                  accum = accum.modifyLabel { _ =>
                    t.copy(text = t.text+elem.getData())
                  }
                case _ =>
                  // sys.error(s"xml text found outside of tspan element: '${elem.getData()}'")
              }
          }

      // rt(x: 0, y:20, w:20, h: 30):p {layout: {page: p}, unit: px}
      // pt(x: 0, y:20)
      // ln(pt(0, 20), point(30, 30))
      // page: rt(0,20,20,30)
      /*

       labels=" page:rt(0,20,20,30) "

       */

      // println(s"""accu: ${if(accum!=null) accum.getLabel else "<null>"} """)
    }

    WatrDom(accum.toTree)
  }

}
