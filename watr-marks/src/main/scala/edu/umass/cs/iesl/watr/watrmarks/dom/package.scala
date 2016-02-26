package edu.umass.cs.iesl.watr
package watrmarks

import java.io.Reader
import javax.xml.stream.XMLInputFactory
import javax.xml.stream.events._;
import scala.collection.JavaConversions._

package object dom {

  import javax.xml.namespace.QName
  import scalaz.{Show, TreeLoc, Tree}

  implicit val showElementTree = Show.shows[WatrElement](_.toString())

  def maybeAttrValue(e: StartElement, s:String): Option[String] = {
    val maybeAttr = e.getAttributeByName(QName.valueOf(s))
    if (maybeAttr!=null) Some(maybeAttr.getValue())
    else None
  }

  def attrValue(e: StartElement, s:String): String = {
    e.getAttributeByName(QName.valueOf(s)).getValue()
  }

  def getTransforms(e: StartElement): List[Transform] = {
    transformParser.parse(maybeAttrValue(e, "transform").getOrElse(""))
      .left.map(err => sys.error(s"parsing transform='${attrValue(e, "transform")}': ${err}"))
      .right.get
  }

  def digits(s: String): Double = {
    val ds = """((?:\d+)(?:\.\d+)?)(px)?""".r
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

  import scala.collection.mutable

  def readWatrDom(ins: Reader, bioDict: BioLabelDictionary): WatrDom = {
    val factory = XMLInputFactory.newInstance();
    val reader = factory.createXMLEventReader(ins);

    var accum: TreeLoc[WatrElement] = null

    def accAppend(e: WatrElement): Unit = {
      accum = accum.insertDownLast(Tree.leaf(e))
    }

    def accPop(): Unit = {
      accum = accum.parent.get
    }

    var annotations = mutable.ArrayBuffer[Annotation]()



    while (reader.hasNext()) {
      val event = reader.nextEvent();

      event match {
        case elem: Namespace =>
        case attribute: Attribute =>
          val name = attribute.getName();
          val value = attribute.getValue();
        case elem: StartDocument =>
          val n: WatrElement = Document(bioDict)


          accum = Tree.leaf(n).loc

        case elem: EndDocument =>

        case elem: StartElement =>
          // if (accum.path.length == 2) {
          //   println(s"StartElement: ${elem.getName()} stack size = ${accum.path.length}")
          // }
          // println(s"Start Element: ${elem.getName}, prefix: ${elem.getName().getPrefix()}, local: ${elem.getName().getLocalPart()}")

          elem.getName.getLocalPart.toLowerCase match {
            case "svg"   =>
              val n = Svg(
                width=getWidth(elem),
                height=getHeight(elem),
                viewBox=getViewBox(elem),
                getTransforms(elem)
              )

              accAppend(n)

            case "g"     =>
              if (accum.getLabel == AnnotationMarker) {
                annotations.add(Annotation(
                  elem.getAttributeByName(new QName("id")).getValue,
                  List()
                ))

                accAppend(NullElement)
              } else {
                val n = Grp(
                  getTransforms(elem)
                )
                accAppend(n)
              }

            case "defs"  =>
              val annotationAttrib = new QName("annotation-boxes")
              val attribs = elem.getAttributes.toList
              if (attribs.contains(annotationAttrib)) {
                accAppend(AnnotationMarker)
              } else {
                accAppend(Defs())
              }
            case "text"  =>
              val n =  Text(getTransforms(elem))
              accAppend(n)

            case "path"  =>
              val n =  Path(getTransforms(elem))
              accAppend(n)

            case "annotation-links"  =>
              accAppend(NullElement)
            case "citation-reference-link"  =>
              accAppend(NullElement)
            case "use"  =>
              accAppend(NullElement)
            case "rect"  =>
              if (accum.getLabel == AnnotationMarker) {
                val ann = annotations.remove(0)
                ann.copy(
                  bboxes = ann.bboxes :+ Rect(
                    attrValue(elem, "x").toDouble,
                    attrValue(elem, "y").toDouble,
                    attrValue(elem, "width").toDouble,
                    attrValue(elem, "height").toDouble
                  )
                )
              }

              accAppend(NullElement)
            case "image"  =>
              accAppend(NullElement)
            case "mask"  =>
              accAppend(NullElement)

            case "tspan" =>
              import scalaz._, Scalaz._

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
                getFontFamily(elem)
              )
              accAppend(n)

            case "style"
               | "clippath" =>
              accAppend(NullElement)
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


              val tspan = TSpan(
                init.text,
                init.textXYOffsets,
                init.fontSize,
                init.fontFamily,
                rootDocument
              )

              accum = accum.modifyLabel { _ => tspan }

            case _   =>
          }
          // println(s"EndElement: ${elem}, accum.parents=${accum.parents}")
          accPop()

        case elem: EntityReference =>
        case elem: Characters =>

              accum.getLabel match {
                case t: TSpanInit =>
                  accum = accum.modifyLabel { _ =>
                    t.copy(text = t.text+elem.getData())
                  }
                case _ =>
                  // sys.error(s"xml text found outside of tspan element: '${elem.getData()}'")
              }
          }


    }

    WatrDom(accum.toTree, annotations.toList)
  }

}
