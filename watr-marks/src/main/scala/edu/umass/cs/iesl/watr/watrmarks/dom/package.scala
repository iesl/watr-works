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

  def getXs(e: StartElement): Option[List[Double]] = { maybeAttrValue(e, "x").map(_.split(" ").map(_.toDouble).toList) }
  def getEndX(e: StartElement): Option[Double]     = { maybeAttrValue(e, "endX").map(_.toDouble) }
  def getY(e: StartElement): Double                = { attrValue(e, "y").toDouble }
  def getFontSize(e: StartElement): String         = { maybeAttrValue(e, "font-size").getOrElse("0") }
  def getFontFamily(e: StartElement): String       = { maybeAttrValue(e, "font-family").getOrElse("") }
  def getBioBrick(e: StartElement): Option[String] = { maybeAttrValue(e, "bio") }

  def readWatrDom(ins: Reader, bioDict: BioLabelDictionary): WatrDom = {
    val factory = XMLInputFactory.newInstance();
    val reader = factory.createXMLEventReader(ins);

    var accum: TreeLoc[WatrElement] = null

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
          // println(s"StartElement: ${elem}")
          // println(s"Start Element: ${elem.getName}, prefix: ${elem.getName().getPrefix()}, local: ${elem.getName().getLocalPart()}")

          elem.getName.getLocalPart match {
            case "svg"   =>
              val n = Svg(getTransforms(elem))

              accum = accum.insertDownLast(Tree.Leaf(n))

            case "g"     =>
              val n = Grp(getTransforms(elem))
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

              val xs = getXs(elem)
              val endx = getEndX(elem)
              val y = getY(elem)

              val offs = ^(xs, endx)(
                (xs0, y0) => TextXYOffsets(y, xs0, y0)
              )

              val n = TSpan(
                "",
                getTransforms(elem),
                offs,
                getFontSize(elem),
                getFontFamily(elem),
                getBioBrick(elem),
                accum.root.getLabel.asInstanceOf[Document]
              )
              accum = accum.insertDownLast(Tree.Leaf(n))
            case "style"
               | "clippath" =>
              accum = accum.insertDownLast(Tree.Leaf(NullElement))
            case _ =>
              sys.error(s"no case match for StartElement: ${elem}")

          }


        case elem: EndElement =>
          // println(s"EndElement: ${elem}, accum.parents=${accum.parents}")
          accum = accum.parent.get
        case elem: EntityReference =>
        // println(s"EntityReference: ${elem}")
        case elem: Characters =>
          // println(s"Characters: '${elem.getData().trim()}'")

          accum.getLabel match {
            case t: TSpan =>
              accum = accum.modifyLabel { _ =>
                t.copy(text = t.text+elem.getData())
              }
            case _ =>
              // sys.error(s"xml text found outside of tspan element: '${elem.getData()}'")
          }
      }

      // println(s"""accu: ${if(accum!=null) accum.getLabel else "<null>"} """)
    }

    WatrDom(accum.toTree)
  }

}
