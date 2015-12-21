package edu.umass.cs.iesl.watr
package watrmarks
package dom

import java.io.StringReader
import org.scalatest._

class DomCursorSpec extends FlatSpec {
  import StandardLabels._

  behavior of "dom cursor"

  val svgStr1 = """| <svg version="1.1" width="612px" height="3168px" viewBox="0 0 612 3168">
                   |   <g transform="matrix(1 0 0 -1 0 792)">
                   |     <text transform="matrix(0 1 -1 0 32 256) scale(1, -1)">
                   |       <tspan x="0 8.88 15.54" endX="100.2" y="0" font-size="20px">abc</tspan>
                   |     </text>
                   |     <text transform="translate(136.8 669.12) scale(1, -1)">
                   |       <tspan x="137.16 146.75 154.43" endX="191.22" y="19.91" font-size="17.2154px">123</tspan>
                   |     </text>
                   |   </g>
                   |   <g transform="matrix(1 0 0 -1 0 792)">
                   |     <text transform="matrix(0 1 -1 0 32 256) scale(1, -1)">
                   |       <tspan x="0 8.88 15.54" endX="100.2" y="0" font-size="20px">def</tspan>
                   |     </text>
                   |     <text transform="translate(136.8 669.12) scale(1, -1)">
                   |       <tspan x="137.16 146.75 154.43" endX="191.22" y="19.91" font-size="17.2154px">456</tspan>
                   |     </text>
                   |   </g>
                   | </svg>
                   |""".stripMargin



  // it should "navigate next/prev elements" in {}
  it should "navigate next/prev tspans" in {

    val doc = readWatrDom(new StringReader(svgStr1), bioDict)

    val otspan = doc.toDomCursor

    val t1 = otspan.nextTSpan.get
    val t2 = t1.nextTSpan.get
    val t3 = t2.nextTSpan.get
    val t4 = t3.nextTSpan.get

    assert(t1.getLabel.asInstanceOf[TSpan].text === "abc")
    assert(t2.getLabel.asInstanceOf[TSpan].text === "123")
    assert(t3.getLabel.asInstanceOf[TSpan].text === "def")
    assert(t4.getLabel.asInstanceOf[TSpan].text === "456")

    import scalaz.std.stream._

    val uf = unfold[Option[DomCursor], WatrElement](
      Some(otspan)
    )(_ match {
      case Some(cur) => Some((cur.getLabel, cur.nextElem))
      case None => None
    })

    assert(uf.map(_.getClass.getSimpleName).mkString(",") === "Document,Svg,Grp,Text,TSpan,Text,TSpan,Grp,Text,TSpan,Text,TSpan")


    // val domstr = Stream.iterate(otspan){ domcursor =>
    //   domcursor.nextElem match {
    //     case Some(e) => e
    //     case None => domcursor
    //   }
    // }

    // domstr.take(20).foreach { dcur =>
    //   val l = dcur.getLabel
    //   println(l)
    // }


  }

}
