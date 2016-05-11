// package edu.umass.cs.iesl.watr
// package watrmarks
// package dom

// import java.io.StringReader
// import org.scalatest._

// class DomCursorSpec extends FlatSpec {
//   import StandardLabels._

//   behavior of "dom cursor"

//   val svgStr1 = """| <svg version="1.1" width="612px" height="3168px" viewBox="0 0 612 3168">
//                    |   <g>
//                    |     <text>
//                    |       <tspan x="0 8.88 15.54" endX="100.2" y="0" font-size="20px">abc</tspan>
//                    |     </text>
//                    |     <text>
//                    |       <tspan x="137.16 146.75 154.43" endX="191.22" y="19.91" font-size="17.2154px">123</tspan>
//                    |     </text>
//                    |   </g>
//                    |   <g>
//                    |     <text>
//                    |       <tspan x="0 8.88 15.54" endX="100.2" y="0" font-size="20px">def</tspan>
//                    |     </text>
//                    |     <text>
//                    |       <tspan x="137.16 146.75 154.43" endX="191.22" y="19.91" font-size="17.2154px">456</tspan>
//                    |     </text>
//                    |   </g>
//                    | </svg>
//                    |""".stripMargin


//   import scalaz.std.stream._

//   it should "navigate all elements" in {
//     val doc = readWatrDom(new StringReader(svgStr1), bioDict)

//     val otspan = Some(doc.toDomCursor)

//     val elems = unfold[Option[DomCursor], WatrElement](
//       otspan
//     )(_ match {
//       case Some(cur) => Some((cur.getLabel, cur.nextElem))
//       case None => None
//     })

//     assert (
//       elems.map(_.getClass.getSimpleName).mkString(",") ===
//         "Document,Svg,Grp,Text,TSpan,Text,TSpan,Grp,Text,TSpan,Text,TSpan"
//     )

//     val cursors = unfold[Option[DomCursor], DomCursor](
//       otspan
//     )(_ match {
//       case Some(cur) => Some((cur, cur.nextElem))
//       case None => None
//     })

//     val lcursor = cursors.last
//     val elemsReversed = unfold[Option[DomCursor], WatrElement](
//       Some(lcursor)
//     )(_ match {
//       case Some(cur) => Some((cur.getLabel, cur.prevElem))
//       case None => None
//     })

//     assert (
//       elemsReversed.reverse.map(_.getClass.getSimpleName).mkString(",") ===
//         "Document,Svg,Grp,Text,TSpan,Text,TSpan,Grp,Text,TSpan,Text,TSpan"
//     )
//   }


//   it should "navigate next/prev tspans" in {

//     val doc = readWatrDom(new StringReader(svgStr1), bioDict)

//     val otspan = doc.toDomCursor

//     val t1 = otspan.nextTSpan.get
//     val t2 = t1.nextTSpan.get
//     val t3 = t2.nextTSpan.get
//     val t4 = t3.nextTSpan.get

//     assert(t1.getLabel.asInstanceOf[TSpan].text === "abc")
//     assert(t2.getLabel.asInstanceOf[TSpan].text === "123")
//     assert(t3.getLabel.asInstanceOf[TSpan].text === "def")
//     assert(t4.getLabel.asInstanceOf[TSpan].text === "456")

//   }
//   val svgStr = """|<svg version="1.1" width="612px" height="3168px" viewBox="0 0 612 3168">
//                   |    <g>
//                   |      <text>
//                   |        <tspan x="0 1 2" endX="100.2" y="0" font-size="20px"
//                   |bio="| |T t| {ns:tok, type: {token: t}, unit: char}"
//                   |       >abc</tspan>
//                   |      </text>
//                   |      <text>
//                   |        <tspan x="0 1 2" endX="100.2" y="0" font-size="20px"
//                   |bio="|t|~$t| {ns:tok, type: {token: t}, unit: char}"
//                   |       >def</tspan>
//                   |      </text>
//                   |    </g>
//                   |    <text>
//                   |      <tspan x="0 1 2" endX="100.2" y="0" font-size="20px"
//                   |bio="|t|$T | {ns:tok, type: {token: t}, unit: char}"
//                   |       >123</tspan>
//                   |    </text>
//                   |    <g>
//                   |      <text>
//                   |        <tspan x="0 1 2" endX="100.2" y="0" font-size="20px"
//                   |bio="| |TTT| {ns:tok, type: {token: t}, unit: char}"
//                   |       >ghi</tspan>
//                   |      </text>
//                   |      <text>
//                   |        <tspan x="0 1 2" endX="100.2" y="0" font-size="20px"
//                   |bio="| | t$| {ns:tok, type: {token: t}, unit: char}"
//                   |       >jkl</tspan>
//                   |      </text>
//                   |    </g>
//                   |  </svg>
//                   |""".stripMargin

//   it should "navigate nexts properly" in {
//     val doc = dom.readWatrDom(new StringReader(svgStr), bioDict)
//     val dcur = doc.toDomCursor

//     val _ = for {
//       t1 <- dcur.nextTSpan
//       // _ = println("t1" + t1.getLabelAsTSpan)
//       _ = assert(t1.getLabelAsTSpan.text == "abc")
//       t2 <- t1.nextTSpan
//       _ = assert(t2.getLabelAsTSpan.text == "def")
//       t3 <- t2.nextTSpan
//       _ = assert(t3.getLabelAsTSpan.text == "123")
//       t4 <- t3.nextTSpan
//       _ = assert(t4.getLabelAsTSpan.text == "ghi")
//       t5 <- t4.nextTSpan
//       _ = assert(t5.getLabelAsTSpan.text == "jkl")
//     }{
//       // println("done")
//     }

//   }
// }
