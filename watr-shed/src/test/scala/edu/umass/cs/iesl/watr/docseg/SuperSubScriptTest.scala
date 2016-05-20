package edu.umass.cs.iesl
package watr
package docseg

import java.io.InputStream
import org.scalatest._

import watrmarks._
import ext._
import scalaz.@@

case class Example(
  pdf: InputStream, page: Int @@ PageID, bbox: LTBounds,
  expectedChars: String, // This is the character string as we are best able to recover it
  desiredChars: String, // This is the character string we hope to extract with more work
  expectedTokenization: String, // This is the translation we see now
  desiredTokenization: String, // This is the desired translation
  skip: Boolean = false
)

class SuperSubScriptTest extends FlatSpec with Matchers {
  behavior of "docstrum segmenter"


  it should "handle super/subscripts" in {
    val examples = List(
      Example(papers.`6376.pdf`, page(0), LTBounds(166.0d, 586.0, 350.0, 12.0),
        expectedChars = """a Faculty of Engineering, Yamagata University, Yonezawa 992-8510, Japan""".replaceAll(" ", ""),
        desiredChars = "",
        expectedTokenization = """^a^ Faculty of Engineering, Yamagata University, Yonezawa 992-8510, Japan""",
        desiredTokenization = ""),

      Example(papers.`6376.pdf`, page(0), LTBounds(166.0d, 549.0, 350.0, 15.0),
        expectedChars = """Y. Adachi a∗, H. Morita a, T. Kanomata b, A. Sato b, H. Yoshida c,""".replaceAll(" ", ""),
        desiredChars  = """Y. Adachi a,∗, H. Morita a, T. Kanomata b, A. Sato b, H. Yoshida c,""".replaceAll(" ", ""),
        expectedTokenization = """Y. Adachi ^a^ ^∗^, H. Morita ^a^, T. Kanomata ^b^, A. Sato ^b^, H. Yoshida ^c^,""",
        desiredTokenization = """Y. Adachi ^a,∗^, H. Morita ^a^, T. Kanomata ^b^, A. Sato ^b^, H. Yoshida ^c^,"""),

      Example(papers.`6376.pdf`, page(0), LTBounds(84.33d, 700.0, 403.2, 12.2),
        expectedChars = """to be 431K and  2.6 10−2 GPa−1 for Rh2MnSn, and 471 K and  1.7 10−2 for GPa−1 Rh2MnGe, respectively""",
        desiredChars  = """to be 431K and +2.6×10−2 GPa−1 for Rh2MnSn, and 471 K and +1.7×10−2 for GPa−1 Rh2MnGe, respectively.""",
        expectedTokenization = """to be 431 K and 2.6 10^−^^2^ GPa^−^^1^ for Rh_2_MnSn, and 471 K and 1.7 10^−^^2^ for GPa^−^^1^ Rh_2_MnGe, respectively""",
        desiredTokenization  = """to be 431 K and +2.6×10^−2^ GPa^−1^ for Rh_2_MnSn, and 471 K and +1.7×10^−2^ for GPa^−1^ Rh_2_MnGe, respectively"""),

      Example(papers.`bongard2005.pdf`, page(0), LTBounds(30.33d, 145.0, 453.2, 15.2),
        expectedChars = """byDirkBongarda),MartinMöllera)b),S.NagarajaRao,DavidCorrb),andLorenzWalder*a)""",
        desiredChars = "",
        expectedTokenization = """by Dirk Bongard^a^), Martin Möller^a^)^b^), S. Nagaraja Rao, David Corr^b^), and Lorenz Walder*^a^)""",
        desiredTokenization  = """by Dirk Bongard^a^), Martin Möller^a^)^b^), S. Nagaraja Rao, David Corr^b^), and Lorenz Walder^*a^)"""),


      Example(papers.`bongard2005.pdf`, page(1), LTBounds(30.33d, 240.0, 453.2, 15.2),
        expectedChars = """(EF(c)=ðEðphenylvioÞlogenÞ).Electrontransferisslowinsituationc)ðduetothÞeactivationbarrier.""",
        desiredChars = "",
        expectedTokenization = "",
        desiredTokenization = "",
        skip=true
      ),

      Example(papers.`bongard2005.pdf`, page(1), LTBounds(30.33d, 232.0, 453.2, 12.0),
        expectedChars = """layer(EF(a)=Ealkylviologen),b)flatbandsituation(EF(b)=EFB(b)=Ebenzylviologen),andc)depletionlayer""",
        desiredChars = """""",
        expectedTokenization = """  """,
        desiredTokenization = """""",
        skip=true
      ),

      Example(papers.`bongard2005.pdf`, page(5), LTBounds(30.33d, 540.0, 223.2, 12.0),
        expectedChars = """phosphate(1·PF6;2mmol)dissolvedin""",
        desiredChars = """phosphate (1·PF%-%6%; 2 mmol) dissolved in""",
        expectedTokenization = """phosphate (1·PF%-%6%; 2 mmol) dissolved in""",
        desiredTokenization = """""",
        skip=true
      ),

      Example(papers.`bongard2005.pdf`, page(5), LTBounds(30.33d, 588.0, 500.0, 15.0),
        expectedChars = """2458.1H-NMR(250MHz,CD3CN):7.15(d,3J=8.9,arom.H);7.61(d,3J=8.9,arom.H);7.89(d,3J=6.1,""",
        desiredChars = """2458.1H-NMR(250MHz,CD3CN):7.15(d,3J=8.9,arom.H);7.61(d,3J=8.9,arom.H);7.89(d,3J=6.1,""",
        expectedTokenization = """phosphate (1·PF%-%6%; 2 mmol) dissolved in""",
        desiredTokenization = """phosphate (1·PF%-%6%; 2 mmol) dissolved in""",
        skip=true
      )
    )
    examples.foreach { example =>
      if (! example.skip) {

        val charsAndGeometry = CermineExtractor.extractChars(example.pdf)
        val zoneIndex = ZoneIndexer.loadSpatialIndices(charsAndGeometry)

        val chars = zoneIndex.queryChars(example.page, example.bbox)

        val found = chars.sortBy(_.bbox.left).map({ cbox => cbox.char }).toList.mkString
        // println(s"trying: $found")

        assertResult(example.expectedChars.replaceAll(" ",""))(found)
        val exc = example.expectedChars.replaceAll(" ", "")
        val des = example.desiredChars.replaceAll(" ", "")
        if (!des.isEmpty && exc != des) {
          println(s"""|warning: extracted characters need improvement... \n    ${found}
                      | want: ${des}
                      | have: ${exc}
                      |""")
        }


        val lineChars = chars.sortBy(_.bbox.left)
        val ccs = Component(lineChars.map(Component(_)), 0d, LB.Line)

        // val docstrum = new DocstrumSegmenter(zoneIndex)
        // val orientation = docstrum.computeOrientation(Seq(ccs))
        val tokenized = ccs.tokenizeLine()


        assertResult(example.expectedTokenization)(tokenized.toText)
      }

    }

  }

  // it should "segment page 1" in {
  //   // extract chars (via cermine)
  //   val charsAndGeometry = CermineExtractor.extractChars(papers.`6376.pdf`)
  //   println(s"extracted ${charsAndGeometry.length} pages")

  //   val zoneIndex = ZoneIndexer.loadSpatialIndices(charsAndGeometry)

  //   // zoneIndex.getPages.foreach { pageId =>
  //   //   docstrum.segmentPage(pageId)
  //   // }
  // }
}

