package edu.umass.cs.iesl.watr
package extract

import org.scalatest._


class TextExtractionTest extends FlatSpec {
  // import watrmarks._
  // import StandardLabels._

  behavior of "itextpdf - generated text"

  it should "properly extract the following characters" in {
    // Example(TestRegion(papers.`6376.pdf`, page(0), LTBounds(166.0d, 549.0, 350.0, 15.0)),
    //   expectedChars = """Y. Adachi a∗, H. Morita a, T. Kanomata b, A. Sato b, H. Yoshida c,""".replaceAll(" ", ""),
    //   desiredChars  = """Y. Adachi a,∗, H. Morita a, T. Kanomata b, A. Sato b, H. Yoshida c,""".replaceAll(" ", ""),
    // Example(TestRegion(papers.`6376.pdf`, page(0), LTBounds(84.33d, 700.0, 403.2, 12.2)),
    //   expectedChars = """to be 431K and  2.6 10−2 GPa−1 for Rh2MnSn, and 471 K and  1.7 10−2 for GPa−1 Rh2MnGe, respectively""",
    //   desiredChars  = """to be 431K and +2.6×10−2 GPa−1 for Rh2MnSn, and 471 K and +1.7×10−2 for GPa−1 Rh2MnGe, respectively.""",
    // Example(TestRegion(papers.`bongard2005.pdf`, page(5), LTBounds(30.33d, 540.0, 223.2, 12.0)),
    //   expectedChars = """phosphate(1·PF6;2mmol)dissolvedin""",
    //   desiredChars = """phosphate (1·PF%-%6%; 2 mmol) dissolved in""",

    // Problems here: bad/missing char, math sym extraction, incorrect line joining
    //   want output like this:  bohedral) cell (a_{hex} = a_{0}{\sqrt}3, c_{hex} = 3c_{0}
    //    <!--
    //     however, disclosed an even larger (rhom– -->
    //     <svg:rect class="linebox" x="57.60" y="116.25" width="189.97"  height="9.99" />
    //     <!--
    //     bohedral) cell (ahex= aoti, chex= -->
    //     <svg:rect class="linebox" x="57.60" y="128.49" width="167.92"  height="9.99" />
    //     <!--
    //     34 -->
    //     <svg:rect class="linebox" x="233.04" y="130.55" width="7.66"  height="7.33" />
    //     <!--
    //     which also applied for the ThB*C phase, -->
    //     <svg:rect class="linebox" x="57.84" y="140.01" width="191.18"  height="9.99" />

    val _ = DocumentExtractor.extractChars(papers.`6376.pdf`)

    // assertResult(example.expectedChars.replaceAll(" ",""))(found)
    // val exc = example.expectedChars.replaceAll(" ", "")
    // val des = example.desiredChars.replaceAll(" ", "")
    // if (!des.isEmpty && exc != des) {
    //   println(s"""|warning: extracted characters need improvement... \n    ${found}
    //               | want: ${des}
    //               | have: ${exc}
    //               |""")
    // }


  }

}
