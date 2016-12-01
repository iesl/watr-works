package edu.umass.cs.iesl.watr
package textreflow

// import watrmarks.{StandardLabels => LB}
// import TextReflow._

trait TextReflowExamples extends StringReflowTestUtil {
  // val ffi = 0xFB03.toChar
  // val sufficient = flows(toAtoms(s"su${ffi}cient"))

  // def `Eu1 - x` = flow(
  //   flows(toAtoms("Eu")),
  //   labeled(LB.Sub, flows(toAtoms("1 - x")))
  // )

  // Eu_{1¿23;x}Bi_{x}VO_{4} bar
}

class TextReflowRenderingTest extends TextReflowExamples {


  behavior of "text reflow rendering"

  it should "render greppable (char-count) text" in {
    val textReflow = stringToTextReflow(
      """Eu_{1 - x}Bi_{x}VO_{4}"""
    )
    // val zoneIndex = createZoneIndexer(
    //   // 012 3 4567890
    //   """Eu1 - xBixVO4"""
    // )
    // Sufficient len = 10
    // `Eu1 - x` len = 7
  }

  it should "render sup/sub/char-entity formatted text" in {
    // `Eu1 - x`  ==> "Eu_{1 - x}"
    //  len = 11
  }

  it should "produce line-based IDs" in {}
}
