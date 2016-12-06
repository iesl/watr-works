package edu.umass.cs.iesl.watr
package textreflow



class TextReflowRenderingTest extends StringReflowTestUtil {


  behavior of "text reflow rendering"

  import spindex.ZoneIndexer

  it should "render greppable (char-count) text" in {
    val textReflow0 = stringToTextReflow("""Eu_{1 - x}Bi_{x}VO_{4}""")
    val zoneIndex = new ZoneIndexer(dummyUri)
    val textReflow = textReflowToComponentReflow(textReflow0, zoneIndex)

    // Sufficient len = 10
    // `Eu1 - x` len = 7
  }

  it should "render sup/sub/char-entity formatted text" in {
    // `Eu1 - x`  ==> "Eu_{1 - x}"
    //  len = 11
  }

  it should "produce line-based IDs" in {}
}
