package edu.umass.cs.iesl.watr
package textreflow

import TextReflow._

class TextReflowParsingTest extends StringReflowTestUtil {
  behavior of "text reflow construction from string"

  it should "parse a string into correct nested structure" in {
    val textReflow = stringToTextReflow("""Eu_{1 - x}Bi_{x}VO_{4}""")

    // println(prettyPrintTree(textReflow))
  }

  it should "parse multiline strings" in {
    val textReflow = stringToTextReflow(
      """|of LiFePO4 scan-
         |ning electron
         |""".stripMargin
    )

    // println(prettyPrintTree(textReflow))
  }
}
