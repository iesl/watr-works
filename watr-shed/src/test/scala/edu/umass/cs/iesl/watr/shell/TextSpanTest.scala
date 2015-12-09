package edu.umass.cs.iesl.watr.shell

import org.scalatest._


class TextSpanSpec extends FlatSpec {

  behavior of "TextSpan"

  // TextSpan functionality includes
  //  - chars (the actual text)
  //  - x,y position for each
  //  - font info: name/type/weight/height/etc
  //  - labels
  //    - serialize to/from block format

  it should "unserialize from an svg:tspan" in {}

  behavior of "TextSpanCursor"


  it should "convert from/to TextSpan" in {}

  it should "navigate chars" in {
    // val tcursor = textSpan.cursor
    // tcursor.current.char === 'x'
    // tcursor.current.x === 30.33
    // tcursor.current.y ===...
    // tcursor.current.y ===...
  }
  it should "navigate labels" in {
    // val ld = LabelDictionary(
    //    "phrase", 'p', Constraints.Chars
    // )
    // val phrase = ld("phrase")
    // val token = ld("token")
    // val tcursor = textSpan.cursor
    // tcursor.current.labels === Set(phrase.B, token.I)
    // tcursor.next(phrase) ===
  }

}
