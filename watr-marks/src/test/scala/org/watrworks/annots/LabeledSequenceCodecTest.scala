package org.watrworks
package annots

import org.scalacheck._

import LabeledSequenceCodecs._
// import LabeledSequencePrinting._
// import LabeledSequence.Things
// import LabelTarget.Thing



object LabeledSequenceCodecChecks extends Properties("LabeledSequenceCodecChecks") with LabeledSequenceThings {
  // import ArbitraryStuff._

  // property("json <--> LabeledSequence") = Prop.forAll{ (labeledSequence: LabeledSequence[Thing[Char]]) =>
  //   // TODO
  //   true
  // }

}

class LabeledSequenceCodecTest extends LabeledSequenceTestBasics {


  it should "Behavior of labeled sequence serialization to/from Json" in {

    val thingCount = 10
    val things = unlabeledThings(thingCount)

    things.addBioLabel(Journal)
    things.addBioLabel(Author)
    things.addBioLabel(FirstName, 0, 3)
    things.addBioLabel(MiddleName, 4, 1)
    things.addBioLabel(LastName, 6, 3)
    val encodedJson = encodeBioLabels(things)

    val things2 = unlabeledThings(thingCount)
    decodeAndApplyBioLabels(encodedJson, things2)

    assert(things === things2)

  }
}
