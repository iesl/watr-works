package org.watrworks
package rsearch

import TypeTags._

class LabeledShapeIndexTest extends WatrSpec {
  behavior of "LabeledShapeIndex"

  it should "get/set attributes" in {
    val sa = new ShapeAttributes {}
    val id = ShapeID(23)

    val Foo: AttrWitness[String] = AttrWitness.Mk[String]()
    val Bars = AttrWitness.Mk[List[String]]()

    sa.setAttr(id, Foo, "I")
    sa.getAttr(id, Foo)

    sa.setAttr(id, Foo, "I@F")
    sa.setAttr(id, Bars, "S[I]" :: Nil)

  }

}

import org.scalacheck._
import org.scalacheck.Prop._

object LabeledShapeIndexProps extends Properties("LabeledShapeIndex") {

  property("attrib set/get") = forAll { (example: String) =>
    val MyString: AttrWitness[String] = AttrWitness.Mk[String]()
    val sa = new ShapeAttributes {}
    val id = ShapeID(23)
    sa.setAttr(id, MyString, example)
    val att1 = sa.getAttr(id, MyString)

    example == att1.get
  }

}
