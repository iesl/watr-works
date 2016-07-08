package edu.umass.cs.iesl.watr
package utils

import org.scalatest._


class StringCaseUtilsTest extends FlatSpec with Matchers {
  behavior of "string casing"

  import StringCaseUtils._

  it should "convert camel->snake->camel" in {

    assertResult("this-is-camel-case"){
      "ThisIsCamelCase".toSnakeCase
    }
    assertResult("ThisIsCamelCase"){
      "this-is-camel-case".toCamelCase()
    }

  }
}
