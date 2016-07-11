package edu.umass.cs.iesl.watr
package utils

import org.scalatest._


class StringCaseUtilsTest extends FlatSpec with Matchers {
  behavior of "string casing"

  import StringCaseUtils._

  it should "convert camel/snake/underscore case" in {

    assertResult("this-is-camel-case"){
      "ThisIsCamelCase".toSnakeCase
    }
    assertResult("ThisIsCamelCase"){
      "this-is-camel-case".toCamelCase()
    }

    assertResult("this_is_camel_case"){
      "ThisIsCamelCase".toUnderscoreCase()
    }

  }
}
