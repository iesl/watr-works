package org.watrworks
package utils

class StringCaseUtilsTest extends WatrSpec {
  behavior of "string casing"

  import StringUtils._

  it should "convert camel/snake/underscore case" in {

    assertResult("this-is-camel-case"){
      "ThisIsCamelCase".toSnakeCase()
    }
    assertResult("ThisIsCamelCase"){
      "this-is-camel-case".toCamelCase()
    }

    assertResult("this_is_camel_case"){
      "ThisIsCamelCase".toUnderscoreCase()
    }

  }
}
