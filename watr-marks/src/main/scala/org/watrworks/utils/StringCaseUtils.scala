package org.watrworks
package utils



object StringUtils {

  def bracket(l:Char, r:Char, b: String): String = {
    val lb = l.toString
    val rb = r.toString
    lb + b + rb
  }

  def dquote(b: String): String = bracket('"', '"', b)
  def squareBracket(b: String): String = bracket('[', ']', b)
  def curlyBrace(b: String): String = bracket('{', '}', b)


  implicit class RicherString(val str: String) extends AnyVal {
    def toSnakeCase(): String = {
      str.replaceAll("([A-Z])", "-$1")
        .toLowerCase
        .replaceAll("^-", "")
    }

    def toUnderscoreCase(): String = {
      str.replaceAll("([A-Z])", "_$1")
        .toLowerCase
        .replaceAll("^_", "")
    }

    def toCamelCase(): String = {
      str.split("-")
        .map({s => s.headOption
          .map(_.toUpper)
          .map(_ + s.substring(1))
        }).flatten.mkString
    }
  }

}
