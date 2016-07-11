package edu.umass.cs.iesl.watr
package utils

// import scalaz.Tag
// import scalaz.@@

// sealed trait StringCase

// sealed trait SnakeCase extends StringCase
// sealed trait CamelCase extends StringCase
// sealed trait SnakeUScoreCase extends StringCase

object StringCaseUtils {

  // implicit class TagOps[T <: StringCase](val value: String@@T) extends AnyVal {
  //   def unwrap: String = Tag.of[T].unwrap(value)
  // }

  // val SnakeCase = Tag.of[SnakeCase]
  // val SnakeUScoreCase = Tag.of[SnakeUScoreCase]
  // val CamelCase = Tag.of[CamelCase]

  // implicit class RicherStringCase[C <: StringCase](val str: String@@C) extends AnyVal {

  //   def toSnakeCase(): String@@SnakeCase = {
  //     str match {
  //       case t: SnakeCase  => str
  //       case t: SnakeUScoreCase  =>
  //       case t: CamelCase  =>
  //     }

  //     SnakeCase(str.unwrap.replaceAll("([A-Z])", "-$1").toLowerCase.replaceAll("^-", ""))
  //   }

  // }

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
