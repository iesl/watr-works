package edu.umass.cs.iesl.watr
package heuristics

import scala.util.matching.Regex

object Constants {

    final val WORD_SEPARATORS: Seq[String] = Seq("and", "&")
    final val PUNCTUATION_SEPARATORS: Seq[String] = Seq(",")
    final val SPACE_SEPARATOR: String = " "
    final val SPACE_BETWEEN_WORDS_THRESHOLD: Int = 200
    final val COMMA: String = ","
    final val COMMA_CHARACTER: Char = ','
    final val DOT: Char = '.'
    final val PERIOD: String =  "."
    final val BLANK: String = ""

    final val VALID_HEREDITY_SUFFIXES: Seq[String] = Seq("Jr.", "Sr.", "II", "III", "IV")
    final val VALID_SURNAME_PARTICLES: Seq[String] = Seq("van", "von", "der", "de", "du", "da", "di", "do", "la", "del", "della", "ter", "bin", "ben", "den")
    final val FIRST_NAME: String = "first_name"
    final val MIDDLE_NAME: String = "middle_name"
    final val LAST_NAME: String = "last_name"

    final val UNIVERSITY_KEYWORD = "UNIVERSITY"
    final val INSTITUTION_KEYWORD = "INSTITUTION"
    final val DEPARTMENT_KEYWORD = "DEPARTMENT"
    final val FACULTY_KEYWORD = "FACULTY"
    final val COUNTRY_KEYWORD = "COUNTRY"
    final val COMPANY_KEYWORD = "COMPANY"
    final val CITY_KEYWORD = "CITY"
    final val EMAIL_KEYWORD = "EMAIL"

    final val CLEANUP_PATTERN: Regex = """^[‡†\d]|\(\d+\)""".r
    final val NAME_INITIAL_FORMAT_PATTERN: Regex = """^[A-Z\.]+$""".r
    final val EMAIL_PATTERN: Regex = """[^@]+@[^@]+\.[^@]+""".r

    final val USA_ZIP_CODE_KEYWORD = "USA_STATE"
    final val EUR_ZIP_CODE_KEYWORD = "EUR_CITY"
    final val ASIA_ZIP_CODE_KEYWORD = "ASIA_CITY"
    final val AUS_ZIP_CODE_KEYWORD = "AUS_CITY"
    final val USA_ZIP_CODE_PATTERN: Seq[Regex] = Seq("""\b([A-Z][a-z]+(\ [A-Z][a-z]+)*|[A-Z][A-Z])\ \d{5}(\-\d{4})?\b""".r)
    final val EUR_ZIP_CODE_PATTERN: Seq[Regex] = Seq("""\b([A-Z][-–])?\d{4,5} [A-Z][a-z]+\b""".r, """\b\d{2}-\d{3} [A-Z][a-z]+\b""".r, """\b\d{3}\ \d{2}\b""".r)
    final val ASIA_ZIP_CODE_PATTERN: Seq[Regex] = Seq("""\b\d{3}-\d{3,4}\b""".r)
    final val AUS_ZIP_CODE_PATTERN: Seq[Regex] = Seq("""\b.*[A-Z]\. \d{4}\b""".r)
    final val ZIP_CODE_PATTERNS: Map[String, Seq[Regex]] = Map(USA_ZIP_CODE_KEYWORD -> USA_ZIP_CODE_PATTERN, EUR_ZIP_CODE_KEYWORD -> EUR_ZIP_CODE_PATTERN, ASIA_ZIP_CODE_KEYWORD -> ASIA_ZIP_CODE_PATTERN, AUS_ZIP_CODE_KEYWORD -> AUS_ZIP_CODE_PATTERN)

}
