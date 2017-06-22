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
    final val USA_STATE_REGION_KEYWORD = "USA_STATE_REGION"

    final val CLEANUP_PATTERN: Regex = """^[‡†]|\(\d+\)""".r
    final val NAME_INITIAL_FORMAT_PATTERN: Regex = """^[A-Z\.]+$""".r
    final val EMAIL_PATTERN: Regex = """[^@]+@[^@]+\.[^@]+""".r
    final val USA_STATE_ZIP_CODE_PATTERN: Regex = """^[A-Z][A-Z]\ \d\d\d\d\d$""".r

}
