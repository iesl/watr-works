package edu.umass.cs.iesl.watr
package heuristics

import scala.util.matching.Regex

object Constants {

    final val WORD_SEPARATORS: Seq[String] = Seq("and", "&")
    final val PUNCTUATION_SEPARATORS: Seq[String] = Seq(",")
    final val SPACE_SEPARATOR: String = " "
    final val SPACE_BETWEEN_WORDS_THRESHOLD: Int = 2
    final val COMMA: String = ","
    final val COMMA_CHARACTER: Char = ','
    final val DOT: Char = '.'
    final val PERIOD: String = "."
    final val BLANK: String = ""
    final val AT_THE_RATE: String = "@"

    final val VALID_HEREDITY_SUFFIXES: Seq[String] = Seq("Jr.", "Sr.", "II", "III", "IV")
    final val VALID_SURNAME_PARTICLES: Seq[String] = Seq("van", "von", "der", "de", "du", "da", "di", "do", "la", "del", "della", "ter", "bin", "ben", "den")
    final val FIRST_NAME: String = "first_name"
    final val MIDDLE_NAME: String = "middle_name"
    final val LAST_NAME: String = "last_name"


    final val CLEANUP_PATTERN: Regex = """^[‡†]|\(\d+\)|\d\.""".r
    final val NAME_INITIAL_FORMAT_PATTERN: Regex = """^[A-Z\.]+$""".r
    final val EMAIL_PATTERN: Regex = """[^@]+@[^@]+\.[^@]+""".r
    final val EMAIL_SUFFIX_PATTERN: Regex = """@[^@]+\.[^@]+""".r

    final val USA_ZIP_CODE_KEYWORD = "USA_STATE"
    final val EUR_ZIP_CODE_KEYWORD = "EUR_CITY"
    final val ASIA_ZIP_CODE_KEYWORD = "ASIA_CITY"
    final val AUS_ZIP_CODE_KEYWORD = "AUS_CITY"
    final val GENERIC_ZIP_CODE_KEYWORD = "ZIP_CODE"
    final val USA_ZIP_CODE_PATTERN: Seq[Regex] = Seq(
        """\b([a-z]+(\ [a-z]+)*|[a-z][a-z])\ \d{5}(\-\d{4})?\b""".r
    )
    final val EUR_ZIP_CODE_PATTERN: Seq[Regex] = Seq(
        """\b([a-z][-–])?\d{5} [a-z]+\b""".r,
        """\b\d{2}-\d{3} [a-z]+\b""".r,
        """\b\d{3}\ \d{2}\b""".r
    )
    final val ASIA_ZIP_CODE_PATTERN: Seq[Regex] = Seq(
        """\b\d{3}-\d{3,4}\b""".r
    )
    final val AUS_ZIP_CODE_PATTERN: Seq[Regex] = Seq(
        """\b.*[a-z]\. \d{4}\b""".r
    )
    final val GENERIC_ZIP_CODE_PATTERN: Seq[Regex] = Seq(
        """\b\d{4,}\b""".r
    )
    final val ZIP_CODE_PATTERNS: Seq[(String, Seq[Regex])] = Seq(
        (USA_ZIP_CODE_KEYWORD, USA_ZIP_CODE_PATTERN),
        (EUR_ZIP_CODE_KEYWORD, EUR_ZIP_CODE_PATTERN),
        (ASIA_ZIP_CODE_KEYWORD, ASIA_ZIP_CODE_PATTERN),
        (AUS_ZIP_CODE_KEYWORD, AUS_ZIP_CODE_PATTERN),
        (GENERIC_ZIP_CODE_KEYWORD, GENERIC_ZIP_CODE_PATTERN)
    )

    final val ADDRESS_KEYWORD = "ADDRESS"
    final val UNIVERSITY_KEYWORD = "UNIVERSITY"
    final val INSTITUTION_KEYWORD = "INSTITUTION"
    final val DEPARTMENT_KEYWORD = "DEPARTMENT"
    final val FACULTY_KEYWORD = "FACULTY"
    final val COUNTRY_KEYWORD = "COUNTRY"
    final val COMPANY_KEYWORD = "COMPANY"
    final val CITY_KEYWORD = "CITY"
    final val EMAIL_KEYWORD = "EMAIL"
    final val REGION_KEYWORD = "REGION"
    final val TEST_KEYWORD = "TEST"
    final val CITY_RESOURCE_FILE = "/city_full.txt"
    final val COMPANY_RESOURCE_FILE = "/company_keywords.txt"
    final val COUNTRY_RESOURCE_FILE = "/country_full.txt"
    final val DEPARTMENT_RESOURCE_FILE = "/department_keywords.txt"
    final val FACULTY_RESOURCE_FILE = "/faculty_keywords.txt"
    final val INSTITUTION_RESOURCE_FILE = "/institution_keywords.txt"
    final val REGION_RESOURCE_FILE = "/region_full.txt"
    final val UNIVERSITY_RESOURCE_FILE = "/university_keywords.txt"
    final val TEST_RESOURCE_FILE = "/test.txt"
    final val RESOURCE_KEYWORDS: Seq[(String, String)] = Seq(
        (DEPARTMENT_KEYWORD, DEPARTMENT_RESOURCE_FILE),
        (INSTITUTION_KEYWORD, INSTITUTION_RESOURCE_FILE),
        (UNIVERSITY_KEYWORD, UNIVERSITY_RESOURCE_FILE),
        (COMPANY_KEYWORD, COMPANY_RESOURCE_FILE),
        (CITY_KEYWORD, CITY_RESOURCE_FILE),
        (REGION_KEYWORD, REGION_RESOURCE_FILE),
        (COUNTRY_KEYWORD, COUNTRY_RESOURCE_FILE)
    )

    final val ACADEMIA_KEYWORDS: Seq[String] = Seq(DEPARTMENT_KEYWORD, INSTITUTION_KEYWORD, UNIVERSITY_KEYWORD)
    final val LOCATION_KEYWORDS: Seq[String] = Seq(CITY_KEYWORD, REGION_KEYWORD, COUNTRY_KEYWORD)

    final val AFFILIATION_LABELS: Seq[String] = Seq(DEPARTMENT_KEYWORD, INSTITUTION_KEYWORD, UNIVERSITY_KEYWORD, COMPANY_KEYWORD, CITY_KEYWORD, REGION_KEYWORD, COUNTRY_KEYWORD)

    final val PUNCTUATIONS_PATTERN: Array[Char] = Array('\'', '!', '"', '#', '$', '%', '&',
        '(', ')', '*', '+', ',',
        '-', '.', '/', ':', ';',
        '<', '=', '>', '?', '@',
        '[', '\\', ']', '^', '_',
        '`', '{', '|', '}', '~')
    final val PUNCTUATION_TAG = "<PUNCT>"
    final val NUMBER_TAG = "<NUM>"

}
