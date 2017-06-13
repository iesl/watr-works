package edu.umass.cs.iesl.watr.heuristics

object Constants {

    final val WORD_SEPARATORS: Seq[String] = Seq("and", "&")
    final val PUNCTUATION_SEPARATORS: Seq[String] = Seq(",")
    final val NAME_SEPARATOR: String = " "
    final val SPACE_BETWEEN_WORDS_THRESHOLD: Int = 200
    final val COMMA: String = ","
    final val COMMA_CHARACTER: Char = ','
    final val DOT: Char = '.'
    final val PERIOD: String =  "."
    final val BLANK: String = ""
    final val VALID_HEREDITY_SUFFIXES: Seq[String] = Seq("Jr.", "Sr.", "II", "III", "IV")
    final val VALID_SURNAME_PARTICLES: Seq[String] = Seq("van", "von", "der", "de", "du", "da", "di", "do", "la", "del", "della", "ter", "bin", "ben")
    final val FIRST_NAME: String = "first_name"
    final val MIDDLE_NAME: String = "middle_name"
    final val LAST_NAME: String = "last_name"

}
