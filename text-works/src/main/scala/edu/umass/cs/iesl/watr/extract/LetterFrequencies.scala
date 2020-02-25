package edu.umass.cs.iesl.watr
package extract


object CharClasses {
  val Midrisers = "acemnorszuvwx"
  val Ascenders = "bdfhkl"
  val Descenders = "gjpqy"

  object Letter {
    val T = "t"
    val I = "i"
  }

  object Ligatures {
    val ST        = 'ﬆ'
    val STLong    = 'ﬅ'
    val FF        = 'ﬀ'
    val FFL       = 'ﬄ'
    val FFI       = 'ﬃ'
    val FL        = 'ﬂ'
    val FI        = 'ﬁ'
    val OE        = 'œ'
    val OEUpper   = 'Œ'
    val IJLower   = 'ĳ'
    val IJUpper   = 'Ĳ'

    val All = List(
      "ﬆ",
      "ﬅ",
      "ﬀ",
      "ﬄ",
      "ﬃ",
      "ﬂ",
      "ﬁ",
      "œ",
      "Œ",
      "ĳ",
      "Ĳ",
    )
  }

  val Caps = ('A'.toInt to 'Z'.toInt).map(_.toChar).mkString
  val Lowers = ('a'.toInt to 'z'.toInt).map(_.toChar).mkString

  val MostFrequentLetters = "etaoinshrdlu".toArray


  // Most common English bigram/trigrams
  //   Frequencies taken from http://norvig.com/mayzner.html
  val Bigrams: Array[String] = {
    """|th he in er an re on at en nd ti es or te of ed is it
       | al ar st to nt ng se ha as ou io le ve co
       | me de hi ri ro ic ne ea ra ce li ch ll be ma si om ur
       |""".stripMargin.trim.split(" ")

  }

  val Trigrams = {
    """|the and ing ion tio ent ati for her ter hat tha ere ate
       | his con res ver all ons nce men ith ted ers pro thi wit
       | are ess not ive was ect rea com eve per int est sta cti
       | ica ist ear ain one our iti rat
       |""".stripMargin.trim.split(" ")

  }


  def hasCommonBigram(s: String): Boolean = {
    s.toSeq.sliding(2).map(_.unwrap).exists { Bigrams.contains(_) }
  }

  def hasCommonTrigram(s: String): Boolean = {
    s.toSeq.sliding(3).map(_.unwrap).exists { Trigrams.contains(_) }
  }

}
