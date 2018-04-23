package edu.umass.cs.iesl.watr
package extract

// Bigram/trigram frequencies from http://norvig.com/mayzner.html

object LetterFrequencies {
  val MostFrequentLetters = "etaoinshrdlu".toArray

  val CapLetters = ('A'.toInt to 'Z'.toInt).map(_.toChar).mkString
  val LowerLetter = ('a'.toInt to 'z'.toInt).map(_.toChar).mkString

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
    s.sliding(2).exists { Bigrams.contains(_) }
  }

  def hasCommonTrigram(s: String): Boolean = {
    s.sliding(3).exists { Trigrams.contains(_) }
  }

}
