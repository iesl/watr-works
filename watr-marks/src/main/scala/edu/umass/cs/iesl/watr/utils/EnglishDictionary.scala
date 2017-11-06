package edu.umass.cs.iesl.watr
package utils 


object EnglishDictionary {
  import java.io.BufferedInputStream
  import java.util.zip.GZIPInputStream

  def fromWords(words: String*): EnglishDictionary = {
    new EnglishDictionary(words.toSet)
  }

  def apply(): EnglishDictionary = {
    val inputStream = new GZIPInputStream(new BufferedInputStream(this.getClass.getResourceAsStream("/eng-dict.txt.gz")))
    val sourceFile = io.Source.fromInputStream(inputStream)

    val mappings = sourceFile.getLines
      .map(_.toLowerCase())

    new EnglishDictionary(mappings.toSet)
  }

  lazy val global = apply()
}

class EnglishDictionary(mapping: Set[String]) {
  def contains(word: String): Boolean = {
    mapping.contains(word.toLowerCase())
  }

}
