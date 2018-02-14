package edu.umass.cs.iesl.watr
package workflow

import corpora.database.DatabaseTest
import corpora.database.CorpusAccessDB
import textgrid.TextGridBuilder
import TypeTags._


class CorpusDirectoryTest extends DatabaseTest with TextGridBuilder with UserbaseTestHelpers {


  def addSampleDocs(n: Int): Seq[String@@DocumentID] = {
    val doc = List(
      "abc\ndef\nghi",
      "012\n345\n678",
      "jkl\nmno\npqr"
    )
      (0 until n).map{ i =>
        val stableId = DocumentID(s"doc#${i}")
        addDocument(stableId, doc)
        stableId
      }
  }


  it should "smokescreen init" in new EmptyDatabase {
    implicit val db:CorpusAccessDB = reflowDB

    reflowDB.runqOnce{
      DBCorpusDirectories.createTables
    }

    addSampleDocs(3)

    val corpusDirectory = new DatabaseCorpusDirectory()

    val entries = corpusDirectory.listEntries()

    println("Entries")
    println(entries)

    val paths = corpusDirectory.listDirectories()
    println("Paths")
    println(paths)

    //  corpusDirectory.makeDirectory(CorpusPath("A.B"))
    corpusDirectory.makeDirectory(CorpusPath("A.B.C"))
    println("Paths")
    println(corpusDirectory.listDirectories())

    entries.right.foreach(es => es.foreach{entry =>
      println( corpusDirectory.moveEntry(entry, CorpusPath("A.B.C")) )
    })

    entries.right.foreach(es => es.drop(1).foreach{entry =>
      println(
        corpusDirectory.moveEntry(entry, CorpusPath("A.B"))
      )
    })

    println("A.B")
    println(corpusDirectory.listEntries(CorpusPath("A.B")))

    println("A.B.C")
    println(corpusDirectory.listEntries(CorpusPath("A.B.C")))

  }

}
