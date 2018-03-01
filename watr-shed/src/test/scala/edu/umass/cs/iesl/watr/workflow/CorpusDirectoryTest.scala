package edu.umass.cs.iesl.watr
package workflow

import corpora.database.DatabaseTest
import corpora.database.CorpusAccessDB
import TypeTags._

class CorpusDirectoryTest extends DatabaseTest with UserbaseTestHelpers {

  it should "smokescreen init" in new EmptyDatabase {
    implicit val db:CorpusAccessDB = corpusAccessDB

    corpusAccessDB.runqOnce{
      for {
        _ <- corpusAccessDB.tables.corpusPathTables.drop.run
        _ <- corpusAccessDB.tables.corpusPathTables.create.run
      } yield ()
    }

    addDummyDocs(3)

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
