package edu.umass.cs.iesl.watr
package watrcolors
package server

import ammonite.ops._
import edu.umass.cs.iesl.watr.table._


object WatrColorTable {

  def main(args: Array[String]): Unit = {
    val corpus = ShellCommands.initCorpus()
    val server =  new EmbeddedServer(corpus, "localhost", 9999)

    ammonite.Main(
      // storageBackend = new Storage.Folder(Defaults.ammoniteHome)
      predef = WatrTable.predef,
      defaultPredef = true,
      wd = pwd,
      welcomeBanner = Some(s""">> WatrTable+WatrColors @ http://localhost:9999/  <<"""),
      inputStream = System.in,
      outputStream  = System.out,
      errorStream = System.err,
      verboseOutput = false
    ).run(
      "server" -> server,
      "corpus" -> corpus
    )

    server.kill()
  }
}
