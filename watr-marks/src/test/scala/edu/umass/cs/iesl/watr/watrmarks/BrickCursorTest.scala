
package edu.umass.cs.iesl.watr
package watrmarks

import java.io.StringReader
import org.scalatest._



class BrickCursorSpec extends FlatSpec {

  import StandardLabels._

  behavior of "labels"

  val runBrick =
    """|| |t~$T| {ns:pos, type: {token: t}, unit: char}
       |  >Run.<
       |""".stripMargin

  it should "navigate chars" in {

    val lspan = biolu.parseBioBrick(runBrick, bioDict, None, None, None)
    val charCursor = lspan.toBrickCursor(CharLabel)

    // charCursor.get.fold{ cur =>
    //   println("cc: " + cur)
    // }



  }

  it should "navigate labels" in {
    val lspan = biolu.parseBioBrick(runBrick, bioDict, None, None, None)
    val charCursor = lspan.toBrickCursor(Token)

    charCursor.get.fold{ cur =>
      println("cc: " + cur)
    }


  }

}
