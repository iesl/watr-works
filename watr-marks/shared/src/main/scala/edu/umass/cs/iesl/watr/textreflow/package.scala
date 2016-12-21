package edu.umass.cs.iesl.watr
package textreflow

import matryoshka.data._

object `package` extends TextReflowFunctions {
  // import acyclic.pkg

  type TextReflow = Fix[TextReflowF]

  type TextReflowT = TextReflowF[Fix[TextReflowF]]

}
