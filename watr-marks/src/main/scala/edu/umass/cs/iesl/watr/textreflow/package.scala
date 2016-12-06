package edu.umass.cs.iesl.watr
package textreflow

import matryoshka.data._

object `package` extends TextReflowFunctions {

  type TextReflow = Fix[TextReflowF]

  type TextReflowT = TextReflowF[Fix[TextReflowF]]

}
