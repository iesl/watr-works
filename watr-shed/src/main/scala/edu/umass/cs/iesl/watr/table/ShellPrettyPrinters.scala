package edu.umass.cs.iesl.watr
package table

import pprint._
import textboxing.{TextBoxing => TB}

object ShellPrettyPrinters  {

  def additionalHandlers: PartialFunction[Any, Tree] = {
    case c: TB.Box    => Tree.Literal(c.toString())
  }

}
