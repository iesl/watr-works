package org.watrworks
package table

import pprint._
import textboxing.{TextBoxing => TB}

object ShellPrettyPrinters  {
  import TB._

  def additionalHandlers: PartialFunction[Any, Tree] = {
    case c: TB.Box => Tree.Literal((emptyBox(1, 1) atop c).toString())
  }

}
