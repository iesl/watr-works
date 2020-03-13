package edu.umass.cs.iesl.watr
package geometry

import textboxing.{TextBoxing => TB}

sealed trait TraceLog

object TraceLog {

  case class Message(s: TB.Box)        extends TraceLog { override val toString = s"""${s}""" }

}
