package edu.umass.cs.iesl.watr
package textreflow

import scalaz._ 

import matryoshka._
trait StructuredRecursion {

  import patterns.EnvT
  // Natural Transformation  from EnvT ~> TextReflow
  def stripEnv = new (EnvT[Offsets, TextReflowF, ?] ~> TextReflowF[?]) {
    def apply[A](env: EnvT[Offsets, TextReflowF, A]): TextReflowF[A] = {
      env.lower
    }
  }
}
