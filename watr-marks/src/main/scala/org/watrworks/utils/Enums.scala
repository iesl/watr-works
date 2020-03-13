package edu.umass.cs.iesl.watr
package utils

import shapeless._

object EnumVals {
  implicit def conv[T](self: this.type)(implicit v: MkEnumVals[T]): Set[T] = EnumVals[T]

  def apply[T](implicit v: MkEnumVals[T]): Set[T] = v.values.toSet

  trait MkEnumVals[T] {
    def values: List[T]
  }

  object MkEnumVals {
    // implicit def values[T, Repr <: Coproduct]
    //   (implicit gen: Generic.Aux[T, Repr], v: Aux[T, Repr]): MkEnumVals[T] =
    //   new MkEnumVals[T] { def values = v.values }
    implicit def values[T, Repr <: Coproduct]
      (implicit v: Aux[T, Repr]): MkEnumVals[T] =
      new MkEnumVals[T] { def values = v.values }

    trait Aux[T, Repr] {
      def values: List[T]
    }

    object Aux {
      implicit def cnilAux[A]: Aux[A, CNil] =
        new Aux[A, CNil] { def values = Nil }

      implicit def cconsAux[T, L <: T, R <: Coproduct]
        (implicit l: Witness.Aux[L], r: Aux[T, R]): Aux[T, L :+: R] =
        new Aux[T, L :+: R] { def values = l.value :: r.values }
    }
  }
}
