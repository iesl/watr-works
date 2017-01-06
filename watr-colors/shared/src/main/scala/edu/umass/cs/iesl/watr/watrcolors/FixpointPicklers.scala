
package edu.umass.cs.iesl.watr
package watrcolors

import boopickle._
import matryoshka.data._
import boopickle.DefaultBasic._

trait FixpointPicklers extends PicklerHelper {


  // final val FixCode  : Byte = 1

  // def FixPickler[FA: P]: P[Fix[F]] = new P[Fix[F]] {
  //   override def pickle(obj: Fix[F])(implicit state: PickleState): Unit = {
  //     state.enc.writeInt(FixCode.toInt)
  //     write[F[A]](obj.unFix)
  //   }

  //   override def unpickle(implicit state: UnpickleState): Fix[F[A]] = {
  //     ???
  //   }
  // }
}



// def OptionPickler[T: P]: P[Option[T]] = new P[Option[T]] {
//   override def pickle(obj: Option[T])(implicit state: PickleState): Unit = {
//     obj match {
//       case null =>          state.enc.writeInt(NullObject)
//       case Some(x) =>          state.enc.writeInt(OptionSome.toInt);        write[T](x)
//       case None =>        state.enc.writeInt(OptionNone.toInt)
//     }
//   }
//   override def unpickle(implicit state: UnpickleState): Option[T] = {
//     state.dec.readInt match {
//       case NullObject =>  null
//       case OptionSome =>  val o = Some(read[T]);         o
//       case OptionNone =>  None
//       case _          =>  throw new IllegalArgumentException("Invalid coding for Option type")
//     }
//   }
// }
// }


