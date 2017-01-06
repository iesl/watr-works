package edu.umass.cs.iesl.watr
package textreflow


// import matryoshka._
// import matryoshka.data._
// import matryoshka.implicits._

import boopickle._
import boopickle.DefaultBasic._

// import java.nio.ByteBuffer
import geometry._

trait TextReflowBoopicklers extends GeometryBoopicklers {
  // import TextReflowF._
  // import watrmarks.Label

  implicit def TextReflowPickler: P[TextReflow] = new P[TextReflow] {
    override def pickle(tr: TextReflow)(implicit state: PickleState): Unit = {
      Pickle.intoBytes(tr)
      // pickleTextReflow(tr, state)
    }

    override def unpickle(implicit state: UnpickleState): TextReflow = {
      // unpickleTextReflow(state)
      state.unpickle[TextReflow]
      // Unpickle[TextReflow].fromBytes()
    }
  }
}

  // final val ConstAtom    : Byte = 1
  // final val ConstInsert  : Byte = 2
  // final val ConstRewrite : Byte = 3
  // final val ConstBracket : Byte = 4
  // final val ConstFlow    : Byte = 5
  // final val ConstLabeled : Byte = 6

  // def writeFCode(tr: TextReflowF[_], pstate: PickleState): Unit = {
  //   val code = tr match {
  //     case f:Atom       => ConstAtom
  //     case f:Insert     => ConstInsert
  //     case f:Rewrite[_] => ConstRewrite
  //     case f:Bracket[_] => ConstBracket
  //     case f:Flow[_]    => ConstFlow
  //     case f:Labeled[_] => ConstLabeled
  //   }
  //   pstate.enc.writeInt(code.toInt)
  // }

  // def pickleTextReflow(textReflow: TextReflow, pstate: PickleState): Unit = {
  //   implicit val ps = pstate
  //   textReflow.universe.foreach({ tr =>
  //     println(s"pickle: ${tr}")

  //     tr.unFix match {
  //       case tr @ Atom(c)               => writeFCode(tr, ps); write(c)
  //       case tr @ Insert(v)             => writeFCode(tr, ps); write(v)
  //       case tr @ Rewrite(from, to)     => writeFCode(tr, ps); write(to)
  //       case tr @ Bracket(pre, post, a) => writeFCode(tr, ps); write(pre); write(post)
  //       case tr @ Flow(atoms)           => writeFCode(tr, ps); ps.enc.writeInt(atoms.length)
  //       case tr @ Labeled(ls, a)        => writeFCode(tr, ps); write(ls)
  //     }
  //   })

  // }


  // def unpickleTextReflow(upickleState: UnpickleState): TextReflow = {

  //   def unfold: Coalgebra[TextReflowF, UnpickleState] = ustate => {
  //     implicit val u0: UnpickleState = ustate
  //     ustate.dec.readInt match {
  //       case ConstAtom     => Atom(read[CharAtom])
  //       case ConstInsert   => Insert(read[String])
  //       case ConstRewrite  => Rewrite(u0, read[String])
  //       case ConstBracket  => Bracket(read[String], read[String], u0)
  //       case ConstFlow     =>
  //         val len = ustate.dec.readInt
  //         val as = (0 until len).toList.map(_ => u0)
  //         Flow(as)
  //       case ConstLabeled  => Labeled(read[Set[Label]], u0)
  //     }
  //   }

  //   def refold: GAlgebra[(TextReflow, ?), TextReflowF, TextReflow] = t => fixf(t match {
  //     case Atom(c)                        => Atom(c)
  //     case Insert(value)                  => Insert(value)
  //     case Rewrite ((from, attr), to)     => Rewrite(attr, to)
  //     case Bracket (pre, post, (a, attr)) => Bracket(pre, post, attr)
  //     case Flow(atomsAndattrs)            => Flow(atomsAndattrs.map(_._2))
  //     case Labeled(labels, (a, attr))     => Labeled(labels, attr)
  //   })

  //   upickleState
  //     .ana[TextReflow](unfold)
  //     .cata(attributePara(refold))
  //     .toPair._1
  // }









  // def dopickle(ft:TextReflowF[_]): State[PickleState, PickleState] = {
  //   for {
  //     ps <- State.get[PickleState]
  //   } yield {
  //     ft match {
  //       case tr @ Atom(c)               => writeFCode(tr, ps); write(c)
  //       case tr @ Insert(v)             => writeFCode(tr, ps); write(v)
  //       case tr @ Rewrite(from, to)     => writeFCode(tr, ps); write(to)
  //       case tr @ Bracket(pre, post, a) => writeFCode(tr, ps); write(pre); write(post)
  //       case tr @ Flow(atoms)           => writeFCode(tr, ps); pstate.enc.writeInt(atoms.length)
  //       case tr @ Labeled(ls, a)        => writeFCode(tr, ps); write(ls)
  //     }
  //     ps
  //   }
  // }
  // textReflow
  //   .attributeTopDownM[State[PickleState, ?], PickleState](pstate)({
  //     case e => dopickle(e._1, e._2)
  //   })
  // def pickleTextReflow(textReflow: TextReflow): ByteBuffer = {
  //   def encodeTextReflow(t: TextReflowF[(TextReflow, ByteBuffer)]): ByteBuffer = t match {
  //     case tr @ Atom(c)                          =>
  //       implicit val pstate = pickle(tr)
  //       write(c)
  //       pstate.toByteBuffer

  //     case tr @ Insert (v)                   =>
  //       implicit val pstate = pickle(tr)
  //       write(v)
  //       pstate.toByteBuffer

  //     case tr @ Rewrite ((from, attr), to)     =>
  //       val pstate = pickle(tr)
  //       write(to)
  //       write(attr)
  //       pstate.toByteBuffer

  //     case tr @ Bracket (pre, post, (a, attr)) =>
  //       val pstate = pickle(tr)
  //       write(pre)
  //       write(post)
  //       write(attr)
  //       pstate.toByteBuffer

  //     case tr @ Flow(atomsAndAttrs)              =>
  //       val pstate = pickle(tr)
  //       val len = atomsAndAttrs.length
  //       pstate.enc.writeInt(len)
  //       atomsAndAttrs.foreach{ case (_, bb) => write(bb) }
  //       pstate.toByteBuffer

  //     case tr @ Labeled(labels, (a, attr))     =>
  //       val pstate = pickle(tr)
  //       write(labels)
  //       write(attr)
  //       pstate.toByteBuffer
  //   }

  //   textReflow
  //     .cata(attributePara(encodeTextReflow))
  //     .toPair._1
  // }

// def pickle(tr: TextReflowF[_]): PickleState = {
//   def pstate = new PickleState(new EncoderSize, true, true)
//   val code = tr match {
//     case f:Atom       => ConstAtom
//     case f:Insert     => ConstInsert
//     case f:Rewrite[_] => ConstRewrite
//     case f:Bracket[_] => ConstBracket
//     case f:Flow[_]    => ConstFlow
//     case f:Labeled[_] => ConstLabeled
//   }
//   pstate.enc.writeInt(code.toInt)
//   pstate
// }
