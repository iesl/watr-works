package edu.umass.cs.iesl.watr
package predsynth

object genericjson {

  import shapeless._
  import HList._
  import play.api.libs.json._
  import play.api.libs.functional._


  // object FormatHelper {
  //   def apply[C](
  //     gen: Generic[C]
  //   )(implicit
  //     reads: Reads[gen.Repr]
  //   ): Format[C] =
  //     new Format[C] {
  //   }

  // }

  // object flatFormat {
  //   def apply[C](implicit
  //     gen: Generic[C],
  //     en: Reads[Generic[C]]
  //   ): Format[C] = new Format[C] {
  //       override def reads(json: JsValue): JsResult[C] =  {
  //         json.validate
  //           .map(gen.from(_))
  //       }
  //       override def writes(o: C): JsValue = JsNull
  //   }
  // }


  private def toHList[H : Reads, T <: HList : Reads] (
    l: List[JsValue]) (
    implicit applicative: Applicative[JsResult]
  ): JsResult[H :: T] = l match {

    case scala.::(head, tail) =>
      applicative.apply(
        //JsResult[T => H :: T]
        applicative.map(
          implicitly[Reads[H]].reads(head),
          (h: H) => (t: T) => h :: t
        ),
        //JsResult[T]
        implicitly[Reads[T]].reads(JsArray(tail))
      )
    case _ => JsError("can't convert empty list using multi-element HList")
  }

  implicit def HNilReads = Reads[HNil]{ js => js match {
    case JsArray(values) if(values.isEmpty)   => JsSuccess(HNil)
    case JsObject(values) if(values.isEmpty)  => JsSuccess(HNil)
    case _                                    => JsError("Not empty JsArray or JsObject")
  } }

  implicit def hlistReads[H : Reads, T <: HList : Reads](implicit applicative: Applicative[JsResult]) = Reads[H :: T]{ js =>
    js match {
      case arr: JsArray   => toHList[H, T](arr.value.toList)
      case obj: JsObject  => toHList[H, T](obj.values.toList)
      case js             => toHList[H, T](List(js))
        //JsError("Single JsValue can't be mapped to multi-element HList")
    }
  }

  implicit def HNilWrites = Writes[HNil]{ hl => JsArray() }

  implicit def hlistWrites[H : Writes, T <: HList : Writes] = Writes[H :: T]{ hl =>
    val head :: tail = hl
    implicitly[Writes[H]].writes(head) +: implicitly[Writes[T]].writes(tail).as[JsArray]
  }


  // implicit class TupleReadsOps[ P <: Product ](r: Reads[P]) {
  //   def hlisted(implicit hlister : HLister[P]) = r map { _.hlisted }
  // }

  // implicit class TupleWritesOps[ P <: Product ](w: Writes[P]) {
  //   def contramap[A, B](wa:Writes[A], f: B => A): Writes[B] = Writes[B]( b => wa.writes(f(b)) )
  //   def hlisted[T <: HList](implicit hlister : HLister[P], tupler: TuplerAux[T, P]) = contramap(w, (hl: T) => hl.tupled)
  // }

  // def JsArrayIso[L <: HList](implicit r: Reads[L], w: Writes[L]): Generic.Aux[JsArray, L] = new Generic[JsArray] {
  // def JsArrayGeneric[L <: HList](implicit r: Reads[L], w: Writes[L]) = new Generic[JsArray] {
  //   def to(t: JsArray): L = r.reads(t).get
  //   def from(l: L): JsArray = w.writes(l).asInstanceOf[JsArray]
  // }


}
