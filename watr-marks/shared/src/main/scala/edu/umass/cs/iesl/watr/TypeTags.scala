package edu.umass.cs.iesl.watr

import scalaz.Tag
import scala.reflect._


sealed trait SHA1String

// Documents are identified by the SHA1 hash of their contents
sealed trait DocumentID extends SHA1String

sealed trait ZoneID
sealed trait LabelID
sealed trait RegionID

sealed trait PageID
sealed trait CharID
sealed trait ComponentID

sealed trait MentionID
sealed trait ClusterID
sealed trait RelationID


sealed trait Ranging
sealed trait Offset
sealed trait Length

sealed trait Percent


object TypeTags extends TypeTags


trait TypeTags {
  val SHA1String = Tag.of[SHA1String]

  val DocumentID = Tag.of[DocumentID]

  val ZoneID = Tag.of[ZoneID]
  val RegionID = Tag.of[RegionID]
  val PageID = Tag.of[PageID]
  val CharID = Tag.of[CharID]
  val ComponentID = Tag.of[ComponentID]
  val LabelID = Tag.of[LabelID]

  val MentionID  = Tag.of[MentionID]
  val ClusterID  = Tag.of[ClusterID]
  val RelationID = Tag.of[RelationID]

  val Ranging = Tag.of[Ranging]
  val Offset = Tag.of[Offset]
  val Length = Tag.of[Length]

  val Percent = Tag.of[Percent]

  val emptyDocId: String@@DocumentID =
    DocumentID("empty")


  def formatTaggedType[T:ClassTag](tt: Int @@ T): String = {
    val tagClsname = implicitly[ClassTag[T]].runtimeClass.getSimpleName
    s"${tagClsname}:${tt.unwrap}"
  }

}



  // implicit def TypeTag_Pickler[U: Pickler, T: Pickler]: Pickler[U @@ T] = {
  //   transformPickler(
  //     (t:Int) => Tag.of[TagT](t))(
  //     t => (t.unwrap))
  // }

  // // (implicit tpickler: Pickler[TagT]): Pickler[Int @@ TagT] = {
  // implicit def IntTypeTag_Pickler[TagT: Pickler]: Pickler[Int @@ TagT] = {
  //   transformPickler(
  //     (t:Int) => Tag.of[TagT](t))(
  //     t => (t.unwrap))
  // }
  // implicit def StringTypeTag_Pickler[TagT]: Pickler[String @@ TagT] = {
  //   transformPickler(
  //     (t:String) => Tag.of[TagT](t))(
  //     t => (t.unwrap))
  // }


// implicit val p00 = compositePickler[SHA1String       ]
// implicit val p01 = compositePickler[DocumentID       ]
// implicit val p02 = compositePickler[ZoneID           ]
// implicit val p03 = compositePickler[LabelID          ]
// implicit val p04 = compositePickler[RegionID         ]
// implicit val p05 = compositePickler[PageID           ]
// implicit val p06 = compositePickler[CharID           ]
// implicit val p07 = compositePickler[ComponentID      ]
// implicit val p08 = compositePickler[MentionID        ]
// implicit val p09 = compositePickler[ClusterID        ]
// implicit val p10 = compositePickler[RelationID       ]
// implicit val p11 = compositePickler[Ranging          ]
// implicit val p12 = compositePickler[Offset           ]
// implicit val p13 = compositePickler[Length           ]
// implicit val p14 = compositePickler[Percent          ]

  // def fromCode(code: Int, v: Any): Any = code match {
  //   case Code_SHA1String  => SHA1String(v)
  //   case Code_DocumentID  => DocumentID(v)
  //   case Code_ZoneID      => ZoneID(v)
  //   case Code_LabelID     => LabelID(v)
  //   case Code_RegionID    => RegionID(v)
  //   case Code_PageID      => PageID(v)
  //   case Code_CharID      => CharID(v)
  //   case Code_ComponentID => ComponentID(v)
  //   case Code_MentionID   => MentionID(v)
  //   case Code_ClusterID   => ClusterID(v)
  //   case Code_RelationID  => RelationID(v)
  //   case Code_Ranging     => Ranging(v)
  //   case Code_Offset      => Offset(v)
  //   case Code_Length      => Length(v)
  //   case Code_Percent     => Percent(v)
  // }


  // val Code_SHA1String  :Int = 0
  // val Code_DocumentID  :Int = 1
  // val Code_ZoneID      :Int = 2
  // val Code_LabelID     :Int = 3
  // val Code_RegionID    :Int = 4
  // val Code_PageID      :Int = 5
  // val Code_CharID      :Int = 6
  // val Code_ComponentID :Int = 7
  // val Code_MentionID   :Int = 8
  // val Code_ClusterID   :Int = 9
  // val Code_RelationID  :Int = 10
  // val Code_Ranging     :Int = 11
  // val Code_Offset      :Int = 12
  // val Code_Length      :Int = 13
  // val Code_Percent     :Int = 14
  // def fromCode(code: Int, v: Any): Any = {
  //   // code match {
  //   //   case Code_SHA1String  => SHA1String(v)
  //   //   case Code_DocumentID  => DocumentID(v)
  //   //   case Code_ZoneID      => ZoneID(v)
  //   //   case Code_LabelID     => LabelID(v)
  //   //   case Code_RegionID    => RegionID(v)
  //   //   case Code_PageID      => PageID(v)
  //   //   case Code_CharID      => CharID(v)
  //   //   case Code_ComponentID => ComponentID(v)
  //   //   case Code_MentionID   => MentionID(v)
  //   //   case Code_ClusterID   => ClusterID(v)
  //   //   case Code_RelationID  => RelationID(v)
  //   //   case Code_Ranging     => Ranging(v)
  //   //   case Code_Offset      => Offset(v)
  //   //   case Code_Length      => Length(v)
  //   //   case Code_Percent     => Percent(v)
  //   // }
  //   ???
  // }


  // def toCode[T: ClassTag]: Int = {
  //   val imp = implicitly[ClassTag[T]]
  //   // implicitly[ClassTag[T]].runtimeClass match {
  //   //   case SHA1String  => 0
  //   //   case DocumentID  => 1
  //   //   case ZoneID      => 2
  //   //   case LabelID     => 3
  //   //   case RegionID    => 4
  //   //   case PageID      => 5
  //   //   case CharID      => 6
  //   //   case ComponentID => 7
  //   //   case MentionID   => 8
  //   //   case ClusterID   => 9
  //   //   case RelationID  => 10
  //   //   case Ranging     => 11
  //   //   case Offset      => 12
  //   //   case Length      => 13
  //   //   case Percent     => 14
  //   // }
  //   0
  // }

  // def TypeTagPickler[A: P, T: ClassTag]: P[A @@ T] = new P[A @@ T] {
  //   override def pickle(obj: A @@ T)(implicit state: PickleState): Unit = {
  //     state.enc.writeInt(toCode[T])
  //     write[A](obj.unwrap)
  //   }

  //   override def unpickle(implicit state: UnpickleState): A @@ T = {
  //     val code = state.dec.readInt
  //     val a = read[A]
  //     fromCode(code, a).asInstanceOf[A @@ T]
  //   }
  // }
