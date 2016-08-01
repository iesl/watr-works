package edu.umass.cs.iesl.watr
package db

import slick.driver.H2Driver.api._


abstract class AnyToAny(tag: Tag, tableName: String) extends Table[(Int, Int)](tag, tableName) {
  def srcId  = column[Int]("srcid")
  def dstId = column[Int]("dstid")

  def * = (srcId, dstId)
}


class OneToMany(tag: Tag, tableName:String) extends AnyToAny(tag, tableName) {
  def i0 = index("idx_srcid_"+tableName, srcId, unique=false)
  def i1 = index("idx_dstId_"+tableName, dstId, unique=true)

}



class OneToOne(tag: Tag, tableName:String) extends AnyToAny(tag, tableName) {
  def i0 = index("idx_srcid_"+tableName, srcId, unique=true)
  def i1 = index("idx_dstId_"+tableName, dstId, unique=true)
}


sealed trait EdgeTable {

  def addEdge(src: Identified, dst: Identified): DBIOAction[Int, NoStream, Effect.Write]

  def deleteAdjacentToSrc(node: Identified): DBIOAction[Int, NoStream, Effect.Write]
  def deleteAdjacentToDst(node: Identified): DBIOAction[Int, NoStream, Effect.Write]

  def selectAdjacentToSrc(node: Identified): DBIOAction[Seq[Int], NoStream, Effect.Read]
  def selectAdjacentToDst(node: Identified): DBIOAction[Seq[Int], NoStream, Effect.Read]

}


trait Identified {
  def id: Int
}


trait EdgeTables {

  type OneToManyEdgeTable = TableQuery[OneToMany] with EdgeTable
  type OneToOneEdgeTable = TableQuery[OneToOne] with EdgeTable

  import scala.reflect._

  def createAnyToAnyTableName[T, U](
    implicit ttag: ClassTag[T], utag: ClassTag[U]
  ): String = {
    val tName = ttag.runtimeClass.getSimpleName
    val uName = utag.runtimeClass.getSimpleName
    s"${tName}_to_${uName}".toUpperCase
  }

  import slick.lifted.Query

  def oneToMany[T <: Identified, U <: Identified](
    implicit ttag: ClassTag[T], utag: ClassTag[U]
  ): OneToManyEdgeTable = {
    val tableName = createAnyToAnyTableName[T, U]

    lazy val tq: OneToManyEdgeTable = new TableQuery(new OneToMany(_, tableName)) with EdgeTable {

      def adjacentToSrc(node: Identified): Query[OneToMany, (Int, Int), Seq] = filter(_.srcId===node.id)
      def adjacentToDst(node: Identified): Query[OneToMany, (Int, Int), Seq] = filter(_.dstId===node.id)

      def deleteAdjacentToSrc(node: Identified)= adjacentToSrc(node).delete
      def deleteAdjacentToDst(node: Identified)= adjacentToDst(node).delete
      def selectAdjacentToSrc(node: Identified)= adjacentToSrc(node).map(_.dstId).result
      def selectAdjacentToDst(node: Identified)= adjacentToDst(node).map(_.srcId).result

      // TODO use tighter bounds than Identified
      override def addEdge(lhs: Identified, rhs: Identified): DBIOAction[Int, NoStream, Effect.Write] = {
        sqlu"""
          insert into "#${tableName}"
             select  ${lhs.id}, ${rhs.id}
          where not exists (
            select 1 from "#${tableName}" t
            where t."dstid" = ${rhs.id}
          )"""
      }
    }

    tq
  }

  def oneToOne[T <: Identified, U <: Identified](
    implicit ttag: ClassTag[T], utag: ClassTag[U]
  ): OneToOneEdgeTable = {
    val tableName = createAnyToAnyTableName[T, U]

    lazy val tq: OneToOneEdgeTable = new TableQuery(new OneToOne(_, tableName)) with EdgeTable {

      def adjacentToSrc(node: Identified): Query[OneToOne, (Int, Int), Seq] = filter(_.srcId===node.id)
      def adjacentToDst(node: Identified): Query[OneToOne, (Int, Int), Seq] = filter(_.dstId===node.id)

      def deleteAdjacentToSrc(node: Identified)= adjacentToSrc(node).delete
      def deleteAdjacentToDst(node: Identified)= adjacentToDst(node).delete
      def selectAdjacentToSrc(node: Identified)= adjacentToSrc(node).map(_.dstId).result
      def selectAdjacentToDst(node: Identified)= adjacentToDst(node).map(_.srcId).result



      override def addEdge(lhs: Identified, rhs: Identified): DBIOAction[Int, NoStream, Effect.Write] = {
        sqlu"""
          insert into "#${tableName}"
             select  ${lhs.id}, ${rhs.id}
          where not exists (
            select 1 from "#${tableName}" t
            where t."dstid" = ${rhs.id}
          )"""
      }
    }

    tq
  }
}








//         val upsertV2  = sqlu"""
//           WITH upsert AS (UPDATE published_tos
//           SET
//              this_publication_id=${upirt.thisPublicationId},
//              parent_publication_id=${upirt.parentPublicationId},
//              forum_id=${upirt.forumId}
//           WHERE
//                this_publication_id = ${upirt.thisPublicationId} AND forum_id = ${upirt.forumId}
//           RETURNING *)
//           INSERT INTO published_tos
//              select nextval('published_tos_id_seq'), ${upirt.thisPublicationId}, ${upirt.parentPublicationId}, ${upirt.forumId}
//           WHERE NOT EXISTS (SELECT * FROM upsert)
//         """

//   def upsertEmail(i: Email)(implicit ec: ExecutionContext) = for {
//     _ <- sqlu"""
// WITH upsert AS (
//   UPDATE "emails"
//   SET
//      confirmed=${i.confirmed}
//   WHERE
//      email = ${i.email}
//   RETURNING *
// )
// INSERT INTO "emails"
//    SELECT nextval('emails_id_seq'), ${i.email}, ${i.confirmed}
// WHERE NOT EXISTS (SELECT * FROM upsert)
// """
//     eupdate <- findByEmail(i.email).result.head
//   } yield eupdate
// }

// class oneToManyEdgeTable(tableName: String) extends TableQuery(new OneToMany(_, tableName)) with EdgeTable {
//   def addEdge(lhs: Identified, rhs: Identified): DBIOAction[Int, NoStream, Effect.Write]= {
//     sqlu"""
//       insert into "#${tableName}"
//          select  ${lhs.id}, ${rhs.id}
//       where not exists (
//         select 1 from "#${tableName}" t
//         where t."dstid" = ${rhs.id}
//       )"""

//     ???
//   }

//   def deleteAdjacentToDst(rhs: Identified): DBIOAction[Int, NoStream, Effect.Write] = {
//     val selQ = for {
//       f2h <- this if f2h.dstId === rhs.id
//     } yield f2h

//     selQ.delete

//     ???
//   }

//   def selectAdjacentToSrc[S <: Identified, D <: Identified](src: S): DBIOAction[D, NoStream, Effect.Read] = {

//     ???
//   }

//   def selectAdjacentToDst[S <: Identified, D <: Identified](dst: D): DBIOAction[S, NoStream, Effect.Read] = {

//     ???
//   }

// }

  // trait TableAux[+A, +E[_] <: AbstractTable[_]] {
  //   type T = E[A]
  // }

  // trait Aux[+A <: Identified, +E[_] <: AbstractTable[_], T <: TableAux[A, E]] {
  //   type T[AA, AT] = TableAux[A, AbstractTable]
  //   // type TQuery = TableQuery[T]
  // }

  // trait Aux[A <: Identified, E <: AbstractTable[A], TQ <: TableQuery[E]] {
  // trait Aux[+A <: Identified, +E <: AbstractTable[A]] {
  //   type TQ = TableQuery[E]
  // }

  // def oneToManyZZ[
  //   A1 <: Identified, E1 <: AbstractTable[A1], TQ1 <: TableQuery[E1],
  //   A2 <: Identified, E2 <: AbstractTable[A2], TQ2 <: TableQuery[E2]
  // ](
  //   t1: TQ1, t2: TQ2
  // )(
  //   implicit ttag: ClassTag[A1], utag: ClassTag[A2]
  // ): Unit = {
  // def oneToManyZZ[
  //   A1 <: Identified, A2 <: Identified
  // ](
  //   t1: Aux[A1]#T[A1, AbstractTable]#T,
  //   t2: Aux[A2]#TQuery
  // )(
  //   implicit ttag: ClassTag[A1], utag: ClassTag[A2]
  // ): Unit = {
  //   val tableName = createAnyToAnyTableName[A1, A2]

  //   lazy val tq: OneToManyEdgeTable = new TableQuery(new OneToMany(_, tableName)) with EdgeTable {


  //     override def addEdge(lhs: A1, rhs: A2): DBIOAction[Int, NoStream, Effect.Write] = {
  //       sqlu"""
  //         insert into "#${tableName}"
  //            select  ${lhs.id}, ${rhs.id}
  //         where not exists (
  //           select 1 from "#${tableName}" t
  //           where t."dstid" = ${rhs.id}
  //         )"""
  //     }

  //     def deleteAdjacentToDst(dst: A2): DBIOAction[Int, NoStream, Effect.Write] = {
  //       (for { src2dst <- this if src2dst.dstId === dst.id } yield src2dst).delete
  //     }
  //     def deleteAdjacentToSrc(src: A1): DBIOAction[Int, NoStream, Effect.Write] = {
  //       (for { src2dst <- this if src2dst.srcId === src.id } yield src2dst).delete
  //     }

  //     // def selectAdjacentToSrc(src: A1): DBIOAction[A2, NoStream, Effect.Read] = {
  //     //   val q = for {
  //     //     src2dst <- this
  //     //     src0 <- t1
  //     //     dst0 <- t2
  //     //     if src2dst.srcId === src.id // && dst. === src2dst.dstId
  //     //   } yield {}
  //     //   q.result
  //     //   ???
  //     // }
  //   }
  // }
