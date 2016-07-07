package edu.umass.cs.iesl.watr
package extract
package fonts

import slick.driver.H2Driver.api._
// import slick.driver.H2Driver.DDL


abstract class AnyToAny(tag: Tag, tableName: String) extends Table[(Int, Int)](tag, tableName) {
  def aCol  = column[Int]("acol")
  def bCol = column[Int]("bcol")

  def * = (aCol, bCol)
}


sealed trait EdgeTable {
  def addEdge(lhs: Identified, rhs: Identified): DBIOAction[Int, NoStream, Effect.Write]
  // def rmEdge(lhs: Identified, rhs: Identified): DBIOAction[Int, NoStream, Effect]
  def rmEdgesRhs(rhs: Identified): DBIOAction[Int, NoStream, Effect.Write]
  // def getSchema: DDL
  // def tableQuery[T <: AnyToAny]: TableQuery[T]
}

trait Identified {
  def id: Int
}

trait EdgeTables {

  import scala.reflect._

  class OneToMany(tag: Tag, tableName:String) extends AnyToAny(tag, tableName) {
    def i0 = index("idx_acol_"+tableName, aCol, unique=false)
    def i1 = index("idx_bcol_"+tableName, bCol, unique=true)
  }

  type OneToManyEdgeTable = TableQuery[OneToMany] with EdgeTable

  def oneToMany[T <: Identified, U <: Identified](
    implicit ttag: ClassTag[T], utag: ClassTag[U]
  ): OneToManyEdgeTable = {
    val tName = ttag.runtimeClass.getSimpleName
    val uName = utag.runtimeClass.getSimpleName
    val tableName = s"${tName}_TO_${uName}".toUpperCase

    lazy val tq: OneToManyEdgeTable = new TableQuery(new OneToMany(_, tableName)) with EdgeTable {

      override def rmEdgesRhs(rhs: Identified): DBIOAction[Int, NoStream, Effect.Write] = {
        val selQ = for {
          f2h <- this if f2h.bCol === rhs.id
        } yield f2h

        selQ.delete
      }

      override def addEdge(lhs: Identified, rhs: Identified): DBIOAction[Int, NoStream, Effect.Write] = {
        sqlu"""
          insert into "#${tableName}"
             select  ${lhs.id}, ${rhs.id}
          where not exists (
            select 1 from "#${tableName}" t
            where t."bcol" = ${rhs.id}
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
