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
  def addEdge(lhs: Identified, rhs: Identified): DBIOAction[Int, NoStream, Effect]
  // def getSchema: DDL
  // def tableQuery[T <: AnyToAny]: TableQuery[T]
}

trait Identified {
  def id: Int
}

trait EdgeTables {

  import scala.reflect._

  def manyToOne[T <: Identified, U <: Identified](
    implicit ttag: ClassTag[T], utag: ClassTag[U]
  ): EdgeTable = {
    val tName = ttag.runtimeClass.getSimpleName
    val uName = utag.runtimeClass.getSimpleName
    val tableName = s"${tName}_TO_${uName}".toUpperCase

    class OneToMany(tag: Tag) extends AnyToAny(tag, tableName) {
      def i0 = index("idx_acol_"+tableName, aCol, unique=false)
      def i1 = index("idx_bcol_"+tableName, bCol, unique=false)
    }


    lazy val tq: TableQuery[OneToMany] with EdgeTable = new TableQuery(new OneToMany(_)) with EdgeTable {

      // override def tableQuery: TableQuery[OneToMany] = tq

      // override def getSchema: DDL = tq.schema

      def addEdge(lhs: Identified, rhs: Identified): DBIOAction[Int, NoStream, Effect] = {
        sqlu"""
          insert into "#${tableName}"
             select  ${lhs.id}, ${rhs.id}
          where not exists (
            select 1 from "#${tableName}" t
            where t."acol" = ${lhs.id} AND t."bcol" = ${rhs.id}
          )"""
      }
    }

    tq
  }

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

      override def addEdge(lhs: Identified, rhs: Identified): DBIOAction[Int, NoStream, Effect] = {
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
