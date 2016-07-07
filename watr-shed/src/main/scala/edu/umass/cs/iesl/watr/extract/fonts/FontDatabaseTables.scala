package edu.umass.cs.iesl.watr
package extract
package fonts

import org.slf4j.LoggerFactory
// import scala.concurrent._
// import ammonite.ops._


import slick.driver.H2Driver.api._

// import scala.concurrent._
// import scala.concurrent.duration._




object FontDatabaseTables extends EdgeTables {
  val log = LoggerFactory.getLogger(this.getClass)

  //   def filename = foreignKey("filename_fk", file, fileChecks)(_.filename, onDelete = ForeignKeyAction.Cascade)

  final case class Font(id: Int=0) extends Identified

  class Fonts(tag: Tag) extends Table[Font](tag, "FONTS") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def * = id <> (Font.apply, Font.unapply)
  }

  object fonts extends TableQuery(new Fonts(_)) {
    val ccQuery = (this returning this.map(_.id) into ((f, i) => f.copy(id = i)))
    def findById(id: Int) = this.findBy(_.id).apply(id).result.headOption
  }



  final case class Family(family: String, id: Int=0)

  class Families(tag: Tag) extends Table[Family](tag, "FAMILIES") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def family = column[String]("family")  // unique key
    def * = (family, id) <> (Family.tupled, Family.unapply)

    def familyIdx = index("idx_family", family, unique = true)
  }

  object families extends TableQuery(new Families(_)) {
    val ccQuery = (this returning this.map(_.id) into ((f, i) => f.copy(id = i)))
    def findById(id: Int) = this.findBy(_.id).apply(id).result.headOption
  }



  final case class Name(name: String, id: Int=0)

  class Names(tag: Tag) extends Table[Name](tag, "NAME") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def name = column[String]("name")  // unique key
    def * = (name, id) <> (Name.tupled, Name.unapply)
  }

  object names extends TableQuery(new Names(_)) {
    val ccQuery = (this returning this.map(_.id) into ((f, i) => f.copy(id = i)))
    def findById(id: Int) = this.findBy(_.id).apply(id).result.headOption
  }


  final case class SplineHash(hash: String, id: Int=0) extends Identified {
    override def toString = s"#${hash.take(3).mkString}"
  }

  class SplineHashes(tag: Tag) extends Table[SplineHash](tag, "SPLINEHASHES") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def hash = column[String]("hash")
    def * = (hash, id) <> (SplineHash.tupled, SplineHash.unapply)

    def hashIdx = index("idx_hash", hash, unique = true)

  }

  object splineHashes extends TableQuery(new SplineHashes(_)) {
    val ccQuery = (this returning this.map(_.id) into ((f, i) => f.copy(id = i)))
    def findById(id: Int) = this.findBy(_.id).apply(id).result.headOption
    def findByHash(h: String) = this.findBy(_.hash).apply(h).result.headOption
  }

  val fontToHash = oneToMany[Font, SplineHash]

  def schemas = (names.schema ++
    splineHashes.schema ++
    families.schema ++
    fonts.schema ++
    fontToHash.schema
  )

}














