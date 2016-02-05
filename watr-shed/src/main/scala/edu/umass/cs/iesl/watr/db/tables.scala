package edu.umass.cs.iesl.watr
package db

import slick.driver.PostgresDriver.api._
import java.util.UUID


case class Rect(id: Long, x:Double, y:Double, w:Double, h:Double)

class Rects(tag: Tag) extends Table[Rect](tag, "rects") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

  def x = column[Double]("x")
  def y = column[Double]("y")
  def w = column[Double]("w")
  def h = column[Double]("h")

  def * = (id, x, y, w, h) <> (
    Rect.tupled, Rect.unapply
  )
}

object Rects extends TableQuery(new Rects(_)){
  val ccQuery = (this returning this.map(_.id) into ((f, i) => f.copy(id = i)))
  val byId = this.findBy(_.id)

}

case class Label(id: Long, label: String)

class Labels(tag: Tag) extends Table[Label](tag, "labels") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)

  def label = column[String]("label")

  def * = (id, label) <> (
    Label.tupled, Label.unapply
  )
}

object Labels extends TableQuery(new Labels(_)){
  val ccQuery = (this returning this.map(_.id) into ((f, i) => f.copy(id = i)))
  val byId = this.findBy(_.id)

}
