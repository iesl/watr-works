package edu.umass.cs.iesl.watr
package corpora

import scala.collection.mutable

trait EdgeTableOneToMany[LhsIDType, RhsIDType] {
  type Lhs = Int@@LhsIDType
  type Rhs = Int@@RhsIDType
  type TableImpl = mutable.HashMap[Lhs, mutable.LinkedHashSet[Rhs]]

  val table: TableImpl = mutable.HashMap[Lhs, mutable.LinkedHashSet[Rhs]]()

  def addEdge(lhs: Lhs, rhs: Rhs): Unit = {
    val rights = table.getOrElseUpdate(lhs, mutable.LinkedHashSet[Rhs]())
    rights.add(rhs)
  }

  def getEdges(lhs: Lhs): Seq[Rhs] = {
    table.getOrElse(lhs, Seq()).toSeq
  }
}


trait EdgeTableOneToOne[LhsIDType, RhsIDType] {
  type Lhs = Int@@LhsIDType
  type Rhs = Int@@RhsIDType
  type TableImpl = mutable.HashMap[Lhs, Rhs]

  val table: TableImpl = mutable.HashMap[Lhs, Rhs]()

  def addEdge(lhs: Lhs, rhs: Rhs): Unit = {
    table.getOrElseUpdate(lhs, rhs)
  }

  def getRhs(lhs: Lhs): Option[Rhs] = {
    table.get(lhs)
  }
}

class DBRelation[IDType, ModelType](
  table: mutable.HashMap[Int@@IDType, ModelType] = mutable.HashMap[Int@@IDType, ModelType]()
)(implicit O: Ordering[Int@@IDType]) {

  def all(): Seq[ModelType] = {
    val keys = table.keys.toSeq
    keys.sorted.map(table(_))
  }

  def option(id: Int@@IDType): Option[ModelType] = table.get(id)

  def unique(id: Int@@IDType): ModelType = {
    option(id).fold(
      sys.error(s"Expected unique ${id}; got None")
    )(x => x)
  }

  lazy val idGen = utils.IdGenerator[IDType]()
  def nextId(): Int@@IDType = idGen.nextId

  def create(f: Int@@IDType => ModelType): ModelType = {
    val id = nextId()
    val rec = f(id)
    table.put(id, rec).foreach { existing =>
      sys.error(s"failed on duplicate insert(${id}), ${rec}")
    }
    rec
  }

  def insert(id: Int@@IDType, m: ModelType): Unit = {
    table.put(id, m).foreach { existing =>
      sys.error(s"failed on duplicate insert(${id}), ${m}")
    }
  }
  def update(id: Int@@IDType, m: ModelType): Option[ModelType] = {
    table.put(id, m)
  }
}
