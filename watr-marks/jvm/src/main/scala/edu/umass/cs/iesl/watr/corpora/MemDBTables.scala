package edu.umass.cs.iesl.watr
package corpora

import scala.reflect._
import scala.collection.mutable

abstract class EdgeTableOneToMany[LhsIDType: ClassTag, RhsIDType: ClassTag] {
  type Lhs = Int@@LhsIDType
  type Rhs = Int@@RhsIDType
  type TableImpl = mutable.HashMap[Lhs, mutable.LinkedHashSet[Rhs]]

  val lhsCls = implicitly[ClassTag[LhsIDType]].runtimeClass.getSimpleName
  val rhsCls = implicitly[ClassTag[RhsIDType]].runtimeClass.getSimpleName

  val table: TableImpl = mutable.HashMap[Lhs, mutable.LinkedHashSet[Rhs]]()

  def debugPrint(): Unit = {
    // println(s"Edges: $modelClsStr")
    val str = table.map{ case (lhs, rhss) =>
      s"Edges ${lhsCls} => ${rhsCls}\n" +
        s"    ${lhs} -> " +
        rhss.mkString("\n      ", "\n      ", "\n")
    }.mkString("\n  ", "\n  ", "\n---")
    println(str)
  }

  def addEdge(lhs: Lhs, rhs: Rhs): Unit = {
    val rights = table.getOrElseUpdate(lhs, mutable.LinkedHashSet[Rhs]())
    rights.add(rhs)
  }

  def removeEdgesFrom(lhs: Lhs): Unit = {
    table.remove(lhs)
  }

  def removeEdgesTo(rhs: Rhs): Unit = {
    table.foreach {case (_, rs) =>
      rs.remove(rhs)
    }
  }

  def removeEdge(lhs: Lhs, rhs: Rhs): Unit = {
    table.get(lhs)
      .foreach(_.remove(rhs))
  }

  def getEdges(): Seq[(Lhs, Rhs)] = {
    for {
      lhs <- table.keys.toSeq
      rhs <- getEdges(lhs)
    } yield {
      (lhs, rhs)
    }
  }

  def getEdges(lhs: Lhs): Seq[Rhs] = {
    table.getOrElse(lhs, Seq()).toSeq
  }
  def getEdgeUnique(lhs: Lhs): Rhs = {
    getEdgeOption(lhs)
      .getOrElse {
        sys.error(s"Expected unique edge, found none for ${lhs}")
      }
  }

  def getEdgeOption(lhs: Lhs): Option[Rhs] = {
    table.get(lhs)
      .flatMap({rs =>
        if (rs.size > 1){
          sys.error(s"Expected 0 or 1 outgoing edge, found ${rs.size} edges for ${lhs}:${lhsCls}")
        } else {
          rs.headOption
        }
      })
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

class DBRelation[IDType: ClassTag, ModelType: ClassTag](
  table: mutable.HashMap[Int@@IDType, ModelType] = mutable.HashMap[Int@@IDType, ModelType]()
)(implicit O: Ordering[Int@@IDType]) {

  val modelClsStr = implicitly[ClassTag[ModelType]].runtimeClass.getSimpleName
  val idClsStr = implicitly[ClassTag[IDType]].runtimeClass.getSimpleName

  def all(): Seq[ModelType] = {
    val keys = table.keys.toSeq
    keys.sorted.map(table(_))
  }

  def debugPrint(): Unit = {
    println(s"Table: $modelClsStr")
    println(all().mkString("\n  ", "\n  ", "\n"))
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

  def delete(id: Int@@IDType): Unit = {
    assert(table.contains(id))
    table.remove(id)
    assert(!table.contains(id))
  }

  def zeroOrOne(ms: Seq[ModelType]): Option[ModelType] = {
    if (ms.length > 1) {
      sys.error(s"expected zeroOrOne ${modelClsStr}, got ${ms.length}")
    }
    ms.headOption
  }

  def batchCommit(): Unit = {

  }
}
