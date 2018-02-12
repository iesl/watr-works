package edu.umass.cs.iesl.watr
package workflow

/**
  *
  *
  */



trait CorpusPathNaming[F[_]] {
  // table zonepath (path ltree)

  def getTagged(): F[Unit]
  def createSubCorpus(): F[Unit]

}

trait TaggingAlg[F[_]] {
  //
  def getAll(): F[Unit]
  def subdivide(): F[Unit]

  def addTag(): F[Unit]
  def removeTag(): F[Unit]
  def renameTag(): F[Unit]
}



