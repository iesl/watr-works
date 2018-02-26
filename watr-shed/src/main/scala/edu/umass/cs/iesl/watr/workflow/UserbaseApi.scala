package edu.umass.cs.iesl.watr
package workflow


import corpora.{RelationModel => R}

/**
  * Curator userbase
  *
  */


trait UserbaseApi {
  def addUser(email: String@@EmailAddr): Int@@UserID
  def getUser(userId: Int@@UserID): Option[R.Person]
  def getUsers(): Seq[Int@@UserID]
  def getUserByEmail(email: String@@EmailAddr): Option[Int@@UserID]
  def deleteUser(userId: Int@@UserID): Unit
  def setPassword(userId:Int@@UserID, passhash: String@@PasswordHash): Unit
  def getPassword(userId:Int@@UserID): String@@PasswordHash

  def grantRole(userId: Int@@UserID, role: String@@Role): Unit
  def getRoles(userId: Int@@UserID): Seq[String@@Role]
  def revokeRole(userId: Int@@UserID, role: String@@Role): Unit

}

