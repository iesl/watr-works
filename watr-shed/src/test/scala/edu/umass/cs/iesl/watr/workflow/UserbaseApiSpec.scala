package edu.umass.cs.iesl.watr
package workflow

import TypeTags._
import corpora.database.DatabaseTest
import textgrid.TextGridBuilder



trait UserbaseTestHelpers {
  def workflowApi: WorkflowApi
  def userbaseApi: UserbaseApi

  def initUsers(n: Int): Seq[Int@@UserID] = {
    0 until n map { i =>
      userbaseApi.addUser(EmailAddr(s"user${i}@umass.edu"))
    }
  }


  def userMap(): Map[Int@@UserID, String@@EmailAddr] = {
    (for {
      userId <- userbaseApi.getUsers()
      user <- userbaseApi.getUser(userId)
    } yield  {
      (userId, user.email)
    }).toMap
  }
}

object Report {
  def withAssignments(assignments: (Int@@UserID, Int)*): WorkflowReport = {
    // WorkflowReport()

    ???
  }

}


class UserbaseApiSpec extends DatabaseTest with TextGridBuilder with UserbaseTestHelpers {
  behavior of "User support"


  it should "handle user creation" in new EmptyDatabase {
    for {
      (userId, i)   <- initUsers(10).zipWithIndex
      user0 <- userbaseApi.getUser(userId)
      userIdByEmail <- userbaseApi.getUserByEmail(EmailAddr(s"user${i}@umass.edu"))
      user1 <- userbaseApi.getUser(userIdByEmail)
    } {
      user0 shouldEqual(user1)
    }

    // println(userMap().mkString("{\n  ", "\n  ", "\n}"))

    userbaseApi.getUserByEmail(EmailAddr("noone@zz.com")) shouldBe None
  }


}
