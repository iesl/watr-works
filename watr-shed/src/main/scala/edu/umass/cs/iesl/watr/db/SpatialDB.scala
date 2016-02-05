package edu.umass.cs.iesl.watr
package db

trait SpatialDB {

  // def addPages(sha1: ID)
  // def addPage(pageBounds: B)
  // def addRect(page: P,... )
  // def addLabel(rect: R, page: P, )

}


object SpatialDB {
  def connect(): SpatialDB = {
    new SpatialDB {

    }
  }


}
