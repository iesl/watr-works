package edu.umass.cs.iesl.watr
package db

// object SpatialDB {
//   val createT0 : Update0 = sql"""
//     CREATE TABLE region2d (
//       id               IDENTITY,
//       spatialindex_id  BIGINT,
//       xmin DOUBLE, ymin DOUBLE,
//       xmax DOUBLE, ymax DOUBLE
//     );

//     CREATE TABLE spatialindex (
//       id   SERIAL
//     );

//     CREATE TABLE label (
//       id          SERIAL,
//       namespace   VARCHAR(32),
//       name        VARCHAR(32)
//     );
//   """.update

//   val dropT0: Update0 = sql"""
//     DROP TABLE IF EXISTS spatialindex;
//     DROP TABLE IF EXISTS region2d;
//   """.update

//   def create(): Unit = {

//   }

//   def drop(): Unit = {


//   }


//   def open(f: File): SpatialDB = {

//     new SpatialDB {
//       override val connectionString = s"jdbc:h2:file:${f.getPath}"

//     }



//     ???
//   }

// }


// import org.bytedeco._

// // import spatialindex._
// // import spatialindex.SpatialIndex

// trait SpatialDB {
//   def connectionString: String
//   def user = "h2"
//   def password = ""

//   lazy val xa = DriverManagerTransactor[Task](
//     "org.h2.Driver", connectionString, user, password
//   )


//   private lazy val spatialindexsdf: spatialindex.SpatialIndex = {

//     sql""" """
//     ???

//   }

//   // def insert(b: Bounds): Data = {
//   //   val mins = b.minMaxPairs.map(_._1)
//   //   val maxs = b.minMaxPairs.map(_._2)

//   //   checkResult(Index_InsertData(
//   //     indexptr,
//   //     id,
//   //     mins.toArray,
//   //     maxs.toArray,
//   //     nOfDimensions.toInt,
//   //     Array[Byte](),
//   //     0
//   //   ))

//   //   Data(id, b)
//   // }
// }





