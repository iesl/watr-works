package edu.umass.cs.iesl.watr

// import org.scalatest._


// import doobie.imports._
// import scalaz._, Scalaz._, scalaz.concurrent.Task

// class SpatialDBTest extends FlatSpec {

//   behavior of "mixed spatial index/relational db"

//   it should "connect to db" in {

//     val xa = DriverManagerTransactor[Task](
//       "org.h2.Driver", "jdbc:h2:world", "h2", ""
//     )

//     val program1 = sql"select 42".query[Int].unique

//     val task = program1.transact(xa)
//     val t = task.run
//   }
// }
