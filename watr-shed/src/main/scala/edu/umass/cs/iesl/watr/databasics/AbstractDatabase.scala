package edu.umass.cs.iesl.watr
package databasics //;import acyclic.file

// import scala.concurrent._
// import scala.concurrent.duration._
// import ammonite.ops._

// import org.slf4j.LoggerFactory

// import com.zaxxer.hikari.HikariDataSource
// import slick.driver.H2Driver.api._

// import scala.concurrent._

// abstract class AbstractDatabase(dir: Path)  {
//   val log = LoggerFactory.getLogger(this.getClass)
//   import ExecutionContext.Implicits.global

//   lazy val (datasource, database) = {

//     // MVCC plus connection pooling speeds up the tests ~10%
//     val backend = "jdbc:h2:file:"
//     val url = backend + dir / "db;MVCC=TRUE"
//     val driver = "org.h2.Driver"

//     val ds = new HikariDataSource()
//     ds.setDriverClassName(driver)
//     ds.setJdbcUrl(url)
//     ds.setMaximumPoolSize(10)
//     val threads = ds.getMaximumPoolSize
//     val executor = AsyncExecutor("Slick", numThreads = threads, queueSize = -1)
//     (ds, Database.forDataSource(ds, executor = executor))
//   }

//   def commit(): Future[Unit] = Future.successful(())


//   def shutdown(): Future[Unit] = {
//     for {
//       // call directly - using slick withSession barfs as it runs a how many rows were updated
//       // after shutdown is executed.
//       _ <- database.run(sqlu"shutdown")
//       _ <- database.shutdown
//       _ = datasource.close()
//     } yield ()
//   }

//   def rmDBDir(): Unit = {
//     if (exists(dir)) {
//       rm(dir)
//     }
//   }


//   def getSchemas(): DBIOAction[Unit, NoStream, Effect.Schema] = DBIOX.noop

//   def createDBDir(): Unit = {
//     if (!exists(dir)) {
//       log.info("creating the search database...")
//       mkdir(dir)
//       Await.result(
//         database.run(getSchemas()),
//         Duration.Inf
//       )
//       log.info("... created the search database")
//     }
//   }



//   def dropAndRecreateDatabase(): Unit = {
//     rmDBDir()
//     createDBDir()
//   }

// }
