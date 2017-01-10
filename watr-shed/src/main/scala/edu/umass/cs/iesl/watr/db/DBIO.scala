package edu.umass.cs.iesl.watr
package db

// import scala.collection.generic.CanBuildFrom
// import scala.concurrent._
// import scala.concurrent.duration._
// import slick.driver.H2Driver.api._


// object DBIOX {

//   def noop(
//     implicit ec: ExecutionContext
//   ): DBIOAction[Unit, NoStream, Effect] = successful(())

//   def point[R](r: => R)(
//     implicit ec: ExecutionContext
//   ): DBIOAction[R, NoStream, Effect] = from(Future{r})

//   /** Convert a `Future` to a [[DBIOAction]]. */
//   def from[R](f: => Future[R]): DBIOAction[R, NoStream, Effect] = DBIO.from(f)

//   /** Lift a constant value to a [[DBIOAction]]. */
//   def successful[R](v: R): DBIOAction[R, NoStream, Effect] = DBIO.successful(v)

//   /** Create a [[DBIOAction]] that always fails. */
//   def failed(t: Throwable): DBIOAction[Nothing, NoStream, Effect] = DBIO.failed(t)

//   /** Transform a `TraversableOnce[ DBIO[R] ]` into a `DBIO[ TraversableOnce[R] ]`. */
//   def sequence[R, M[+_] <: TraversableOnce[_], E <: Effect](
//     in: M[DBIOAction[R, NoStream, E]])(implicit
//       cbf: CanBuildFrom[M[DBIOAction[R, NoStream, E]], R, M[R]]
//   ): DBIOAction[M[R], NoStream, E] = DBIO.sequence(in)(cbf)

//   /** A simpler version of `sequence` that takes a number of DBIOActions with any return type as
//     * varargs and returns a DBIOAction that performs the individual actions in sequence (using
//     * `andThen`), returning `()` in the end. */
//   def seq[E <: Effect](actions: DBIOAction[_, NoStream, E]*): DBIOAction[Unit, NoStream, E] = DBIO.seq(actions:_*)

//   def seqs[E <: Effect](actions: Seq[DBIOAction[_, NoStream, E]]): DBIOAction[Unit, NoStream, E] = DBIO.seq(actions:_*)

//   /** Create a DBIOAction that runs some other actions in sequence and combines their results
//     * with the given function. */
//   def fold[T, E <: Effect](actions: Seq[DBIOAction[T, NoStream, E]], zero: T)(f: (T, T) => T)(implicit ec: ExecutionContext): DBIOAction[T, NoStream, E] =
//     DBIO.fold(actions, zero)(f)(ec)

//   /** Transform a `TraversableOnce[ DBIO[R] ]` into a `DBIO[ TraversableOnce[R] ]`. */
//   def sequenceAndAwait[R, M[+_] <: TraversableOnce[_], E <: Effect](rdb: Database)(
//     in: M[DBIOAction[R, NoStream, E]])(implicit
//       cbf: CanBuildFrom[M[DBIOAction[R, NoStream, E]], R, M[R]]
//   ):M[R] = {
//     Await.result(rdb.run(
//       sequence(in)(cbf)
//     ), Duration.Inf)
//   }

//   def runAndAwait[R, E <: Effect](rdb: Database, in: DBIOAction[R, NoStream, E]): R = {
//     Await.result(rdb.run(in), Duration.Inf)
//   }
// }
