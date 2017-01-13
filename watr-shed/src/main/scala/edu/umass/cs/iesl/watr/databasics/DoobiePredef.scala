package edu.umass.cs.iesl.watr
package databasics //;import acyclic.file

import doobie.imports._


trait DoobiePredef {

  def putStrLn(s: => String): ConnectionIO[Unit] =
    FC.delay(println(s))

  // TODO make the conversion to/from LTBounds -> database implicit via Meta[_]
  def dtoi(d: Double): Int = (d*100.0).toInt
  def itod(i: Int): Double = (i.toDouble)/100.0



  implicit val DocumentIDMeta: Meta[String @@ DocumentID] =
    Meta[String].nxmap(
      str => DocumentID(str),
      docId => docId.unwrap
    )

  implicit val PageIDMeta: Meta[Int @@ PageID] =
    Meta[Int].xmap(
      n => PageID(n),
      tt => tt.unwrap
    )

  implicit val ZoneIDMeta: Meta[Int @@ ZoneID] =
    Meta[Int].xmap(
      n => ZoneID(n),
      tt => tt.unwrap
    )

  // val jdkLogHandler: LogHandler = {
  //   // val jdkLogger = Logger.getLogger(getClass.getName)
  //   LogHandler {

  //     case Success(s, a, e1, e2) =>
  //       println(s"""Successful Statement Execution:
  //           |
  //           |  ${s.lines.dropWhile(_.trim.isEmpty).mkString("\n  ")}
  //           |
  //           | arguments = [${a.mkString(", ")}]
  //           |   elapsed = ${e1.toMillis} ms exec + ${e2.toMillis} ms processing (${(e1 + e2).toMillis} ms total)
  //         """.stripMargin)

  //     case ProcessingFailure(s, a, e1, e2, t) =>
  //       println(s"""Failed Resultset Processing:
  //           |
  //           |  ${s.lines.dropWhile(_.trim.isEmpty).mkString("\n  ")}
  //           |
  //           | arguments = [${a.mkString(", ")}]
  //           |   elapsed = ${e1.toMillis} ms exec + ${e2.toMillis} ms processing (failed) (${(e1 + e2).toMillis} ms total)
  //           |   failure = ${t.getMessage}
  //         """.stripMargin)

  //     case ExecFailure(s, a, e1, t) =>
  //       println(s"""Failed Statement Execution:
  //           |
  //           |  ${s.lines.dropWhile(_.trim.isEmpty).mkString("\n  ")}
  //           |
  //           | arguments = [${a.mkString(", ")}]
  //           |   elapsed = ${e1.toMillis} ms exec (failed)
  //           |   failure = ${t.getMessage}
  //         """.stripMargin)

  //   }
  // }


}
