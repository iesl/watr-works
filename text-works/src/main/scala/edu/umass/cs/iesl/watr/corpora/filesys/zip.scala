package edu.umass.cs.iesl.watr
package corpora
package filesys

// import java.lang.AutoCloseable
// import java.io.{InputStream, PipedInputStream, PipedOutputStream}
// import java.util.zip.ZipEntry
// import java.util.zip.ZipOutputStream

import java.nio.file.{ DirectoryStream, Files, Path }
import fs2._
import cats.effect._
import cats.implicits._


object zip {
  // import scala.concurrent.ExecutionContext

  // def zip[F[_]](chunkSize: Int)(implicit F: Async[F], E: Effect[F], EC: ExecutionContext): Pipe[F, (String, Stream[F,Byte]), Byte] = {
  //   val zipped = F.delay {
  //     val pout = new PipedOutputStream()
  //     val pin = new PipedInputStream(pout, chunkSize)
  //     (pin: InputStream, new ZipOutputStream(pout))
  //   }

  //   def writeSink(zout: ZipOutputStream): Sink[F, (String, Stream[F,Byte])] = _.flatMap {
  //     case (name, data) =>
  //       val newEntry = Stream.eval(F.delay {
  //         val ze = new ZipEntry(name)
  //         zout.putNextEntry(ze)
  //       })
  //       val writeData = data.to(io.writeOutputStream(F.delay(zout), false))
  //       val closeEntry =  Stream.eval(F.delay {
  //         zout.closeEntry
  //       })
  //       newEntry ++ writeData ++ closeEntry
  //   }

  //   def close(t: AutoCloseable) = F.delay {
  //     t.close
  //   }


  //   in => Stream.eval(zipped).flatMap {
  //     case (pin, zout) =>
  //       val fill = in.to(writeSink(zout)).onFinalize(close(zout))
  //       val read = io.readInputStream(F.delay(pin), chunkSize, false).onFinalize(close(pin))
  //       fill.drain merge read
  //   }
  // }

  // def zip[F[_]](entries: Stream[F, (String, Stream[F, Byte])], chunkSize: Int)(implicit F: Effect[F], EC: ExecutionContext): Stream[F, Byte] = {
  //   entries.through(zip(chunkSize))
  // }

  def dirEntries[F[_]](dir: Path, include: Path => Boolean = _ => true)(implicit F: Effect[F]): fs2.Stream[F, Path] = {
    val useDirStream = (dirStream: DirectoryStream[Path]) => {
      Stream.unfold(dirStream.iterator) { iter =>
        if (iter.hasNext) Some((iter.next, iter)) else None
      }
    }

    val closeDirStream = (dirStream: DirectoryStream[Path]) => F.delay(dirStream.close)

    Stream.bracket(F.delay(Files.newDirectoryStream(dir)))(
      useDirStream(_),
      closeDirStream(_)
    ).filter(include)

  }


  def dirEntriesRecursive[F[_]](dir: Path, include: Path => Boolean = _ => true)(implicit F: Effect[F]): Stream[F, Path] =
    dirEntries[F](dir).flatMap { p =>
      val r = if (include(p)) Stream.emit(p) else Stream.empty
      if (Files.isDirectory(p)) r ++ dirEntriesRecursive(p, include)
      else r
    }

}
