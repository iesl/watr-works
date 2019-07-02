package edu.umass.cs.iesl.watr
package corpora
package filesys

import java.nio.file.{ DirectoryStream, Files, Path }
import fs2._
import cats.effect._
import cats.implicits._


object zip {

  def dirEntries[F[_]](dir: Path, include: Path => Boolean = _ => true)(implicit F: Effect[F]): fs2.Stream[F, Path] = {
    def useDirStream(dirStream: DirectoryStream[Path]): fs2.Stream[F, Path] = {
      Stream.unfold(dirStream.iterator) { iter =>
        if (iter.hasNext()) Some((iter.next(), iter)) else None
      }
    }

    val closeDirStream = (dirStream: DirectoryStream[Path]) => F.delay(dirStream.close)
    val acquire = F.delay(Files.newDirectoryStream(dir))
    val release = closeDirStream(_)

    Stream.bracket(acquire)(release)
      .flatMap(ds => useDirStream(ds))
      .filter(include)
  }


  def dirEntriesRecursive[F[_]](dir: Path, include: Path => Boolean = _ => true)(implicit F: Effect[F]): Stream[F, Path] =
    dirEntries[F](dir).flatMap { p =>
      val r = if (include(p)) Stream.emit(p) else Stream.empty
      if (Files.isDirectory(p)) r ++ dirEntriesRecursive(p, include)
      else r
    }

}
