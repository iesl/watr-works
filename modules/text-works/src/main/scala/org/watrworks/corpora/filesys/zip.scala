package org.watrworks
package corpora
package filesys

import java.nio.file.{ DirectoryStream, Files, Path }
import zio._
import zio.stream._

object zip {
  def dirEntries(dir: Path, include: Path => Boolean = _ => true): UStream[Path] = {
    def useDirStream(dirStream: DirectoryStream[Path]): UStream[Path] = {
      Stream.unfold(dirStream.iterator) { iter =>
        if (iter.hasNext()) Some((iter.next(), iter)) else None
      }
    }

    val closeDirStream = (dirStream: DirectoryStream[Path]) => ZIO.succeed(dirStream.close)
    val acquire = ZIO.succeed(Files.newDirectoryStream(dir))
    val release = closeDirStream(_)


    ZStream.acquireReleaseWith(acquire)(release)
      .flatMap(ds => useDirStream(ds))
      .filter(include)
  }


  def dirEntriesRecursive[F[_]](dir: Path, include: Path => Boolean = _ => true): UStream[Path] =
    dirEntries(dir).flatMap { p =>
      val r = if (include(p)) Stream(p) else Stream.empty
      if (Files.isDirectory(p)) r ++ dirEntriesRecursive(p, include)
      else r
    }

}
