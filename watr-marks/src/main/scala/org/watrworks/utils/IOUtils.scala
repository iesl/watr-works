package org.watrworks
package utils

import os.SubPath

object PathUtils {

  import ammonite.{ops => fs}

  import java.nio.{file => nio}

  def appendTimestamp(path: String): String = {
    import java.text.SimpleDateFormat
    import java.util.Date
    val dateStamp = new SimpleDateFormat("yyyyMMddhhmmss").format(new Date())
    s"$path-$dateStamp"
  }

  def nioToAmm(nioPath: nio.Path): fs.Path = {
    fs.FilePath(nioPath) match {
      case p: fs.Path =>  p
      case p: fs.RelPath => fs.pwd / p
      case p: SubPath => fs.pwd / p
    }
  }

  def strToAmmPath(str: String): fs.Path = {
    fs.FilePath(str) match {
      case p: fs.Path =>  p
      case p: fs.RelPath => fs.pwd / p
      case p: SubPath => fs.pwd / p
    }
  }

  implicit class RicherPathUtils_String(val self: String) extends AnyVal {

    def toPath(): fs.Path = {
      strToAmmPath(self)
    }
  }

  implicit class RicherPathUtils_NioPath(val self: nio.Path) extends AnyVal {

    def toFsPath(): fs.Path = {
      nioToAmm(self)
    }
  }

}
